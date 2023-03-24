namespace GameFramework

open System
open System.Collections

type Node = {State : ImmutableGame;
             Value : float;
             Depth : float;
             Alpha : float;
             Beta : float;
             Children : Option<Node []>}

type Bound = LowerBound | UpperBound | Exact

type GameStateAttr = {Value : float; Depth : float; Bound : Bound}

type NegaMaxTimeLimitedPruningCaching (playerID : int, searchTime : int, maxDepth : int) =
    let cachedStates = Generic.Dictionary<ImmutableGame, GameStateAttr> ()
    let mutable usedCacheCount = 0
    let mutable reachedMaxDepth = 0.0
    let mutable player = playerID
    let mutable considerationTime = searchTime
    let sendMessage = Event<String> ()
    interface AI_WithConsiderationTime with
        member x.Player
            with get () = player
            and set (value) = player <- value
        member x.ConsiderationTime 
            with get () = considerationTime
            and set (value) = considerationTime <- value
    interface AI_Informer with
        [<CLIEvent>]
        member x.SendMessage = sendMessage.Publish 
    interface AI_Agent with
        member x.Player = player
        member x.MakeMove mutableGame game =
            let getNewNode (state : ImmutableGame) =
                let depth =
                    if state.NumberOfPossibleMoves = 0 then
                        Double.PositiveInfinity
                    else
                        0    
                {State = state;
                 Value = state.ZSValue;
                 Depth = depth;
                 Alpha = Double.NegativeInfinity;
                 Beta = Double.PositiveInfinity
                 Children = None}
            let mutable timeLeft = true
            let timeLimitedSearch =
                async {
                    let mutable searchDepth = 1.0
                    let state = game :?> ImmutableGame
                    let children = Array.init state.NumberOfPossibleMoves (fun index -> state.NthMove index |> getNewNode)
                    let childMinDepth = children |> Seq.map (fun child -> child.Depth) |> Seq.min
                    let value = children |> Seq.map (fun child -> -child.Value) |> Seq.max
                    let startNode = 
                        {State = state;
                         Value = value
                         Depth = childMinDepth + 1.0
                         Alpha = Double.NegativeInfinity;
                         Beta = Double.PositiveInfinity
                         Children = Some children}                                                           
                    let mutable tree = startNode
                    while searchDepth <= maxDepth && timeLeft do
                        let rec helper (node : Node) depth =
                            let main (node : Node) alpha beta =
                                match node.Children with
                                | Some children ->
                                    let (index, chosenChild) =
                                        let childrenToComplete = 
                                            children |> Array.mapi (fun index child  -> index, child) |> Seq.filter (fun (_, child) -> child.Depth < depth - 1.0) 
                                        childrenToComplete |> Seq.minBy (fun (_, child) -> child.Value)
                                    let childWithAlphaBeta = {chosenChild with Alpha = -beta; Beta = -alpha}
                                    let newChild = helper childWithAlphaBeta (depth - 1.0)
                                    children[index] <- newChild
                                    let newAlpha = Math.Max (alpha, newChild.Value)
                                    if ((newChild.Depth >= depth - 1.0) && (newAlpha >= beta)) || (children |> Seq.forall (fun child -> child.Depth >= depth - 1.0)) then
                                        let relevantChildren = children |> Seq.filter (fun child -> child.Depth >= depth - 1.0)
                                        let relChildrenMinDepth = relevantChildren |> Seq.map (fun child -> child.Depth) |> Seq.min
                                        let newDepth = relChildrenMinDepth + 1.0
                                        let newValue = relevantChildren |> Seq.map (fun child -> -child.Value) |> Seq.max    
                                        let flag =
                                            if newValue <= node.Alpha then
                                                UpperBound
                                            elif newValue >= node.Beta then
                                                LowerBound
                                            else
                                                Exact
                                        cachedStates[node.State] <- {Value = newValue; Depth = newDepth; Bound = flag}               
                                        {node with Alpha = newAlpha; Depth = newDepth; Value = newValue}
                                    else
                                        {node with Alpha = newAlpha}
                                | None ->
                                    let children = Array.init node.State.NumberOfPossibleMoves (fun index -> node.State.NthMove index |> getNewNode)
                                    let childMinDepth = children |> Seq.map (fun child -> child.Depth) |> Seq.min
                                    let value = children |> Seq.map (fun child -> -child.Value) |> Seq.max
                                    {node with Children = Some children; Depth = childMinDepth + 1.0; Value = value}                                                           
                            if node.Depth >= depth then
                                node
                            else
                                match cachedStates.TryGetValue node.State with
                                | true, attr when attr.Depth >= depth ->
                                    usedCacheCount <- usedCacheCount + 1
                                    match attr.Bound with
                                    | UpperBound ->
                                        let beta = Math.Min (node.Beta, attr.Value)
                                        main node node.Alpha beta
                                    | LowerBound ->
                                        let alpha = Math.Max (node.Alpha, attr.Value)
                                        main node alpha node.Beta
                                    | Exact ->
                                        {node with Value = attr.Value; Depth = attr.Depth}
                                | _ ->
                                    main node node.Alpha node.Beta        
                        tree <- helper tree searchDepth
                        if tree.Depth >= searchDepth then
                            sendMessage.Trigger (sprintf "Reached search depth: %A, Cached states: %A, Used cache %A times, Maximally reached depth: %A" 
                                ((int) searchDepth) cachedStates.Count usedCacheCount ((int)reachedMaxDepth))                           
                            searchDepth <- tree.Depth + 1.0
                            if searchDepth > reachedMaxDepth then
                                reachedMaxDepth <- searchDepth   
                    let chosenMove = 
                        [0..(game.NumberOfPossibleMoves-1)] |> List.maxBy (fun index -> -tree.Children.Value[index].Value)
                    mutableGame.MakeMove chosenMove
                }
            let timer = new Timers.Timer (considerationTime)
            timer.AutoReset <- false
            timer.Elapsed.AddHandler (fun _ _ -> timeLeft <- false)
            timer.Start ()
            timeLimitedSearch |> Async.Start