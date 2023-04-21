(*
Copyright (C) 2023  Florian Brinkmeyer

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

module GameFramework.Negamax

open GameFramework
open System
open System.Collections

type Node = {State : ImmutableGame
             Value : float
             Depth : float
             Children : Map<int, Node>
             DevelopedChildren : int
             RealDepth : float //depth without pruning
             RealValue: float //value without pruning
             Alpha : float
             Beta : float
             Initialized : bool}

type GameStateAttr = {Node : Node; Depth : float; mutable Count : int}

let asyncMap mapper = 
    Seq.map (fun item -> async {return mapper item}) >> Async.Parallel >> Async.RunSynchronously

let allowDebugMode = true
let debugDepth = 3

type QueueWrapper (concurrent) =
    let queue = Generic.Queue<ImmutableGame> ()
    let concurrentQueue = Concurrent.ConcurrentQueue<ImmutableGame> ()
    member x.Enqueue item =
        if concurrent then
            concurrentQueue.Enqueue item
        else
            queue.Enqueue item
    member x.TryDequeue =
        if concurrent then
            concurrentQueue.TryDequeue ()
        else
            queue.TryDequeue ()
    member x.Count =
        if concurrent then
            concurrentQueue.Count
        else
            queue.Count    

type NegaMaxTimeLimitedPruningCaching (player : int, searchTime : int, maxDepth : int, increaseDepthForInstableState, debugMode, maybeMaxCacheCount, ?usedThreads) =
    let cachedStates, deleteQueue, threadsCount = 
        match usedThreads with
        | Some count  when count > 1 ->
            Concurrent.ConcurrentDictionary<ImmutableGame, GameStateAttr> () :> Generic.IDictionary<ImmutableGame, GameStateAttr>, QueueWrapper true, count
        | _ ->    
            Generic.Dictionary<ImmutableGame, GameStateAttr> (), QueueWrapper false, 1
    let rnd = Random ()
    let sendMessage = Event<String> ()
    let timer = new Timers.Timer (searchTime)
    let mutable registeredMoveMadeEvent = false
    let mutable multiThreadingInUse = false
    let mutable maybePermanentTree = None
    let mutable usedCacheCount = 0
    let mutable reachedMaxDepth = 0.0
    let mutable timeLeft = false
    do
        timer.Elapsed.AddHandler (fun _ _ -> timeLeft <- false)
        timer.AutoReset <- false
    interface StopableAI with
        member x.Stop () =
            timeLeft <- false
            timer.Stop ()
    interface AI_WithConsiderationTime with
        member x.Player = player
        member x.ConsiderationTime 
            with get () = (int) timer.Interval
            and set (value) = timer.Interval <- value
    interface AI_WithMultiThreading with
        member x.UsedThreads = threadsCount
    interface AI_Informer with
        [<CLIEvent>]
        member x.SendMessage = sendMessage.Publish 
    interface AI_Agent with
        member x.Player = player
        member x.MakeMove mutableGame game =
            let getNewNode increaseDepth alpha beta (state : ImmutableGame) =
                let depth =
                    if state.NumberOfPossibleMoves = 0 then
                        Double.PositiveInfinity
                    elif state.InstableState && increaseDepth then
                        Double.NegativeInfinity
                    else
                        0.0
                {State = state; 
                 Depth = depth; 
                 RealDepth = depth;
                 Alpha = alpha; 
                 Beta = beta;
                 Value = state.ZSValue
                 RealValue = state.ZSValue
                 Children = Map.empty;
                 DevelopedChildren = 0
                 Initialized = true}            
            let rec resetNode (node : Node) =
                let updatedChilden = 
                    Seq.zip node.Children.Keys node.Children.Values |> Seq.map (fun (index, child) -> index, resetNode child)
                let updatedMap = updatedChilden |> Seq.fold (fun (map : Map<int,Node>) (index, child) -> map.Add (index, child)) Map.empty
                {node with Children = updatedMap; Initialized = false; Depth = node.RealDepth; Value = node.RealValue}
            let rec main depth increaseDepth (node : Node) =
                let maxInParallel =
                    if multiThreadingInUse then
                        1
                    else
                        threadsCount    
                let initializedChildren = Seq.zip node.Children.Keys node.Children.Values |> Seq.map (fun (index, child) ->
                    if child.Initialized then
                        index, {child with Beta = -node.Alpha}
                    else
                        index, {child with Alpha = -node.Beta; Beta = -node.Alpha; Initialized = true}    
                )
                let incompleteChildren = initializedChildren |> Seq.filter (fun (_, child) -> child.Depth < depth - 1.0) 
                let orderedChildren = incompleteChildren |> Seq.sortBy (fun (_, child) -> child.Value) |> Seq.toArray
                let childrenToDevelop, newDevChildrenCount =
                    if orderedChildren.Length >= maxInParallel then
                        orderedChildren |> Array.take maxInParallel, node.DevelopedChildren
                    else
                        let additionalChildrenCount = 
                            Math.Min (maxInParallel - orderedChildren.Length, node.State.NumberOfPossibleMoves - node.DevelopedChildren)
                        let additionalChildren = 
                            [|node.DevelopedChildren..(node.DevelopedChildren+additionalChildrenCount-1)|] 
                                |> Array.map (fun index -> index, node.State.NthMove index |> getNewNode increaseDepth -node.Beta -node.Alpha)
                        Array.append orderedChildren additionalChildren, node.DevelopedChildren + additionalChildrenCount
                let newIndChildrenPairs = 
                    if childrenToDevelop.Length <= 1 then //|| (debugMode && allowDebugMode) then
                        childrenToDevelop |> Array.map (fun (index, child) -> index, (getNextTree (depth - 1.0) increaseDepth child))
                    else
                        multiThreadingInUse <- true
                        let res = childrenToDevelop |> asyncMap (fun (index, child) -> index, (getNextTree (depth - 1.0) increaseDepth child))
                        multiThreadingInUse <- false
                        res
                let newChildren = 
                    newIndChildrenPairs |> Seq.fold (fun (acc : Map<int,Node>) (index, child) -> acc.Add (index, child)) node.Children
                let nodeWithUpdatedChildren = {node with Children = newChildren; DevelopedChildren = newDevChildrenCount}
                let newAlpha = 
                    let childrenValues = 
                        newIndChildrenPairs |> Seq.map snd |> Seq.filter (fun child -> child.Depth >= depth - 1.0) |> Seq.map (fun child -> -child.Value)
                    if childrenValues |> Seq.isEmpty then
                        node.Alpha
                    else
                        let childrenAlpha = childrenValues |> Seq.max    
                        Math.Max (node.Alpha, childrenAlpha)
                let maybeCacheState depth (node : Node) =
                    match cachedStates.TryGetValue node.State with
                    | true, attr when node.RealDepth < attr.Node.RealDepth || (node.RealDepth = attr.Node.RealDepth && depth <= attr.Depth) ->
                        ()
                    | _ -> 
                        let count =
                            match cachedStates.TryGetValue node.State with
                            | true, attr ->
                                attr.Count
                            | _ ->
                                0
                        cachedStates[node.State] <- {Node = node |> resetNode; Depth = depth; Count = count + 1}
                        deleteQueue.Enqueue node.State
                        match maybeMaxCacheCount with
                        | Some maxCount ->
                            while cachedStates.Count > maxCount && deleteQueue.Count > 0 do
                                match deleteQueue.TryDequeue with
                                | true, value ->
                                    match cachedStates.TryGetValue value with
                                    | true, attr ->
                                        attr.Count <- attr.Count - 1
                                        if attr.Count <= 0 then
                                            cachedStates.Remove value |> ignore
                                    | _ -> ()    
                                | _ -> ()    
                        | None -> ()                           
                if newDevChildrenCount = node.State.NumberOfPossibleMoves 
                  && newChildren.Values |> Seq.forall (fun child -> child.Depth >= depth - 1.0) then
                    let newChildrenMinDepth = newChildren.Values |> Seq.map (fun child -> child.Depth) |> Seq.min
                    let newDepth = newChildrenMinDepth + 1.0
                    let newValue = newChildren.Values |> Seq.map (fun child -> -child.Value) |> Seq.max
                    let newChildrenRealMinDepth = newChildren.Values |> Seq.map (fun child -> child.RealDepth) |> Seq.min
                    let newRealDepth = newChildrenRealMinDepth + 1.0
                    let newRealValue = newChildren.Values |> Seq.map (fun child -> -child.RealValue) |> Seq.max                    
                    let res = {nodeWithUpdatedChildren with Value = newValue; Depth = newDepth; RealDepth = newRealDepth; RealValue = newRealValue}
                    maybeCacheState newDepth res           
                    res
                elif newAlpha > node.Beta then
                    let res = {nodeWithUpdatedChildren with Depth = depth; Value = newAlpha}         
                    maybeCacheState depth res
                    res
                else 
                    {nodeWithUpdatedChildren with Alpha = newAlpha}                       
            and getNextTree depth increaseDepth (node : Node) =
                if depth <= node.Depth then
                    node
                else
                    match cachedStates.TryGetValue node.State with
                    | true, attr when attr.Node.RealDepth > node.RealDepth || attr.Node.DevelopedChildren > node.DevelopedChildren ->
                        usedCacheCount <- usedCacheCount + 1
                        let rec meltNodes (left : Node) (right : Node) =
                            let updatedChildren =
                                let getUpdatedChildren bigger smaller =
                                    let jointUpdatedChildren =
                                        let keys = Seq.take smaller.DevelopedChildren smaller.Children.Keys
                                        let leftValues = Seq.take smaller.DevelopedChildren bigger.Children.Values
                                        let rightValues = Seq.take smaller.DevelopedChildren smaller.Children.Values
                                        Seq.zip3 keys leftValues rightValues |> Seq.map (fun (index, l, r) -> index, (meltNodes l r)) 
                                    jointUpdatedChildren |> Seq.fold (fun (state : Map<int,Node>) (index, child) -> state.Add (index, child)) bigger.Children
                                if left.DevelopedChildren >= right.DevelopedChildren then
                                    getUpdatedChildren left right
                                else
                                    getUpdatedChildren right left
                            let developedChildren = Math.Max (left.DevelopedChildren, right.DevelopedChildren)
                            let realDepth = 
                                if developedChildren < left.State.NumberOfPossibleMoves || updatedChildren.Values |> Seq.isEmpty then
                                    left.RealDepth
                                else    
                                    (updatedChildren.Values |> Seq.map (fun child -> child.RealDepth) |> Seq.min) + 1.0
                            let realValue = 
                                if developedChildren < left.State.NumberOfPossibleMoves || updatedChildren.Values |> Seq.isEmpty then
                                    left.RealValue
                                else    
                                    updatedChildren.Values |> Seq.map (fun child -> -child.RealValue) |> Seq.max
                            let initializedNode =
                                if left.Initialized then
                                    Some left
                                elif right.Initialized then
                                    Some right
                                else
                                    None         
                            let depth = 
                                match initializedNode with
                                | Some node ->
                                    if realDepth > node.Depth then
                                        realDepth
                                    else
                                        node.Depth    
                                | None ->
                                    realDepth    
                            let value = 
                                match initializedNode with
                                | Some node ->
                                    if realDepth > node.Depth then
                                        realValue
                                    else
                                        node.Value    
                                | None ->
                                    realValue   
                            let initialized = initializedNode |> Option.isSome
                            let alpha, beta =
                                match initializedNode with
                                | Some node ->
                                    node.Alpha, node.Beta
                                | None ->
                                    Double.NegativeInfinity, Double.PositiveInfinity
                            {State = left.State
                             Alpha = alpha
                             Beta = beta
                             Children = updatedChildren
                             Depth = depth
                             Initialized = initialized
                             Value = value
                             DevelopedChildren = developedChildren
                             RealDepth = realDepth
                             RealValue = realValue}
                        meltNodes node attr.Node |> main depth increaseDepth   
                    | _ ->
                        main depth increaseDepth node      
            let timeLimitedSearch =
                async {
                    let mutable tree, completeTree = 
                        match maybePermanentTree with
                        | Some permanentTree ->
                            let completeTr = {permanentTree with Alpha = Double.NegativeInfinity; Beta = Double.PositiveInfinity}
                            let tr = resetNode completeTr
                            tr, completeTr
                        | None ->    
                            let state = game :?> ImmutableGame
                            let tr = getNewNode false Double.NegativeInfinity Double.PositiveInfinity state
                            tr, tr
                    let mutable searchDepth = Math.Max (tree.Depth, 1.0)
                    let message = 
                        String.Format ("Reached search depth: {0:0}, Cached states: {1}, Used cache {2} times, Maximally reached depth: {3:0}",
                            (searchDepth - 1.0), cachedStates.Count, usedCacheCount, reachedMaxDepth)
                    sendMessage.Trigger message
                    while timeLeft && ((searchDepth <= maxDepth && not debugMode) || (allowDebugMode && searchDepth <= debugDepth)) do
                        let increaseDepth = increaseDepthForInstableState && searchDepth > 2
                        tree <- getNextTree searchDepth increaseDepth tree
                        if tree.Depth >= searchDepth then
                            completeTree <- tree
                            if searchDepth > reachedMaxDepth then
                                reachedMaxDepth <- searchDepth
                            let message = 
                                String.Format ("Reached search depth: {0:0}, Cached states: {1}, Used cache {2} times, Maximally reached depth: {3:0}",
                                    searchDepth, cachedStates.Count, usedCacheCount, reachedMaxDepth)
                            sendMessage.Trigger message
                            searchDepth <- tree.Depth + 1.0
                            let reinitializedTree = resetNode {tree with Alpha = Double.NegativeInfinity; Beta = Double.PositiveInfinity}
                            tree <- reinitializedTree 
                    let maxValueComplTree = completeTree.Children.Values |> Seq.map (fun child -> -child.Value) |> Seq.max
                    let maxValueMovesComplTree = 
                        Seq.zip completeTree.Children.Keys completeTree.Children.Values |> Seq.filter (fun (_, child) -> -child.Value = maxValueComplTree) 
                    let maxValueInidizes = maxValueMovesComplTree |> Seq.toList |> List.map fst
                    let correspTreeMoves = maxValueInidizes |> List.map (fun index -> index, tree.Children[index])
                    let maxValueTree = correspTreeMoves |> List.map (fun (_, child) -> -child.Value) |> List.max
                    let maxValueMovesTree = correspTreeMoves |> List.filter (fun (_, child) -> -child.Value = maxValueTree)
                    let maxDepthTree = maxValueMovesTree |> List.map (fun (_, child) -> child.Depth) |> List.max
                    let maxValueMaxDepthTreeMoves = maxValueMovesTree |> List.filter (fun (_, child) -> child.Depth = maxDepthTree)
                    let chosenMove = 
                        if debugMode && allowDebugMode then
                            maxValueMaxDepthTreeMoves[0] |> fst
                        else    
                            maxValueMaxDepthTreeMoves[rnd.Next maxValueMaxDepthTreeMoves.Length] |> fst
                    maybePermanentTree <- Some tree
                    mutableGame.MakeMove chosenMove
                }        
            if not registeredMoveMadeEvent then
                mutableGame.add_MoveMadeEvent (fun move -> 
                    match maybePermanentTree with
                    | Some permanentTree -> 
                        if permanentTree.DevelopedChildren > move then
                            maybePermanentTree <- permanentTree.Children[move] |> Some
                        else
                            maybePermanentTree <- None     
                    | None ->
                        maybePermanentTree <- None
                )   
                registeredMoveMadeEvent <- true     
            timeLeft <- true
            if not debugMode || not allowDebugMode then
                timer.Start ()               
            timeLimitedSearch |> Async.Start