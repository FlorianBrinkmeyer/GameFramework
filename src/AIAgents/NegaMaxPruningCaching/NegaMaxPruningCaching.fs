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

type Node = {State : ImmutableGame;
             Value : float;
             Depth : float;
             Children : Map<int, Node>;
             DevelopedChildren : int;
             Alpha : float;
             Beta : float
             Initialized : bool}

type GameStateAttr = {Node : Node; Depth : float}

let asyncMap mapper = 
    Seq.map (fun item -> async {return mapper item}) >> Async.Parallel >> Async.RunSynchronously

let allowDebugMode = true
let debugDepth = 2    

type NegaMaxTimeLimitedPruningCaching (player : int, searchTime : int, maxDepth : int, increaseDepthForInstableState, debugMode, ?usedThreads) =
    let cachedStates, threadsCount = 
        match usedThreads with
        | Some count  when count > 1 ->
            Concurrent.ConcurrentDictionary<ImmutableGame, GameStateAttr> () :> Generic.IDictionary<ImmutableGame, GameStateAttr>, count
        | _ ->    
            Generic.Dictionary<ImmutableGame, GameStateAttr> (), 1
    let rnd = Random ()
    let sendMessage = Event<String> ()
    let timer = new Timers.Timer (searchTime)
    let mutable registeredMoveMadeEvent = false
    let mutable multiThreadingInUse = false
    let mutable maybePermanentTree = None
    let mutable usedCacheCount = 0
    let mutable reachedMaxDepth = 0.0
    let mutable considerationTime = searchTime
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
            with get () = considerationTime
            and set (value) = 
                considerationTime <- value
                timer.Interval <- considerationTime
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
                 Alpha = alpha; 
                 Beta = beta;
                 Value = state.ZSValue;
                 Children = Map.empty;
                 DevelopedChildren = 0
                 Initialized = true}            
            let rec resetNode (node : Node) =
                let updatedChilden = 
                    Seq.zip node.Children.Keys node.Children.Values |> Seq.map (fun (index, child) -> index, resetNode child)
                let updatedMap = updatedChilden |> Seq.fold (fun (map : Map<int,Node>) (index, child) -> map.Add (index, child)) Map.empty
                {node with Children = updatedMap; Initialized = false}
            let rec main depth increaseDepth (node : Node) =
                let maxInParallel =
                    if multiThreadingInUse then
                        1
                    else
                        threadsCount    
                let initializedChildren = Seq.zip node.Children.Keys node.Children.Values |> Seq.map (fun (index, child) ->
                    if child.Initialized then
                        index, child
                    else
                        let depth =
                            if child.State.NumberOfPossibleMoves = 0 then
                                Double.PositiveInfinity
                            elif child.State.InstableState && increaseDepth then
                                Double.NegativeInfinity
                            elif child.DevelopedChildren = 0 then
                                0.0
                            else    
                                Double.NegativeInfinity       
                        index, {child with Alpha = -node.Beta; Beta = -node.Alpha; Depth = depth; Initialized = true}    
                )
                let incompleteChildren = initializedChildren |> Seq.filter (fun (_, child) -> child.Depth < depth - 1.0) 
                let orderedChildren = incompleteChildren |> Seq.sortBy (fun (_, child) -> child.Value) |> Seq.toArray
                let childrenToDevelop, newChildrenCount =
                    if orderedChildren.Length >= maxInParallel then
                        orderedChildren |> Array.take maxInParallel, node.DevelopedChildren
                    else
                        let additionalChildrenCount = 
                            Math.Min (maxInParallel - orderedChildren.Length, node.State.NumberOfPossibleMoves - node.DevelopedChildren)
                        let additionalChildren = 
                            [|node.DevelopedChildren..(node.DevelopedChildren+additionalChildrenCount-1)|] 
                                |> Array.map (fun index -> index, node.State.NthMove index |> getNewNode increaseDepth -node.Beta -node.Alpha)
                        Array.append orderedChildren additionalChildren, node.DevelopedChildren + additionalChildrenCount
                let newChildrenPairs = 
                    if childrenToDevelop.Length <= 1 || (debugMode && allowDebugMode) then
                        childrenToDevelop |> Array.map (fun (index, child) -> index, (getNextTree (depth - 1.0) increaseDepth child))
                    else
                        multiThreadingInUse <- true
                        let res = childrenToDevelop |> asyncMap (fun (index, child) -> index, (getNextTree (depth - 1.0) increaseDepth child))
                        multiThreadingInUse <- false
                        res
                let newChildren = 
                    newChildrenPairs |> Seq.fold (fun (acc : Map<int,Node>) (index, child) -> acc.Add (index, child)) node.Children
                let nodeWithUpdatedChildren = {node with Children = newChildren; DevelopedChildren = newChildrenCount}
                let newAlpha = 
                    let childrenValues = 
                        newChildrenPairs |> Seq.map snd |> Seq.filter (fun child -> child.Depth >= depth - 1.0) |> Seq.map (fun child -> -child.Value)
                    if childrenValues |> Seq.isEmpty then
                        node.Alpha
                    else
                        let childrenAlpha = childrenValues |> Seq.max    
                        Math.Max (node.Alpha, childrenAlpha)
                let maybeCacheState depth (node : Node) =
                    match cachedStates.TryGetValue node.State with
                    | true, attr when attr.Depth > depth ->
                        ()
                    | _ -> 
                        cachedStates[node.State] <- {Node = node; Depth = depth}   
                if newChildrenCount = node.State.NumberOfPossibleMoves 
                  && newChildren.Values |> Seq.forall (fun child -> child.Depth >= depth - 1.0) then
                    let childrenMinDepth = newChildren.Values |> Seq.map (fun child -> child.Depth) |> Seq.min
                    let newDepth = childrenMinDepth + 1.0
                    let newValue = newChildren.Values |> Seq.map (fun child -> -child.Value) |> Seq.max    
                    let res = {nodeWithUpdatedChildren with Value = newValue; Depth = newDepth}
                    maybeCacheState newDepth res           
                    res
                elif newAlpha >= node.Beta then
                    let res = {nodeWithUpdatedChildren with Depth = depth; Value = newAlpha}         
                    maybeCacheState depth res
                    res
                else 
                    {nodeWithUpdatedChildren with Alpha = newAlpha}                       
            and getNextTree depth increaseDepth (node : Node) =
                if depth <= node.Depth then
                    node
                elif node.DevelopedChildren = 0 then
                    match cachedStates.TryGetValue node.State with
                    | true, attr ->
                        usedCacheCount <- usedCacheCount + 1
                        attr.Node |> resetNode |> main depth increaseDepth
                    | _ ->
                        main depth increaseDepth node
                else
                    main depth increaseDepth node      
            let timeLimitedSearch =
                async {
                    let mutable tree = 
                        match maybePermanentTree with
                        | Some permanentTree ->
                            resetNode permanentTree
                        | None ->    
                            let state = game :?> ImmutableGame
                            getNewNode false Double.NegativeInfinity Double.PositiveInfinity state
                    let mutable searchDepth = 1.0
                    while (searchDepth <= maxDepth && timeLeft && not debugMode) || (allowDebugMode && searchDepth <= debugDepth) do
                        let increaseDepth = increaseDepthForInstableState && searchDepth > 2
                        tree <- getNextTree searchDepth increaseDepth tree
                        if tree.Depth >= searchDepth then
                            let message = 
                                String.Format ("Reached search depth: {0:0}, Cached states: {1}, Used cache {2} times, Maximally reached depth: {3:0}",
                                    searchDepth, cachedStates.Count, usedCacheCount, reachedMaxDepth)
                            sendMessage.Trigger message
                            searchDepth <- tree.Depth + 1.0
                            let reinitializedTree = {tree with Alpha = Double.NegativeInfinity; Beta = Double.PositiveInfinity; Depth = 0.0}
                            let nextTree = resetNode reinitializedTree
                            tree <- nextTree 
                            if searchDepth > reachedMaxDepth then
                                reachedMaxDepth <- searchDepth
                    let maxValue = tree.Children.Values |> Seq.map (fun child -> -child.Value) |> Seq.max
                    let maxValueMoves = 
                        Seq.zip tree.Children.Keys tree.Children.Values |> Seq.filter (fun (_, child) -> -child.Value = maxValue)
                    let maxDepth = maxValueMoves |> Seq.map (fun (_, child) -> child.Depth) |> Seq.max 
                    let maxValueMaxDepthMoves = maxValueMoves |> Seq.filter (fun (_, child) -> child.Depth = maxDepth) |> Seq.toArray
                    let chosenMove = 
                        if debugMode && allowDebugMode then
                            maxValueMaxDepthMoves[0] |> fst
                        else    
                            maxValueMaxDepthMoves[rnd.Next maxValueMaxDepthMoves.Length] |> fst
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
            timer.Start ()
            timeLimitedSearch |> Async.Start