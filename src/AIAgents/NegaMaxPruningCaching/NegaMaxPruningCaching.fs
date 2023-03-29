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

module GameFramework

open GameFramework
open System
open System.Collections

type Node = {State : ImmutableGame;
             Value : float;
             Depth : float;
             DepthAim : Option<float>
             Alpha : float;
             Beta : float;
             MaybeChildren : Option<Node []>}

type Bound = LowerBound | UpperBound | Exact

type GameStateAttr = {Value : float; Depth : float; Bound : Bound; State : ImmutableGame}

let asyncMap mapper items = 
    items |> Seq.map (fun item -> async {return mapper item}) |> Async.Parallel |> Async.RunSynchronously

type NegaMaxTimeLimitedPruningCaching (player : int, searchTime : int, maxDepth : int, ?usedThreads) =
    let cachedStates, threadsCount = 
        match usedThreads with
        | Some count  when count > 1 ->
            Concurrent.ConcurrentDictionary<ImmutableGame, GameStateAttr> () :> Generic.IDictionary<ImmutableGame, GameStateAttr>, count
        | _ ->    
            Generic.Dictionary<ImmutableGame, GameStateAttr> (), 1
    let rnd = Random ()
    let sendMessage = Event<String> ()
    let mutable registeredMoveMadeEvent = false
    let mutable multiThreadingInUse = false
    let mutable maybePermanentTree = None
    let mutable usedCacheCount = 0
    let mutable reachedMaxDepth = 0.0
    let mutable considerationTime = searchTime
    interface IReInitializableAI with
        member x.ReInitialize () =
            maybePermanentTree <- None
            registeredMoveMadeEvent <- false
    interface AI_WithConsiderationTime with
        member x.Player = player
        member x.ConsiderationTime 
            with get () = considerationTime
            and set (value) = considerationTime <- value
    interface AI_WithMultiThreading with
        member x.UsedThreads = threadsCount
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
                 MaybeChildren = None
                 DepthAim = None}
            let mutable timeLeft = true
            let timeLimitedSearch =
                async {
                    let mutable tree = 
                        match maybePermanentTree with
                        | Some permanentTree ->
                            permanentTree
                        | None ->    
                            let state = game :?> ImmutableGame
                            let children = Array.init state.NumberOfPossibleMoves (fun index -> state.NthMove index |> getNewNode)
                            let childMinDepth = children |> Seq.map (fun child -> child.Depth) |> Seq.min
                            let value = children |> Seq.map (fun child -> -child.Value) |> Seq.max
                            let mutable multiThreadingInUse = false
                            {State = state;
                             Value = value
                             Depth = childMinDepth + 1.0
                             Alpha = Double.NegativeInfinity;
                             Beta = Double.PositiveInfinity
                             MaybeChildren = Some children
                             DepthAim = None}                                                           
                    let mutable searchDepth = tree.Depth
                    while searchDepth <= maxDepth && timeLeft do
                        let rec helper depth (node : Node) =
                            let main alpha beta =
                                match node.MaybeChildren with
                                | Some children ->
                                    let childrenToComplete = 
                                        children |> Array.mapi (fun index child  -> index, child) 
                                            |> Array.filter (fun (_, child) -> child.Depth < depth - 1.0) |> Array.sortBy (fun (_, child) -> child.Value) 
                                    let childrenToUpdate =
                                        if multiThreadingInUse then
                                            childrenToComplete |> Array.take 1
                                        elif childrenToComplete.Length >= threadsCount then
                                            childrenToComplete |> Array.take threadsCount
                                        else
                                            childrenToComplete    
                                    let childrenWithAlphaBeta = 
                                        childrenToUpdate |> Array.map (fun (_, child) ->
                                            match child.DepthAim with
                                            | Some aim when aim >= depth - 1.0 ->
                                                child
                                            | _ ->    
                                                {child with Alpha = -beta; Beta = -alpha; DepthAim = Some (depth - 1.0)}
                                        )
                                    let newChildren = 
                                        if childrenToComplete.Length = 1 then
                                            childrenWithAlphaBeta |> Array.map (helper (depth - 1.0))
                                        else
                                            multiThreadingInUse <- true
                                            let res = childrenWithAlphaBeta |> asyncMap (helper (depth - 1.0))
                                            multiThreadingInUse <- false
                                            res
                                    childrenToUpdate |> Array.map fst |> Array.iteri (fun sourceIndex destIndex ->
                                        children[destIndex] <- newChildren[sourceIndex]
                                    )
                                    let newAlpha = 
                                        let childrenValues = 
                                            newChildren |> Seq.filter (fun child -> child.Depth >= depth - 1.0) |> Seq.map (fun child -> -child.Value)
                                        if childrenValues |> Seq.isEmpty then
                                            alpha
                                        else
                                            let childrenAlpha = childrenValues |> Seq.max    
                                            Math.Max (alpha, childrenAlpha)
                                    let cacheState newValue newDepth =
                                        let flag =
                                            if newValue <= node.Alpha then
                                                UpperBound
                                            elif newValue >= beta then
                                                LowerBound
                                            else
                                                Exact
                                        cachedStates[node.State] <- {Value = newValue; Depth = newDepth; Bound = flag; State = node.State}                
                                    if children |> Seq.forall (fun child -> child.Depth >= depth - 1.0) then
                                        let childrenMinDepth = children |> Seq.map (fun child -> child.Depth) |> Seq.min
                                        let newDepth = childrenMinDepth + 1.0
                                        let newValue = children |> Seq.map (fun child -> -child.Value) |> Seq.max    
                                        cacheState newValue newDepth
                                        {node with Depth = newDepth; Value = newValue}
                                    elif newAlpha >= beta then
                                        cacheState newAlpha depth
                                        {node with Depth = depth; Value = newAlpha}                                        
                                    else 
                                        {node with Alpha = newAlpha}
                                | None ->
                                    let children = Array.init node.State.NumberOfPossibleMoves (fun index -> node.State.NthMove index |> getNewNode)
                                    let childMinDepth = children |> Seq.map (fun child -> child.Depth) |> Seq.min
                                    let value = children |> Seq.map (fun child -> -child.Value) |> Seq.max
                                    {node with MaybeChildren = Some children; Depth = childMinDepth + 1.0; Value = value}
                            if node.Depth >= depth then
                                node
                            else
                                if node <> tree then
                                    match cachedStates.TryGetValue node.State with
                                    | true, attr when attr.Depth >= depth ->
                                        usedCacheCount <- usedCacheCount + 1
                                        match attr.Bound with
                                        | Exact ->
                                            {node with Value = attr.Value; Depth = attr.Depth; State = attr.State}
                                        | LowerBound ->
                                            let alpha = Math.Max (node.Alpha, attr.Value)
                                            if alpha >= node.Beta then
                                                {node with Value = attr.Value; Depth = depth; Alpha = alpha}
                                            else
                                                main alpha node.Beta
                                        | UpperBound ->
                                            let beta = Math.Min (node.Beta, attr.Value)
                                            if node.Alpha >= beta then
                                                {node with Value = attr.Value; Depth = depth; Beta = beta}
                                            else
                                                main node.Alpha beta                       
                                    | _ ->
                                        main node.Alpha node.Beta
                                else
                                    main node.Alpha node.Beta
                        tree <- helper searchDepth tree 
                        if tree.Depth >= searchDepth then
                            let message = 
                                String.Format ("Reached search depth: {0:0}, Cached states: {1}, Used cache {2} times, Maximally reached depth: {3:0}",
                                    searchDepth, cachedStates.Count, usedCacheCount, reachedMaxDepth)
                            sendMessage.Trigger message
                            searchDepth <- tree.Depth + 1.0
                            tree <- {tree with Alpha = Double.NegativeInfinity; Beta = Double.PositiveInfinity}
                            if searchDepth > reachedMaxDepth then
                                reachedMaxDepth <- searchDepth
                    (*if tree.State.Equals game then
                        Console.WriteLine "Alles ok."
                    else
                        Console.WriteLine "?!?!"    *)
                    let maxValue = [0..(game.NumberOfPossibleMoves-1)] |> List.map (fun index -> -tree.MaybeChildren.Value[index].Value) |> List.max
                    let maxMoves = 
                        [0..(game.NumberOfPossibleMoves-1)] |> List.filter (fun index -> -tree.MaybeChildren.Value[index].Value = maxValue) |> List.toArray
                    let chosenMove = maxMoves[rnd.Next maxMoves.Length]
                    maybePermanentTree <- Some tree
                    mutableGame.MakeMove chosenMove
                }
            if not registeredMoveMadeEvent then
                mutableGame.add_MoveMadeEvent (fun move -> 
                    match maybePermanentTree with
                    | Some permanentTree -> 
                        match permanentTree.MaybeChildren with
                        | Some children ->
                            if not (mutableGame.Equals children[move].State) then
                                Console.WriteLine ()
                                Console.WriteLine "Mutable game:"
                                Console.WriteLine ()
                                Console.WriteLine mutableGame
                                Console.WriteLine ()
                                Console.WriteLine "Children[move]:"
                                Console.WriteLine ()
                                Console.WriteLine children[move].State
                                Console.WriteLine ()
                                raise (Exception "Not matching.")
                            else
                                Console.WriteLine "Everything fine."    
                            maybePermanentTree <- Some children[move]
                        | None ->
                            maybePermanentTree <- None     
                    | None ->
                        maybePermanentTree <- None
                )   
                mutableGame.add_ReInitialized (fun _ _ -> maybePermanentTree <- None)
                registeredMoveMadeEvent <- true     
            let timer = new Timers.Timer (considerationTime)
            timer.AutoReset <- false
            timer.Elapsed.AddHandler (fun _ _ -> timeLeft <- false)
            timer.Start ()
            timeLimitedSearch |> Async.Start