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

namespace GameFramework

open System
open System.Collections

type Node = {State : ImmutableGame;
             Value : float;
             Depth : float;
             DepthAim : Option<float>
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
                 Children = None
                 DepthAim = None}
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
                         Children = Some children
                         DepthAim = None}                                                           
                    let mutable tree = startNode
                    while searchDepth <= maxDepth && timeLeft do
                        let rec helper (node : Node) depth =
                            let main alpha beta =
                                match node.Children with
                                | Some children ->
                                    let (index, chosenChild) =
                                        let childrenToComplete = 
                                            children |> Array.mapi (fun index child  -> index, child) 
                                                |> Seq.filter (fun (_, child) -> child.Depth < depth - 1.0) 
                                        childrenToComplete |> Seq.minBy (fun (_, child) -> child.Value)
                                    let childWithAlphaBeta = 
                                        match chosenChild.DepthAim with
                                        | Some aim when aim >= depth - 1.0 ->
                                            chosenChild
                                        | _ ->    
                                            {chosenChild with Alpha = -beta; Beta = -alpha; DepthAim = Some (depth - 1.0)}
                                    let newChild = helper childWithAlphaBeta (depth - 1.0)
                                    children[index] <- newChild
                                    let newAlpha = 
                                        if newChild.Depth >= depth - 1.0 then
                                            Math.Max (alpha, -newChild.Value)
                                        else
                                            alpha        
                                    let cacheState newValue newDepth =
                                        let flag =
                                            if newValue <= node.Alpha then
                                                UpperBound
                                            elif newValue >= beta then
                                                LowerBound
                                            else
                                                Exact
                                        cachedStates[node.State] <- {Value = newValue; Depth = newDepth; Bound = flag}                
                                    if children |> Seq.forall (fun child -> child.Depth >= depth - 1.0) then
                                        let childrenMinDepth = children |> Seq.map (fun child -> child.Depth) |> Seq.min
                                        let newDepth = childrenMinDepth + 1.0
                                        let newValue = children |> Seq.map (fun child -> -child.Value) |> Seq.max    
                                        cacheState newValue newDepth
                                        {node with Depth = newDepth; Value = newValue}
                                    elif newAlpha >= beta then
                                        cacheState newAlpha depth
                                        {node with Depth = depth; Value = newAlpha}                                        
                                    elif newChild.Depth >= depth - 1.0 then
                                        {node with Alpha = newAlpha}
                                    else
                                        node    
                                | None ->
                                    let children = Array.init node.State.NumberOfPossibleMoves (fun index -> node.State.NthMove index |> getNewNode)
                                    let childMinDepth = children |> Seq.map (fun child -> child.Depth) |> Seq.min
                                    let value = children |> Seq.map (fun child -> -child.Value) |> Seq.max
                                    {node with Children = Some children; Depth = childMinDepth + 1.0; Value = value}
                            if node.Depth >= depth then
                                node
                            else
                                match node.DepthAim with
                                | Some aim when aim >= depth ->
                                    main node.Alpha node.Beta
                                | _ ->    
                                    match cachedStates.TryGetValue node.State with
                                    | true, attr when attr.Depth >= depth ->
                                        usedCacheCount <- usedCacheCount + 1
                                        match attr.Bound with
                                        | Exact ->
                                            {node with Value = attr.Value; Depth = attr.Depth}
                                        | LowerBound ->
                                            let alpha = Math.Max (node.Alpha, attr.Value)
                                            if alpha >= node.Beta then
                                                {node with Value = attr.Value; Depth = attr.Depth; Alpha = alpha}
                                            else
                                                main alpha node.Beta
                                        | UpperBound ->
                                            let beta = Math.Min (node.Beta, attr.Value)
                                            if node.Alpha >= beta then
                                                {node with Value = attr.Value; Depth = attr.Depth; Beta = beta}
                                            else
                                                main node.Alpha beta                       
                                    | _ ->
                                        main node.Alpha node.Beta
                        tree <- helper tree searchDepth
                        if tree.Depth >= searchDepth then
                            let message = 
                                String.Format ("Reached search depth: {0:0}, Cached states: {1}, Used cache {2} times, Maximally reached depth: {3:0}",
                                    searchDepth, cachedStates.Count, usedCacheCount, reachedMaxDepth)
                            sendMessage.Trigger message
                            searchDepth <- tree.Depth + 1.0
                            tree <- {tree with Alpha = Double.NegativeInfinity; Beta = Double.PositiveInfinity}
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