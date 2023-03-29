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

type MCTree = 
    {State : ImmutableGame;
     NumberOfSimulations : int;
     TotalScore : float;
     Children : MCTree [];
     UntriedMoves : Set<int>}

type MonteCarloTreeSearch (player : int, searchTime : int, countInfinitiesMode, ?usedThreads) =
    let rnd = Random ()
    let sendMessage = Event<String> ()
    let threadsCount =
        match usedThreads with
        | Some threads ->
            threads
        | None ->
            1        
    let mutable considerationTime = searchTime
    let mutable registeredMoveMadeEvent = false
    let mutable maybePermanentTree = None
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
            //Performs one additional simulation in "tree" and returns a tuple consisting of the resulting tree and the final score of the simulation
            //for the AI player. The score is only needed for the recursive calls to propagate the value back. 
            let rec getNextTree (tree : MCTree) =
                let getFinalScore (state : ImmutableGame) =
                    let score = state.Value player
                    if countInfinitiesMode then
                        if score = Double.PositiveInfinity then
                            1.0
                        elif score = Double.NegativeInfinity then
                            -1.0
                        else
                            0.0        
                    else
                        score
                //Check if final state.
                if tree.State.NumberOfPossibleMoves = 0 then
                    let finalScore = getFinalScore tree.State
                    let resTree = {tree with NumberOfSimulations = tree.NumberOfSimulations + 1; TotalScore = tree.TotalScore + finalScore}
                    resTree, finalScore
                else
                    //Check if already completely expanded.
                    if tree.UntriedMoves.Count = 0 then
                        //Select child according to UCB1-formula, which reflects a compromise between exploitation and exploration.
                        let selectedChildIndex = 
                            let UCB1formula (child : MCTree) = 
                                let nodeSimCount = tree.NumberOfSimulations |> float
                                let childSimCount = child.NumberOfSimulations |> float
                                child.TotalScore / childSimCount + sqrt (2.0 * (log nodeSimCount) / childSimCount) 
                            [0..(tree.State.NumberOfPossibleMoves-1)] |> List.maxBy (fun i -> UCB1formula tree.Children[i]) 
                        //Perform one additional simulation in child-tree and update score.
                        let updatedChild, additionalScore = getNextTree tree.Children[selectedChildIndex]
                        tree.Children[selectedChildIndex] <- updatedChild
                        let updatedTree = {tree with NumberOfSimulations = tree.NumberOfSimulations + 1; TotalScore = tree.TotalScore + additionalScore}
                        updatedTree, additionalScore
                    else
                        //Expand next child.   
                        let index = rnd.Next tree.UntriedMoves.Count
                        let untriedMove = (tree.UntriedMoves |> Seq.toArray).[index]
                        let reducedUntriedMoves = tree.UntriedMoves |> Set.remove untriedMove
                        let nextState = tree.State.NthMove untriedMove 
                        let childUntriedMoves = [0..(nextState.NumberOfPossibleMoves-1)] |> Set.ofList
                        //random rollout
                        let randomFinalState =
                            let rec helper (state : ImmutableGame) =
                                if state.Running then
                                    let randomMove = rnd.Next state.NumberOfPossibleMoves
                                    state.NthMove randomMove |> helper
                                else    
                                    state
                            nextState |> helper       
                        let finalScore = getFinalScore randomFinalState
                        let newChild = 
                            {State = nextState; NumberOfSimulations = 1; TotalScore = finalScore; UntriedMoves = childUntriedMoves;
                            Children = Array.zeroCreate nextState.NumberOfPossibleMoves} 
                        tree.Children[untriedMove] <- newChild
                        let updatedTree = 
                            {tree with NumberOfSimulations = tree.NumberOfSimulations + 1; TotalScore = tree.TotalScore + finalScore;
                                       UntriedMoves = reducedUntriedMoves}      
                        updatedTree, finalScore  
            let rec mergeTrees (originalTree : MCTree) (maybeNewTree1 : Option<MCTree>) (maybeNewTree2 : Option<MCTree>) =
                match maybeNewTree1, maybeNewTree2 with
                | None, None ->
                    originalTree
                | None, Some newTree2 ->
                    newTree2
                | Some newTree1, None ->
                    newTree1
                | Some newTree1, Some newTree2 ->        
                    if newTree1.NumberOfSimulations > originalTree.NumberOfSimulations && newTree2.NumberOfSimulations = originalTree.NumberOfSimulations then
                        newTree1
                    elif newTree2.NumberOfSimulations > originalTree.NumberOfSimulations && newTree1.NumberOfSimulations = originalTree.NumberOfSimulations then
                        newTree2
                    elif newTree1.NumberOfSimulations > originalTree.NumberOfSimulations && newTree2.NumberOfSimulations > originalTree.NumberOfSimulations then
                        let children = [|0..originalTree.State.NumberOfPossibleMoves-1|] |> Array.map (fun move ->
                            if not (originalTree.UntriedMoves.Contains move) then
                                let maybeTree1Child = 
                                    if newTree1.UntriedMoves.Contains move then
                                        None
                                    else
                                        Some newTree1.Children[move]
                                let maybeTree2Child = 
                                    if newTree2.UntriedMoves.Contains move then
                                        None
                                    else
                                        Some newTree2.Children[move]
                                mergeTrees originalTree.Children[move] maybeTree1Child maybeTree2Child         
                            elif not (newTree1.UntriedMoves.Contains move) && newTree2.UntriedMoves.Contains move then
                                newTree1.Children[move]
                            elif not (newTree2.UntriedMoves.Contains move) && newTree1.UntriedMoves.Contains move then
                                newTree2.Children[move]
                            elif not (newTree1.UntriedMoves.Contains move) && not (newTree2.UntriedMoves.Contains move) then
                                let rec mergeTwo (tree1 : MCTree) (tree2 : MCTree) =
                                    let children =  [|0..tree1.State.NumberOfPossibleMoves-1|] |> Array.map (fun move ->
                                        if not (tree1.UntriedMoves.Contains move) && tree2.UntriedMoves.Contains move then
                                            tree1.Children[move]
                                        elif not (tree2.UntriedMoves.Contains move) && tree1.UntriedMoves.Contains move then
                                            tree2.Children[move]
                                        elif not (tree1.UntriedMoves.Contains move) && not (tree2.UntriedMoves.Contains move) then
                                            mergeTwo tree1.Children[move] tree2.Children[move]
                                        else
                                            tree1.Children[move]    
                                    )
                                    {State = tree1.State
                                     NumberOfSimulations = tree1.NumberOfSimulations + tree2.NumberOfSimulations
                                     TotalScore = tree1.TotalScore + tree2.TotalScore
                                     Children = children
                                     UntriedMoves = Set.intersect tree1.UntriedMoves tree2.UntriedMoves}
                                mergeTwo newTree1.Children[move] newTree2.Children[move]     
                            else
                                originalTree.Children[move]
                        )
                        {State = originalTree.State
                         NumberOfSimulations = newTree1.NumberOfSimulations + newTree2.NumberOfSimulations - originalTree.NumberOfSimulations
                         TotalScore = newTree1.TotalScore + newTree2.TotalScore - originalTree.TotalScore
                         Children = children
                         UntriedMoves = Set.intersect newTree1.UntriedMoves newTree2.UntriedMoves}  
                    else
                        originalTree      
            if not registeredMoveMadeEvent then
                mutableGame.add_MoveMadeEvent (fun move -> 
                    match maybePermanentTree with
                    | Some permanentTree when not (permanentTree.UntriedMoves.Contains move) -> 
                        maybePermanentTree <- Some permanentTree.Children[move]
                    | _ ->
                        maybePermanentTree <- None
                )   
                mutableGame.add_ReInitialized (fun _ _ -> maybePermanentTree <- None)
                registeredMoveMadeEvent <- true               
            //Initialize timer.
            let mutable timeLeft = true
            let timer = new Timers.Timer (considerationTime)
            timer.Elapsed.AddHandler (fun _ _ -> timeLeft <- false)
            timer.AutoReset <- false
            timer.Start ()
            //Initialize algorithm and then perform as many simulations as possible.
            let mainRoutine = async {
                let mutable tree = 
                    match maybePermanentTree with
                    | Some permanentTree ->
                        permanentTree
                    | None ->    
                        let state = game :?> ImmutableGame
                        let root = 
                            {State = state; NumberOfSimulations = 0; TotalScore = 0; Children = Array.zeroCreate state.NumberOfPossibleMoves;
                            UntriedMoves = [0..state.NumberOfPossibleMoves-1] |> Set.ofList}       
                        root
                let mutable simCount = 0
                while timeLeft do
                    let nextTrees = 
                        if threadsCount = 1 then
                            [|getNextTree tree |> fst|]
                        else    
                            async {return getNextTree tree |> fst} |> Array.replicate threadsCount |> Async.Parallel |> Async.RunSynchronously
                    let mergedTree =
                        if threadsCount = 1 then
                            nextTrees[0]
                        else
                            nextTrees |> Array.reduce (fun tree1 tree2 -> mergeTrees tree (Some tree1) (Some tree2))    
                    tree <- mergedTree
                    simCount <- simCount + threadsCount   
                    sendMessage.Trigger (sprintf "Number of simulations: %A" simCount)    
                //Apply move most simulations have been performed with.
                let chosenMove = 
                    tree.Children |> Array.mapi (fun index ch -> index, ch)  |> Seq.maxBy (fun (_, child) -> 
                        if child :> Object <> null then child.NumberOfSimulations else 0) |> fst
                maybePermanentTree <- Some tree
                mutableGame.MakeMove chosenMove
            }
            mainRoutine |> Async.Start