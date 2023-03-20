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

type MonteCarloTreeSearch (playerID : int, searchTime : int) =
    let sendMessage = Event<String> ()
    let mutable player = playerID
    let mutable considerationTime = searchTime
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
            let rnd = Random ()
            //Performs one additional simulation in "tree" and returns a tuple consisting of the resulting tree and the final score of the simulation
            //for the AI player. The score is only needed for the recursive calls to propagate the value back. 
            let rec getNextTree (tree : MCTree) =
                //Check if final state.
                if tree.State.NumberOfPossibleMoves = 0 then
                    let finalScore = tree.State.Value player
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
                                if not state.Running then
                                    state
                                else    
                                    let randomMove = rnd.Next state.NumberOfPossibleMoves
                                    state.NthMove randomMove |> helper
                            nextState |> helper       
                        let finalScore = randomFinalState.Value player
                        let newChild = 
                            {State = nextState; NumberOfSimulations = 1; TotalScore = finalScore; UntriedMoves = childUntriedMoves;
                            Children = Array.zeroCreate nextState.NumberOfPossibleMoves} 
                        tree.Children[untriedMove] <- newChild
                        let updatedTree = 
                            {tree with NumberOfSimulations = tree.NumberOfSimulations + 1; TotalScore = tree.TotalScore + finalScore;
                                       UntriedMoves = reducedUntriedMoves}      
                        updatedTree, finalScore  
            //Initialize timer.
            let mutable timeLeft = true
            let timer = new Timers.Timer (considerationTime)
            timer.Elapsed.AddHandler (fun _ _ -> timeLeft <- false)
            timer.AutoReset <- false
            timer.Start ()
            //Initialize algorithm and then perform as many simulations as possible.
            let mainRoutine = async {
                let state = game :?> ImmutableGame
                let root = 
                    {State = state; NumberOfSimulations = 0; TotalScore = 0; Children = Array.zeroCreate state.NumberOfPossibleMoves;
                    UntriedMoves = [0..state.NumberOfPossibleMoves-1] |> Set.ofList}       
                let mutable tree = root
                let mutable simCount = 0
                while timeLeft do
                    tree <- getNextTree tree |> fst 
                    simCount <- simCount + 1    
                    sendMessage.Trigger (sprintf "Number of simulations: %A" simCount)    
                //Apply move most simulations have been performed with.
                let chosenMove = 
                    tree.Children |> Array.mapi (fun index ch -> index, ch)  |> Seq.maxBy (fun (_, child) -> 
                        if child :> Object <> null then child.NumberOfSimulations else 0) |> fst
                mutableGame.MakeMove chosenMove
            }
            mainRoutine |> Async.Start