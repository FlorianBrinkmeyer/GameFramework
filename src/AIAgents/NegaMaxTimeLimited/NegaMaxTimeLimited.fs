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

type NegaMaxTimeLimited (player : int, searchTime : int, maxDepth : int) =
    let rnd = Random ()
    let sendMessage = Event<String> ()
    let mutable considerationTime = searchTime
    let mutable reachedMaxDepth = 0
    member val MaximalSearchDepth = maxDepth with get, set
    interface AI_WithConsiderationTime with
        member x.Player = player
        member x.ConsiderationTime
            with get () = considerationTime
            and set (value) = considerationTime <- value
    interface AI_Informer with
        [<CLIEvent>]
        member x.SendMessage = sendMessage.Publish
    interface AI_Agent with
        member x.Player = player
        member x.MakeMove mutableGame game =
            let immutableGame = game :?> ImmutableGame
            let mutable chosenMove = 0
            let mutable timeLeft = true
            let timer = new Timers.Timer (considerationTime)
            timer.AutoReset <- false
            timer.Elapsed.AddHandler (fun _ _ -> timeLeft <- false)
            timer.Start ()
            let timeLimitedSearch =
                async {
                    let mutable searchDepth = 1
                    while searchDepth <= x.MaximalSearchDepth && timeLeft do
                        let rec helper step (state : ImmutableGame) =
                            let numberOfPossibleMoves = state.NumberOfPossibleMoves
                            if (step = 0) || (numberOfPossibleMoves = 0)  then
                                state.ZSValue |> Some
                            else 
                                if timeLeft then
                                    let results = [0..(numberOfPossibleMoves-1)] |> List.map (fun move -> (helper (step-1) (state.NthMove move)))
                                    if timeLeft then
                                        results |> List.map (fun potValue -> -potValue.Value) |> List.max |> Some
                                    else
                                        None    
                                else
                                    None
                        let moveAndValue = 
                            [0..(immutableGame.NumberOfPossibleMoves-1)] |> List.map (fun move -> move, (helper (searchDepth-1) (immutableGame.NthMove move))) 
                        if timeLeft then
                            let maxValue = moveAndValue |> List.map (fun (_, maybeValue) -> - maybeValue.Value) |> List.max
                            let maxMoves = 
                                moveAndValue |> List.filter (fun (_, maybeValue) -> - maybeValue.Value = maxValue) |> List.map fst |> List.toArray
                            chosenMove <- maxMoves[rnd.Next maxMoves.Length]
                            if searchDepth > reachedMaxDepth then
                                reachedMaxDepth <- searchDepth
                            sendMessage.Trigger (sprintf "Reached search depth: %A, Maximally reached depth: %A" searchDepth reachedMaxDepth)
                            searchDepth <- searchDepth + 1
                    timer.Stop ()
                    mutableGame.MakeMove chosenMove
                }
            timeLimitedSearch |> Async.Start            