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

type NegaMaxTimeLimited (playerID : int, searchTime : int, maxDepth : int) =
    let sendMessage = Event<String> ()
    let mutable player = playerID
    let mutable considerationTime = searchTime
    let mutable reachedMaxDepth = 0
    member val MaximalSearchDepth = maxDepth with get, set
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
            let state = game :?> ImmutableGame
            let mutable chosenMove = 0
            let mutable timeLeft = true
            let timer = new Timers.Timer (considerationTime)
            timer.AutoReset <- false
            timer.Elapsed.AddHandler (fun _ _ -> 
                timeLeft <- false
                mutableGame.MakeMove chosenMove
            )
            timer.Start ()
            let timeLimitedSearch =
                async {
                    let mutable searchDepth = 1
                    while searchDepth <= x.MaximalSearchDepth && timeLeft do
                        let rec helper step (state : ImmutableGame) =
                            let numberOfPossibleMoves = state.NumberOfPossibleMoves
                            if (step = 0) || (numberOfPossibleMoves = 0)  then
                                state.ZSValue
                            else 
                                [0..(numberOfPossibleMoves-1)] |> List.map (fun move -> -(helper (step-1) (state.NthMove move))) |> List.max
                        let chosen = [0..(state.NumberOfPossibleMoves-1)] |> List.maxBy (fun move -> -(helper (searchDepth-1) (state.NthMove move)))
                        if timeLeft then
                            chosenMove <- chosen
                            if searchDepth > reachedMaxDepth then
                                reachedMaxDepth <- searchDepth
                            sendMessage.Trigger (sprintf "Reached search depth: %A, Maximally reached depth: %A" searchDepth reachedMaxDepth)
                            searchDepth <- searchDepth + 1
                    if searchDepth >= maxDepth && timeLeft then
                        timer.Interval <- 1
                }
            timeLimitedSearch |> Async.Start            