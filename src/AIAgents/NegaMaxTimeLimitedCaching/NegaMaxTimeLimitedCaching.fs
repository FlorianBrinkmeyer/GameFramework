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
open System.Collections.Concurrent

type GameStateAttr = {Value : float; RemainingSteps : int; mutable UsedCount : int}

type NegaMaxTimeLimitedCaching (player : int, searchTime : int, maxDepth : int, justStoreHashes, ?cacheMaxSize : int) =
    let cachedStateHashes = ConcurrentDictionary<int, GameStateAttr> ()
    let cachedHashesQueue = ConcurrentQueue<int> ()
    let cachedStates = ConcurrentDictionary<ImmutableGame, GameStateAttr> ()
    let cachedStatesQueue = ConcurrentQueue<ImmutableGame> ()
    let mutable usedCacheCount = 0
    let mutable reachedMaxDepth = 0
    let sendMessage = Event<String> ()
    interface AI_Informer with
        [<CLIEvent>]
        member x.SendMessage = sendMessage.Publish 
    interface AI_Agent with
        member x.Player = player
        member x.MakeMove mutableGame game =
            let immutableGame = game :?> ImmutableGame
            let mutable chosenMove = 0
            let mutable timeLeft = true
            let timer = new Timers.Timer (searchTime)
            timer.AutoReset <- false
            timer.Elapsed.AddHandler (fun _ _ -> 
                timeLeft <- false
                mutableGame.MakeMove chosenMove
            )
            timer.Start ()
            let timeLimitedSearch =
                async {
                    let mutable searchDepth = 1
                    while searchDepth <= maxDepth && timeLeft do
                        let rec helper step (state : ImmutableGame) =
                            let numberOfPossibleMoves = state.NumberOfPossibleMoves
                            if (step = 0) || (numberOfPossibleMoves = 0)  then
                                state.ZSValue 
                            else 
                                let help (cached : ConcurrentDictionary<'t,GameStateAttr>) (queue : ConcurrentQueue<'t>) (st : 't) =
                                    match cached.TryGetValue st with
                                    | true, attr when attr.RemainingSteps >= step ->
                                        usedCacheCount <- usedCacheCount + 1
                                        queue.Enqueue st
                                        attr.UsedCount <- attr.UsedCount + 1
                                        attr.Value
                                    | _ ->
                                        let usedCount =
                                            match cached.TryGetValue st with
                                            | true, attr ->
                                                attr.UsedCount
                                            | _ -> 0    
                                        let value = 
                                            [0..(numberOfPossibleMoves-1)] |> List.map (fun move -> -(helper (step-1) (state.NthMove move))) |> List.max
                                        cached[st] <- {Value = value; RemainingSteps = step; UsedCount = usedCount + 1}
                                        queue.Enqueue st
                                        match cacheMaxSize with
                                        | Some maxSize ->
                                            while cached.Count > maxSize && queue.Count > 0 && timeLeft do
                                                match queue.TryDequeue () with
                                                | true, potToDelete ->
                                                    match cached.TryGetValue potToDelete with
                                                    | true, attr ->
                                                        attr.UsedCount <- attr.UsedCount - 1
                                                        if attr.UsedCount <= 0 then
                                                            cached.TryRemove potToDelete |> ignore
                                                    | _ -> ()
                                                | _ -> ()        
                                        | None -> ()
                                        value
                                if justStoreHashes then
                                    let hash = state.GetHashCode ()
                                    help cachedStateHashes cachedHashesQueue hash
                                else
                                    help cachedStates cachedStatesQueue state    
                        let chosen = [0..(immutableGame.NumberOfPossibleMoves-1)] |> List.maxBy (fun move -> -(helper (searchDepth-1) (immutableGame.NthMove move)))
                        if timeLeft then
                            chosenMove <- chosen
                            let statesCount =
                                if justStoreHashes then
                                    cachedStateHashes.Count
                                else
                                    cachedStates.Count     
                            if searchDepth > reachedMaxDepth then
                                reachedMaxDepth <- searchDepth
                            sendMessage.Trigger (sprintf "Reached search depth: %A, Cached states: %A, Used cache %A times, Maximally reached depth: %A" searchDepth statesCount usedCacheCount reachedMaxDepth)
                            searchDepth <- searchDepth + 1
                    if searchDepth >= maxDepth && timeLeft then
                        timer.Interval <- 1
                }
            timeLimitedSearch |> Async.Start            