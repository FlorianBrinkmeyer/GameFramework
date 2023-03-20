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

type GameStateAttr = {Value : float; RemainingSteps : int; mutable UsedCount : int}

type NegaMaxTimeLimitedCaching (playerID : int, searchTime : int, maxDepth : int, ?justStoreHashes, ?cacheMaxSize : int) =
    let cachedStateHashes = Generic.Dictionary<int, GameStateAttr> ()
    let cachedHashesQueue = Generic.Queue<int> ()
    let cachedStates = Generic.Dictionary<ImmutableGame, GameStateAttr> ()
    let cachedStatesQueue = Generic.Queue<ImmutableGame> ()
    let mutable usedCacheCount = 0
    let mutable reachedMaxDepth = 0
    let mutable player = playerID
    let mutable considerationTime = searchTime
    let mutable justHashes =
        match justStoreHashes with
        | Some value ->
            value
        | None -> false    
    let sendMessage = Event<String> ()
    member val MaximalSearchDepth = maxDepth with get, set
    member val CacheMaxSize = cacheMaxSize with get, set
    member x.JustStoreHashes
        with get () = justHashes
        and set (value) = justHashes <- value 
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
                        let rec helper step (state : ImmutableGame) : Option<float> =
                            let numberOfPossibleMoves = state.NumberOfPossibleMoves
                            let nextStep (cached : Generic.Dictionary<'t,GameStateAttr>) (queue : Generic.Queue<'t>) (st : 't) =
                                if timeLeft then
                                    match cached.TryGetValue st with
                                    | true, attr when attr.RemainingSteps >= step ->
                                        usedCacheCount <- usedCacheCount + 1
                                        queue.Enqueue st
                                        attr.UsedCount <- attr.UsedCount + 1
                                        attr.Value |> Some
                                    | _ ->
                                        let usedCount =
                                            match cached.TryGetValue st with
                                            | true, attr ->
                                                attr.UsedCount
                                            | _ -> 0    
                                        let results = [0..(numberOfPossibleMoves-1)] |> List.map (fun move -> (helper (step-1) (state.NthMove move)))
                                        if timeLeft then
                                            let value = results |> List.map (fun potValue -> -potValue.Value) |> List.max
                                            cached[st] <- {Value = value; RemainingSteps = step; UsedCount = usedCount + 1}
                                            queue.Enqueue st
                                            match x.CacheMaxSize with
                                            | Some maxSize ->
                                                while cached.Count > maxSize && timeLeft do
                                                    let potToDelete = queue.Dequeue ()
                                                    let attr = cached[potToDelete]
                                                    attr.UsedCount <- attr.UsedCount - 1
                                                    if attr.UsedCount = 0 then
                                                        cached.Remove potToDelete |> ignore
                                            | None -> ()
                                            value |> Some
                                        else
                                            None    
                                else
                                    None
                            if (step = 0) || (numberOfPossibleMoves = 0)  then
                                state.ZSValue |> Some
                            else 
                                if justHashes then
                                    let hash = state.GetHashCode ()
                                    nextStep cachedStateHashes cachedHashesQueue hash
                                else
                                    nextStep cachedStates cachedStatesQueue state    
                        let moveAndValue = 
                            [0..(immutableGame.NumberOfPossibleMoves-1)] |> List.map (fun move -> move, (helper (searchDepth-1) (immutableGame.NthMove move))) 
                        if timeLeft then
                            chosenMove <- moveAndValue |> List.maxBy (fun (_, maybeValue) -> - maybeValue.Value) |> fst 
                            let statesCount =
                                if justHashes then
                                    cachedStateHashes.Count
                                else
                                    cachedStates.Count     
                            if searchDepth > reachedMaxDepth then
                                reachedMaxDepth <- searchDepth
                            sendMessage.Trigger (sprintf "Reached search depth: %A, Cached states: %A, Used cache %A times, Maximally reached depth: %A" 
                                searchDepth statesCount usedCacheCount reachedMaxDepth)
                            searchDepth <- searchDepth + 1
                    timer.Stop ()
                    mutableGame.MakeMove chosenMove
                }
            timeLimitedSearch |> Async.Start            