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

module Reversi.Init

open GameFramework
open Euclid2DGame
open System
open Euclid2D.Enumerable2DArray

let turnedPiecesInDir (board : IEnumerable2DArray<int>) activePlayer pos dir =
    let otherPlayer = activePlayer * (-1)
    let enemyPiecesInDir = board.ToSeqInDirWithCoords pos dir |> Seq.skip 1 |> Seq.takeWhile (fun (maybePiece,_) ->
        match maybePiece with
        | Some player ->
            player = otherPlayer
        | None ->
            false    
    )
    let enemyPiecesLen = enemyPiecesInDir |> Seq.length
    if enemyPiecesLen >= 1 then
        match board.ToSeqInDir pos dir |> Seq.skip (enemyPiecesLen + 1) |> Seq.tryHead with
        | Some (Some player) when player = activePlayer ->
            enemyPiecesInDir |> Seq.map snd |> Some    
        | _ ->
            None
    else
        None            

let rec possibleMoves withPassing alreadyPassed (board : IEnumerable2DArray<int>) previousPlayer state = 
    let activePlayer = previousPlayer * (-1)
    if state >= 4 then
        let posMoves = 
            board.AllEmptyCoords |> Seq.filter (fun pos -> allDirs |> Seq.choose (turnedPiecesInDir board activePlayer pos) |> Seq.isEmpty |> not)
        let posMovesArr = posMoves |> Seq.toArray
        if posMovesArr.Length = 0 then
            if alreadyPassed || not withPassing then
                activePlayer, GameOver
            else
                possibleMoves true true board activePlayer state    
        else 
            activePlayer, PossibleMoves posMovesArr    
    else
        let potFourFields = 
            [(board.xDim / 2, board.yDim / 2); (board.xDim / 2 - 1, board.yDim / 2); (board.xDim / 2, board.yDim / 2 - 1); (board.xDim / 2 - 1, board.yDim / 2 - 1)]
        let legalMoves = potFourFields |> List.filter (fun (x,y) -> board[x,y].IsNone) |> List.toArray |> PossibleMoves
        activePlayer, legalMoves

let makeMove<'Board when 'Board :> IEnumerable2DArray<int> and 'Board :> ImmutableArray<(int*int),int>> 
    chosenMove (board : 'Board) activePlayer state =
    let allTurnedPieces = 
        if state >= 4 then
            let turnedPieces = allDirs |> List.choose (turnedPiecesInDir board activePlayer chosenMove) |> Seq.concat
            Seq.append turnedPieces [chosenMove]
        else
            [chosenMove]   
    let nextBoard = allTurnedPieces |> Seq.fold (fun (brd : 'Board) (x,y) -> brd.GetNext (x,y) (Some activePlayer) :?> 'Board) board   
    let nextState = state + 1
    let events = allTurnedPieces |> Seq.map BoardSetEvent<int*int>
    nextBoard, events, nextState

let getZSValue<'Board when 'Board :> IEnumerable2DArray<int> and 'Board :> ImmutableArray<(int*int),int>>
    (board : 'Board) =
    let whitePiecesCount = board.AllEntriesWithCoords |> Seq.filter (fun (piece, _) -> piece = 1) |> Seq.length
    let blackPiecesCount = board.AllEntriesWithCoords |> Seq.filter (fun (piece, _) -> piece = -1) |> Seq.length    
    (float) (whitePiecesCount - blackPiecesCount)

let initReversi xDim yDim startPlayer withPassing (resultMapper : Func<ImmutableGame,'T>) (agents : seq<AI_Agent>) debugMode =
    let board = ImmutableEnumerable2DArray.ImmutableEnumerable2DArray (xDim, yDim, Map.empty<int*int,int>, debugMode) 
    let _, firstMoveCalcRes = possibleMoves withPassing false board (startPlayer * (-1)) 0
    let game = ImmutableBoardSetGame (board, startPlayer, firstMoveCalcRes, makeMove, possibleMoves withPassing false, getZSValue, 0, None)
    let gameCompanion = BoardGameCompanion (game, agents, resultMapper, debugMode)
    let boardCompanion = TwoDSetBoardCompanion<int> gameCompanion
    gameCompanion, boardCompanion