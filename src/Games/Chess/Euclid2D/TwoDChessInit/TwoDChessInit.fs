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

module Chess.Init

open System
open System.IO
open GameFramework
open Euclid2DGame
open Euclid2D.Enumerable2DArray
open ImmutableEnumerable2DArray
open GameFramework.Chess.MoveCalculation

let initChess xDim yDim startPlayer (pieceFactory : Func<String, int, IPiece>) (boardStartConfigurationCsv : String) (resultMapper : Func<ImmutableGame,'GameResult>)
    (agents : seq<AI_Agent>) =
    let boardSeqOfSeq = 
        boardStartConfigurationCsv |> File.ReadAllLines |> Seq.rev |> Seq.map (fun line -> line.Split (',') |> Seq.map (fun entry -> 
            if entry <> String.Empty then
                let kind = entry |> Seq.takeWhile Char.IsAsciiLetter |> Seq.toArray |> String
                let player = entry |> Seq.skip kind.Length |> Seq.toArray |> Int32.Parse
                pieceFactory.Invoke (kind, player) |> Some
            else
                None    
        ))
    let board = ImmutableEnumerable2DArray<IPiece> (xDim, yDim, seqOfSeqOptToMap boardSeqOfSeq)
    let firstMoveCalcRes, _ = calculatePossibleMoves board startPlayer None
    let zsEvaluation (board : ImmutableEnumerable2DArray<IPiece>) = 
        (board :> IEnumerable2DArray<IPiece>).AllEntriesWithCoords |> Seq.map (fun (piece, pos) -> 
            ((piece :?> ISelfEvaluatingPiece<ImmutableEnumerable2DArray<IPiece>, int*int>).Value board pos) * (float) piece.Player
        ) |> Seq.sum
    let game = 
        ImmutableZeroSumBoardGameSelfCalculatingPieces<ImmutableEnumerable2DArray<IPiece>, int*int, IMoveCommand<int*int>, IBoardMoveEvent>
            (board, startPlayer, firstMoveCalcRes, calculatePossibleMoves, Some zsEvaluation)
    let gameCompanion = BoardGameCompanion<'GameResult, IEnumerable2DArray<IPiece>, IBoardMoveEvent> (game, agents, resultMapper)
    let boardCompanion = TwoDBoardCompanionMovablePieces gameCompanion    
    gameCompanion, boardCompanion   