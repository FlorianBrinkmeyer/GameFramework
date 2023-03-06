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
open GameFramework

///Current board * active player * new position of last moved piece -> possible moves / game result * board events
type MoveCalcDelegate<'Board, 'Coords, 'MoveCommand, 'BoardEvnt> = Func<'Board, int, 'Coords, MoveCalcResult<'MoveCommand, 'Coords> * seq<'BoardEvnt>> 

///Current board * active player -> zero-sum value
type ZSValueCalcDelegate<'Board> = Func<'Board, int, float>

type ImmutableZeroSumBoardGameSelfCalculatingPieces<'Board, 'Coords, 'Piece, 'MoveCommand, 'BoardEvnt
    when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, 'Piece> and 'Board : equality
    and 'Piece :> ISelfCalculatingPiece<'Board, 'Coords, 'MoveCommand, 'BoardEvnt>>
    (board: 'Board, activePlayer, moveCalcResult: MoveCalcResult<'MoveCommand, 'Coords>,
    moveCalc : MoveCalcDelegate<'Board, 'Coords, 'MoveCommand, 'BoardEvnt>, maybeZsValueCalc : Option<ZSValueCalcDelegate<'Board>>, ?previous, ?events) =
        member x.ActivePlayer = activePlayer
        member x.Board = board
        override x.Equals other =
            let castedOther = other :?> ImmutableZeroSumBoardGameSelfCalculatingPieces<'Board, 'Coords, 'Piece, 'MoveCommand, 'BoardEvnt>
            activePlayer = castedOther.ActivePlayer && board = castedOther.Board
        override x.GetHashCode () = HashCode.Combine (activePlayer, board)                
        interface IBoardGameForCompanion<'Board, 'BoardEvnt> with
            member x.get_GameBoard () = board
            member x.BoardEvents = 
                match events with
                | Some evnts ->
                    evnts
                | None ->
                    Seq.empty    
        interface IBoardGameForPieces<'Coords, 'MoveCommand> with
            member x.GetMoves coords =
                match moveCalcResult with
                | PossibleMoves possibleMoves -> 
                    possibleMoves |> Seq.filter (fun move -> move.StartField = coords)
                | _ -> Seq.empty    
        interface ImmutableGame with
            member this.NthMove moveIndex =
                match moveCalcResult with
                | PossibleMoves possibleMoves ->
                    let move = possibleMoves[moveIndex]
                    let piece = (board.Item move.StartField).Value
                    let nextBoard, movedPieceNewPosition, nextBoardEvents = piece.ApplyMove board move.StartField move.Cmd
                    let nextPlayer = activePlayer * (-1)
                    let nextMoveCalcResult, additionalNewBoardEvents = moveCalc.Invoke (nextBoard, nextPlayer, movedPieceNewPosition)
                    let allNewBoadEvents = Seq.concat [nextBoardEvents; additionalNewBoardEvents]
                    ImmutableZeroSumBoardGameSelfCalculatingPieces<'Board, 'Coords, 'Piece, 'MoveCommand, 'BoardEvnt> (nextBoard, nextPlayer,
                    nextMoveCalcResult, moveCalc, maybeZsValueCalc, this, allNewBoadEvents)
                | _ ->
                    raise (Exception "Game has already terminated.")    
            member x.Previous = previous |> Option.map (fun p -> p :> ImmutableGame)
            member x.ZSValue = 
                match moveCalcResult with
                | GameOverZSValue value ->
                    value
                | _ ->    
                    match maybeZsValueCalc with
                    | Some zsValueCalc ->
                        zsValueCalc.Invoke (board, activePlayer)
                    | None -> raise (Exception "Intermediate evaluation of game state impossible: No evaluation function assigned.")    
            member x.Value player = (x :> ImmutableGame).ZSValue * (float) player
            member x.ActivePlayer = activePlayer
            member x.NumberOfPossibleMoves = 
                match moveCalcResult with
                | PossibleMoves possibleMoves ->
                    possibleMoves.Length
                | _ -> 0    
            member x.Running = (x :> ImmutableGame).NumberOfPossibleMoves <> 0            