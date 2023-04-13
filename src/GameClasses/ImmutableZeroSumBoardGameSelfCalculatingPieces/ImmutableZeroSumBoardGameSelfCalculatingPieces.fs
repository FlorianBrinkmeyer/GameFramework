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

///Current board * active player * optional state * new position of last moved piece -> possible moves / game result * board events * new optional state * game state is "unstable"
type MoveCalcDelegate<'Board, 'Coords, 'MoveCommand, 'BoardEvnt, 'State> = 
    Func<'Board, int, 'State, Option<'Coords>, MoveCalcResult<'MoveCommand, 'Coords> * seq<'BoardEvnt> * 'State * bool> 

///Current board -> zero-sum value
type ZSValueCalcDelegate<'Board> = Func<'Board, float>

type ImmutableZeroSumBoardGameSelfCalculatingPieces<'Board, 'Coords, 'MoveCommand, 'BoardEvnt, 'State
    when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IPiece> and 'Board : equality>
    (board: 'Board, activePlayer, moveCalcResult: MoveCalcResult<'MoveCommand, 'Coords>,
    moveCalc : MoveCalcDelegate<'Board, 'Coords, 'MoveCommand, 'BoardEvnt, 'State>, maybeZsValueCalc : Option<ZSValueCalcDelegate<'Board>>, 
    state : 'State, unstableGameState, ?previous, ?events) =
        member x.ActivePlayer = activePlayer
        member x.Board = board
        override x.Equals other =
            let castedOther = other :?> ImmutableZeroSumBoardGameSelfCalculatingPieces<'Board, 'Coords, 'MoveCommand, 'BoardEvnt, 'State>
            activePlayer = castedOther.ActivePlayer && board = castedOther.Board
        override x.GetHashCode () = HashCode.Combine (activePlayer, board)                
        override x.ToString () =
            let activePlayer = sprintf $"ActivePlayer: {activePlayer} \n"
            let rest = (board.ToString ()).Split ('\n') |> Seq.filter (not << String.IsNullOrEmpty) |> Seq.rev |> Seq.reduce (fun str1 str2 -> str1 + "\n" + str2)
            activePlayer + rest
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
                    try
                        let move = possibleMoves[moveIndex]
                        let piece = (board.Item move.StartField).Value :?> ISelfCalculatingPiece<'Board, 'Coords, 'MoveCommand, 'BoardEvnt>
                        let nextBoard, movedPieceNewPosition, nextBoardEvents = piece.ApplyMove board move.StartField move.Cmd
                        let nextPlayer = activePlayer * (-1)
                        let nextMoveCalcResult, additionalNextBoardEvents, nextState, gameStateIsUnstable = 
                            moveCalc.Invoke (nextBoard, nextPlayer, state, Some movedPieceNewPosition)
                        let allNewBoadEvents = Seq.append nextBoardEvents additionalNextBoardEvents
                        ImmutableZeroSumBoardGameSelfCalculatingPieces<'Board, 'Coords, 'MoveCommand, 'BoardEvnt, 'State> (nextBoard, nextPlayer,
                        nextMoveCalcResult, moveCalc, maybeZsValueCalc, nextState, gameStateIsUnstable, this, allNewBoadEvents)
                    with
                        | ex ->
                            raise (InvalidGameStateException (ex.Message, this))
                | _ ->
                    raise (InvalidOperationException "Game has already terminated.")    
            member x.Previous = previous |> Option.map (fun p -> p :> ImmutableGame)
            member x.ZSValue = 
                match moveCalcResult with
                | GameOverZSValue value ->
                    value * (float) activePlayer
                | _ ->    
                    match maybeZsValueCalc with
                    | Some zsValueCalc ->
                        (zsValueCalc.Invoke board) * (float) activePlayer
                    | None -> raise (InvalidOperationException "Intermediate evaluation of game state impossible: No evaluation function assigned.")    
            member x.Value player = (x :> ImmutableGame).ZSValue * (float) (activePlayer * player)
            member x.ActivePlayer = activePlayer
            member x.NumberOfPossibleMoves = 
                match moveCalcResult with
                | PossibleMoves possibleMoves ->
                    possibleMoves.Length
                | _ -> 0    
            member x.Running = (x :> ImmutableGame).NumberOfPossibleMoves <> 0
            member x.InstableState = unstableGameState      