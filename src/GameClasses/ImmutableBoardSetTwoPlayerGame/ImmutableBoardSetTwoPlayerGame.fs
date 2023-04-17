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

///Chosen move * Current board * active player * optional state -> next board * board events * next optional state
type MakeMoveDelegate<'Board, 'MoveCommand, 'BoardEvnt, 'State> = 
    Func<'MoveCommand, 'Board, int, 'State, 'Board * seq<'BoardEvnt> * 'State>

///Current board * previous player * optional state -> active player * possible moves | GameOver
type CalculatePossibleMovesDelegate<'Board, 'MoveCommand, 'State> =
    Func<'Board, int, 'State, int * MoveCalcResult<'MoveCommand>>    

///Current board -> zero-sum value
type ZSValueCalcDelegate<'Board> = Func<'Board, float>

type ImmutableBoardSetGame<'Board, 'MoveCommand, 'BoardEvnt, 'State when 'Board : equality>
    (board: 'Board, activePlayer, moveCalcResult : MoveCalcResult<'MoveCommand>, makeMove : MakeMoveDelegate<'Board, 'MoveCommand, 'BoardEvnt, 'State>,
    calculatePossibleMoves : CalculatePossibleMovesDelegate<'Board, 'MoveCommand, 'State>, zsValueCalc : ZSValueCalcDelegate<'Board>,
    state : 'State, maybeDeviantingIntermediateEvaluation: Option<ZSValueCalcDelegate<'Board>>, ?previous, ?events) =
        let mutable maybeCachedValue = None
        member x.ActivePlayer = activePlayer
        member x.Board = board
        member x.PossibleMoves =
            match moveCalcResult with
            | PossibleMoves moves ->
                moves
            | GameOver -> raise (InvalidOperationException "Making another move impossible: Game has already terminated.")    
        override x.Equals other =
            let castedOther = other :?> ImmutableBoardSetGame<'Board, 'MoveCommand, 'BoardEvnt, 'State>
            activePlayer = castedOther.ActivePlayer && board = castedOther.Board
        override x.GetHashCode () = HashCode.Combine (activePlayer, board)                
        override x.ToString () =
            let activePlayer = sprintf $"ActivePlayer: {activePlayer} \n\n"
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
        interface IGameForBoardOrCompanion<'MoveCommand> with
            member x.PossibleMoves = x.PossibleMoves
        interface ImmutableGame with
            member this.NthMove moveIndex =
                if (this :> ImmutableGame).Running then
                    try
                        let move = this.PossibleMoves[moveIndex]
                        let nextBoard, boardEvents, nextState = makeMove.Invoke (move, board, activePlayer, state)
                        let nextPlayer, nextMoveCalcResult = calculatePossibleMoves.Invoke (nextBoard, activePlayer, nextState)
                        ImmutableBoardSetGame<'Board, 'MoveCommand, 'BoardEvnt, 'State> (nextBoard, nextPlayer, nextMoveCalcResult, makeMove,
                        calculatePossibleMoves, zsValueCalc, nextState, maybeDeviantingIntermediateEvaluation, this, boardEvents)
                    with
                        | ex ->
                            raise (InvalidGameStateException (ex.Message, this))
                else
                    raise (InvalidOperationException "Game has already terminated.")    
            member x.Previous = previous |> Option.map (fun p -> p :> ImmutableGame)
            member x.ZSValue = 
                match maybeCachedValue with
                | Some value ->
                    value
                | None ->    
                    let result =
                        match (x :> ImmutableGame).Running, maybeDeviantingIntermediateEvaluation with
                        | true, Some deviantEval ->
                            (deviantEval.Invoke board) * (float) activePlayer
                        | _ -> 
                            (zsValueCalc.Invoke board) * (float) activePlayer
                    maybeCachedValue <- Some result
                    result
            member x.Value player = (x :> ImmutableGame).ZSValue * (float) (activePlayer * player)
            member x.ActivePlayer = activePlayer
            member x.NumberOfPossibleMoves = 
                match moveCalcResult with
                | PossibleMoves possibleMoves ->
                    possibleMoves.Length
                | GameOver ->
                    0    
            member x.Running = (x :> ImmutableGame).NumberOfPossibleMoves <> 0
            member x.InstableState = false