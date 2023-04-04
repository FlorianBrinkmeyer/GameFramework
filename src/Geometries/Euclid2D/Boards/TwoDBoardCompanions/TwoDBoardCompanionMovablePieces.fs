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

namespace Euclid2DGame

open System;
open GameFramework

type TwoDBoardCompanionMovablePieces (companion) = 
    inherit TwoDBoardCompanion<IPiece, IBoardMoveEvent> (companion)    
    interface IBoardMover<int*int> with
        member this.PossibleMoves coords =
            let (x,y) = coords
            let maybePiece = this.GameBoard[x,y]
            match maybePiece with
            | Some piece ->
                let game = this.Game :?> IBoardGameForPieces<int*int, IMoveCommand<int*int>>
                (piece :?> IMovablePiece<int*int>).PossibleMoves (game, coords)
            | None ->
                Seq.empty    
        member this.MakeMove ((startX, startY), dest) =
            let maybePiece = this.GameBoard[startX, startY]
            match maybePiece with
            | Some piece ->
                let game = this.Game :?> IBoardGameForPieces<int*int, IMoveCommand<int*int>>
                let mutableGame = this.GameCompanion :?> IGameMoveMaker
                (piece :?> IMovablePiece<int*int>).MakeMove (mutableGame, game, (startX, startY), dest)
            | None ->
                raise (Exception "Field is empty: Impossible to make a move from here.")
    interface ITwoDBoardMovablePieces<IPiece>