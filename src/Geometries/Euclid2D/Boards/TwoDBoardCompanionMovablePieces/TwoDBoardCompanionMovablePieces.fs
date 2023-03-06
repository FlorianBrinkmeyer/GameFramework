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
open Euclid2D;
open GameFramework

type TwoDBoardCompanion<'Piece when 'Piece :> IMovablePiece<int*int>> (companion) = 
    inherit BoardCompanion<Enumerable2DArray.IEnumerable2DArray<'Piece>, int*int, IBoardMoveEvent> (companion)
    interface IBoardMover<int*int> with
        member this.PossibleMoves coords =
            let (x,y) = coords
            let maybePiece = this.board[x,y]
            match maybePiece with
            | Some piece ->
                let game = this.game :?> IBoardGameForPieces<int*int, IMoveCommand<int*int>>
                piece.PossibleMoves (game, coords)
            | None ->
                Seq.empty    
        member this.MakeMove ((startX, startY), dest) =
            let maybePiece = this.board[startX, startY]
            match maybePiece with
            | Some piece ->
                let game = this.game :?> IBoardGameForPieces<int*int, IMoveCommand<int*int>>
                let mutableGame = this.gameCompanion :?> IGameMoveMaker
                piece.MakeMove (mutableGame, game, (startX, startY), dest)
            | None ->
                raise (Exception "Field is empty: Impossible to make a move from here.")
        member this.add_BoardInformerEvent value = this.BoardInformerEvent.AddHandler value            
        member this.remove_BoardInformerEvent value = this.BoardInformerEvent.RemoveHandler value            
    interface ITwoDBoardMovablePieces<'Piece> with
        member this.ToSeqInDir (xPos, yPos) (xDir, yDir) = this.board.ToSeqInDir (xPos, yPos) (xDir, yDir)
        member this.ToSeqInDirWithCoords (xPos, yPos) (xDir, yDir) = this.board.ToSeqInDirWithCoords (xPos, yPos) (xDir, yDir)
        member this.xDim = this.board.xDim
        member this.yDim = this.board.yDim
        member this.Item (x,y) = this.board[x,y]
        member this.Item coords = this.board[coords]
        member this.To2DArray = this.board.To2DArray
        member this.AllCoords = this.board.AllCoords
        member this.ToSeqWhole = this.board.ToSeqWhole
        member this.AllEmptyCoords = this.board.AllEmptyCoords
        member this.AllEntries = this.board.AllEntries
        member this.AllEntriesWithCoords = this.board.AllEntriesWithCoords
        member this.AllUsedCoords = this.board.AllUsedCoords
        member this.FilterCoordsByBoundaries coordsSeq = this.board.FilterCoordsByBoundaries coordsSeq
        member this.CheckCoordsByBoundaries (x,y) = this.board.CheckCoordsByBoundaries (x,y)
        member this.get_Item (x,y) = this.board.get_Item (x,y)
        member this.get_Item coords = this.board.get_Item coords        