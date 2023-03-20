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
open Euclid2D.Enumerable2DArray
open GameFramework

[<AbstractClass>]
type TwoDBoardCompanion<'Piece, 'Evnt> (companion) = 
    inherit BoardCompanion<IEnumerable2DArray<'Piece>, 'Evnt> (companion)
    interface IBoardInformer<'Evnt>
    interface IEnumerable2DArray<'Piece> with
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
        member this.get_Item (coords : Tuple<int,int>) = this.board.get_Item coords        
        member this.get_Item (coords : Euclid2DCoords) = this.board.get_Item coords        