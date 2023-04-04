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
    override this.ToString () = this.GameBoard.ToString ()
    interface IBoardInformer<'Evnt>
    interface IEnumerable2DArray<'Piece> with
        member this.ToSeqInDir (xPos, yPos) (xDir, yDir) = this.GameBoard.ToSeqInDir (xPos, yPos) (xDir, yDir)
        member this.ToSeqInDirWithCoords (xPos, yPos) (xDir, yDir) = this.GameBoard.ToSeqInDirWithCoords (xPos, yPos) (xDir, yDir)
        member this.xDim = this.GameBoard.xDim
        member this.yDim = this.GameBoard.yDim
        member this.Item (x,y) = this.GameBoard[x,y]
        member this.Item coords = this.GameBoard[coords]
        member this.To2DArray = this.GameBoard.To2DArray
        member this.AllCoords = this.GameBoard.AllCoords
        member this.ToSeqWhole = this.GameBoard.ToSeqWhole
        member this.AllEmptyCoords = this.GameBoard.AllEmptyCoords
        member this.AllEntries = this.GameBoard.AllEntries
        member this.AllEntriesWithCoords = this.GameBoard.AllEntriesWithCoords
        member this.AllUsedCoords = this.GameBoard.AllUsedCoords
        member this.FilterCoordsByBoundaries coordsSeq = this.GameBoard.FilterCoordsByBoundaries coordsSeq
        member this.CheckCoordsByBoundaries (x,y) = this.GameBoard.CheckCoordsByBoundaries (x,y)
        member this.get_Item (x,y) = this.GameBoard.get_Item (x,y)
        member this.get_Item (coords : Tuple<int,int>) = this.GameBoard.get_Item coords        
        member this.get_Item (coords : Euclid2DCoords) = this.GameBoard.get_Item coords        