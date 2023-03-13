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

type TwoDSetBoardCompanion<'Piece> (companion) = 
    inherit TwoDBoardCompanion<'Piece, BoardSetEvent<int*int>> (companion)    
    interface IBoardSetter<int*int> with
        member this.PossibleMoves = (this.game :?> IGameForBoardOrCompanion<int*int>).PossibleMoves
        member this.MakeMove field =
            let possibleMoves = (this :> IBoardSetter<int*int>).PossibleMoves
            let index = possibleMoves |> Seq.findIndex (fun t -> t = field)
            (this.gameCompanion :?> IGameMoveMaker).MakeMove index
    interface ITwoDSetBoard<'Piece>