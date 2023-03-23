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

module Reversi.ResultMapper

open GameFramework

let resultMapper (game : ImmutableGame) =
    if game.Value 1 > 0 then
        sprintf "White has %A pieces more on the board than black: White wins." (int (game.Value 1))
    elif game.Value -1 > 0 then
        sprintf "Black has %A pieces more on the board than white: Black wins." (int (game.Value -1))
    else
        "Tie: Black and white have the same amount of pieces on the board."    