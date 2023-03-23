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

module Chess.ResultMapper

open GameFramework
open System

let resultMapper (game : ImmutableGame) =
    if game.Value 1 = Double.PositiveInfinity then
        "White has checkmated black: White wins."
    elif game.Value -1 = Double.PositiveInfinity then
        "Black has checkmated white: Black wins."
    else
        if game.ActivePlayer = 1 then
            "White isn't checked, but unable to make a legal move: Tie."
        else
            "Black isn't checked, but unable to make a legal move: Tie."