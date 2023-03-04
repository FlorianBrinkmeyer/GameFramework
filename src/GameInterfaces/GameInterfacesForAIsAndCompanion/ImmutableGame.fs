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

type IGame =
    abstract Value: (*player*) int -> float
    abstract ZSValue: float
    abstract Running: bool
    abstract ActivePlayer: int
    abstract NumberOfPossibleMoves: int     

type ImmutableGame =
    inherit IGame
    abstract NthMove: int -> ImmutableGame
    abstract Previous: Option<ImmutableGame>