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

type NegaMax (player : int, searchDepth : int) =
    interface AI_Agent with
        member x.Player = player
        member x.MakeMove mutableGame game =
            let state = game :?> ImmutableGame
            let rec helper step (state : ImmutableGame) =
                let numberOfPossibleMoves = state.NumberOfPossibleMoves
                if (step = 0) || (numberOfPossibleMoves = 0)  then
                    state.ZSValue * (state.ActivePlayer |> float)  
                else 
                    [0..(numberOfPossibleMoves-1)] |> List.map (fun move -> -(helper (step-1) (state.NthMove move))) |> List.max
            let chosenMove = [0..(state.NumberOfPossibleMoves-1)] |> List.maxBy (fun move -> -(helper (searchDepth-1) (state.NthMove move)))
            mutableGame.MakeMove chosenMove