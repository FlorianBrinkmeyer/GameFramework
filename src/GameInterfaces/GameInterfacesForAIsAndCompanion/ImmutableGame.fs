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
    ///Can be used to signal the AI that the current game state needs special attention. (In order to increase search depth, for instance.)
    abstract InstableState : bool

type InvalidGameStateException (message, state : ImmutableGame) =
    inherit InvalidOperationException (message)     
    do
        let rec printStates step (state : ImmutableGame) =
            state.Previous |> Option.iter (fun st ->
                printStates (step + 1) st
                let name = (String.replicate step "previous ") + "board:"
                Console.WriteLine () 
                Console.WriteLine name
                Console.WriteLine () 
                Console.WriteLine st
                Console.WriteLine () 
            )    
        printStates 0 state               