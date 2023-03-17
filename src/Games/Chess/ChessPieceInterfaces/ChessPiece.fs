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

namespace Chess

open GameFramework

type IKing<'Board, 'Coords> =
    inherit IPiece
    inherit ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent>
    ///Currend board -> piece position -> black list -> resulting board
    abstract AugmentByBlackList : 'Board (*current board*) -> 'Coords (*piece position*) -> seq<'Coords> (*black list*) -> 'Board (*resulting board*)

type INonKingChessPiece<'Board, 'Coords> =
    inherit IPiece
    inherit ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent>
    ///Current board -> piece position -> black list for king
    abstract BlackListForKing : 'Board (*current board*) -> 'Coords (*piece position*) -> seq<'Coords>
    ///Current board -> piece position -> field to check -> threatening ? Some (threat neutralizing moves) : None 
    abstract IsThreateningField : 'Board (*current board*) -> 'Coords (*piece position*) -> 'Coords (*field to check*) -> Option<seq<'Coords>>
    ///Current board -> piece position -> white list -> resulting board
    abstract AugmentByWhiteList : 'Board (*current board*) -> 'Coords (*piece position*) -> seq<'Coords> (*white list*) -> 'Board (*resulting board*)   
    ///Current board -> piece position -> black list -> resulting board
    abstract AugmentByBlackList : 'Board (*current board*) -> 'Coords (*piece position*) -> seq<'Coords> (*black list*) -> 'Board (*resulting board*)

type IBlockableChessPiece<'Board, 'Coords> =
    inherit INonKingChessPiece<'Board, 'Coords>
    ///Current board -> piece position -> position of other king -> resulting board
    abstract PotentiallyAugmentBlockingPieceByLists : 'Board (*current board*) -> 'Coords (*piece position*) -> 'Coords (*position of other king*) -> 
        'Board (*resulting board*)

type IBaseChessPiece<'Board, 'Coords> =
    inherit ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent>
    inherit IMovablePiece<'Coords>           