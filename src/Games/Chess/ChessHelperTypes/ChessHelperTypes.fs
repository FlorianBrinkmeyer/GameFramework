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

type CastlingInfo<'Coords> = {KingDestPos : 'Coords; RookStartPos : 'Coords; RookDestPos : 'Coords; FieldsInBetween : seq<'Coords>}

type EnPassantInfo<'Coords> = {PossibleOtherPawnPos : 'Coords; PossibleOwnDestPos : 'Coords}

type MoveCommand<'Coords> (dest : 'Coords) =
    interface IMoveCommand<'Coords> with
        member x.Dest = dest

type CastlingCommand<'Coords> (info : CastlingInfo<'Coords>) =
    member x.Info = info
    interface IMoveCommand<'Coords> with
        member x.Dest = info.KingDestPos

type PawnEnPassantCaptureCommand<'Coords> (info : EnPassantInfo<'Coords>) =
    member x.Info = info
    interface IMoveCommand<'Coords> with
        member x.Dest = info.PossibleOwnDestPos

type BoardKingCheckedEvent<'Coords> = 
    {CheckedPlayer : int; KingPos : 'Coords; CheckedBy : seq<'Coords>}
    interface IBoardMoveEvent

type BoardCastlingEvent<'Coords> (kingStartPos : 'Coords, info : CastlingInfo<'Coords>) =
    member x.KingStartPos = kingStartPos
    member x.KingDestPos = info.KingDestPos
    member x.RookStartPos = info.RookStartPos
    member x.RookDestPos = info.RookDestPos
    interface IBoardMoveEvent