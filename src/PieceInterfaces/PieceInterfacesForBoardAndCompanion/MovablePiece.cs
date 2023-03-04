/*
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
*/

namespace GameFramework;

using System.Collections.Generic;

public interface IMoveCommand<Coords>
{
    Coords Dest {get;}
}

public interface IMovablePiece<Coords> : IPiece
{
    IEnumerable<Coords> PossibleMoves (IBoardGameForPieces<Coords, IMoveCommand<Coords>> game, Coords startField);
    void MakeMove (IGameMoveMaker mutableGame, IBoardGameForPieces<Coords, IMoveCommand<Coords>> game, Coords start, Coords dest);
}
