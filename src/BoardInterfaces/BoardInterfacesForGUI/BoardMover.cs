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

using System;
using System.Collections.Generic;

public interface IBoardInformerEvent {}

public interface IBoardInformer<Coords>
{
    event EventHandler<IBoardInformerEvent>? BoardInformerEvent;
}

public class BoardInformerMovingEvent<Coords> : IBoardInformerEvent
{
    public Coords Start {get;}
    public Coords Dest {get;}
    public BoardInformerMovingEvent (Coords start, Coords dest)
    {
        Start = start;
        Dest = dest;
    }
}

public class BoardInformerDestroyedEvent<Coords> : IBoardInformerEvent
{
    public Coords Field {get;}
    public BoardInformerDestroyedEvent (Coords field) => Field = field;
}

public class BoardInformerTransformedEvent<Coords> : IBoardInformerEvent
{
    public Coords Field {get;}
    public IPiece TransformedTo {get;}
    public BoardInformerTransformedEvent (Coords field, IPiece transformedTo)
    {
        Field = field;
        TransformedTo = transformedTo;
    }
} 

public interface IBoardMover<Coords> : IBoardInformer<Coords>
{
    IEnumerable<Coords> PossibleMoves (Coords field);
    void MakeMove (Coords start, Coords dest);
}