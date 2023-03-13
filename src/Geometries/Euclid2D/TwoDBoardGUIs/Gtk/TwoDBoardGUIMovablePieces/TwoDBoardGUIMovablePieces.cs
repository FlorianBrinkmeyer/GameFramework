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

namespace Euclid2DGame;

using System;
using Euclid2D;
using GameFramework;

public abstract class TwoDBoardGUIMovablePieces<Board, Piece> : TwoDBoardGUI<Board, Piece>
where Board : ITwoDBoardMovablePieces<Piece> 
where Piece : IPiece 
{
    bool startFieldAlreadyChosen = false;
    Euclid2DCoords? startField;
    override protected void OnOwnPlayersTurn (int activePlayer)
    {
        base.OnOwnPlayersTurn (activePlayer);
        foreach (Tuple<int,int> entry in board!.AllUsedCoords)
        {
            var coords = new Euclid2DCoords (entry);
            var possibleMoves = board.PossibleMoves (entry);
            if (possibleMoves.Any ())
                fields![coords.X, coords.Y].Sensitive = true;
        }
        startFieldAlreadyChosen = false;     
    }
    override protected void OnClick (Object? sender, EventArgs args)
    {
        var button = (DecoratedButton?) sender;
        var position = button!.Position;
        if (startFieldAlreadyChosen)
        {
            var destField = position;
            foreach (Tuple<int,int> entry in board!.AllCoords)
                fields![entry.Item1,entry.Item2].Sensitive = false;
            moveFinished ();    
            board!.MakeMove (startField!.AsTuple, destField.AsTuple);
        } else {
            startField = position;
            startFieldAlreadyChosen = true;
            foreach (Tuple<int,int> entry in board!.AllCoords)
                fields![entry.Item1,entry.Item2].Sensitive = false;
            foreach (Tuple<int,int> entry in board!.PossibleMoves (startField.AsTuple))
            {
                fields![entry.Item1, entry.Item2].Sensitive = true;
            }
        }        
    }
    protected virtual void OnBoardInformerEvent (object? sender, IBoardMoveEvent evnt)
    {
        if (evnt is BoardMovingEvent<Tuple<int,int>>)
        {
            var movingEvent = evnt as BoardMovingEvent<Tuple<int,int>>;
            var start = new Euclid2DCoords (movingEvent!.Start);
            var dest = new Euclid2DCoords (movingEvent!.Dest);
            var destPiece = board![dest];
            fields![start.X,start.Y].Image = null;
            setFieldToPieceImage (dest, destPiece); 
        }
        if (evnt is BoardDestroyedEvent<Tuple<int,int>>)
        {
            var destroyedEvent = evnt as BoardDestroyedEvent<Tuple<int,int>>;
            var coords = new Euclid2DCoords (destroyedEvent!.Field);
            fields![coords.X,coords.Y].Image = null;
        }
        if (evnt is BoardTransformedEvent<Tuple<int,int>>)
        {
            var transformedEvent = evnt as BoardTransformedEvent<Tuple<int,int>>;
            var coords = new Euclid2DCoords (transformedEvent!.Field);
            var transformedTo = transformedEvent.TransformedTo;
            setFieldToPieceImage (coords, (Piece) transformedTo);
        }
    }
    override protected void initialize (int windowsWidth, int windowHeight, String pictureFolder, Board _board, 
    IGameInformer<String> game, IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs)
    {
        base.initialize (windowsWidth, windowHeight, pictureFolder, _board, game, thisGUIusers, AIs);
        board!.BoardInformerEvent += OnBoardInformerEvent;
    }
}