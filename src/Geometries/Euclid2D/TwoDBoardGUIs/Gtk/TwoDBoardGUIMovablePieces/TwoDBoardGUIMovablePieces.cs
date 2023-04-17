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

using System.Timers;

namespace Euclid2DGame;

using System;
using Euclid2D;
using GameFramework;

public abstract class TwoDBoardGUIMovablePieces : TwoDBoardGUI<ITwoDBoardMovablePieces<IPiece> , IPiece>
{
    bool startFieldAlreadyChosen = false;
    Euclid2DCoords? startField;
    override protected void OnOwnPlayersTurn (int activePlayer)
    {
        base.OnOwnPlayersTurn (activePlayer);
        foreach (Tuple<int,int> entry in GameBoard!.AllUsedCoords)
        {
            var coords = Euclid2DCoords.FromTuple (entry);
            var possibleMoves = GameBoard.PossibleMoves (entry);
            if (possibleMoves.Any ())
                Fields![coords.X, coords.Y].Sensitive = true;
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
            foreach (Tuple<int,int> entry in GameBoard!.AllCoords)
                Fields![entry.Item1,entry.Item2].Sensitive = false;
            GameBoard!.MakeMove (startField!.AsTuple, destField.AsTuple);
        } else {
            startField = position;
            startFieldAlreadyChosen = true;
            foreach (Tuple<int,int> entry in GameBoard!.AllCoords)
                Fields![entry.Item1,entry.Item2].Sensitive = false;
            foreach (Tuple<int,int> entry in GameBoard!.PossibleMoves (startField.AsTuple))
            {
                Fields![entry.Item1, entry.Item2].Sensitive = true;
            }
        }        
    }
    protected virtual void OnBoardInformerEvent (object? sender, IBoardMoveEvent evnt)
    {
        if (evnt is BoardMovingEvent<Tuple<int,int>> movingEvent)
        {
            var start = Euclid2DCoords.FromTuple (movingEvent!.Start);
            var dest = Euclid2DCoords.FromTuple (movingEvent!.Dest);
            var destPiece = GameBoard![dest];
            FieldClearImage (start);
            SetFieldToPieceImage (dest, destPiece);
            if (ThisGUIusers!.All(player => player != destPiece.Player))
            {
                var timer = new System.Timers.Timer(700);
                timer.AutoReset = false;
                timer.Elapsed += (sender, args) =>
                {
                    var shown = true;
                    var count = 0;
                    var timer2 = new System.Timers.Timer(70);
                    timer2.AutoReset = true;
                    timer2.Elapsed += (sender, args) =>
                    {
                        if (!shown)
                            SetFieldToPieceImage(dest, destPiece);
                        else
                            FieldClearImage(dest);
                        shown = !shown;
                        count++;
                        if (count == 4)
                            timer2.Stop();
                    };
                    timer2.Start();
                };
                timer.Start();
            }
        }
        if (evnt is BoardDestroyedEvent<Tuple<int,int>> destroyedEvent)
        {
            var coords = Euclid2DCoords.FromTuple (destroyedEvent!.Field);
            FieldClearImage (coords);
        }
        if (evnt is BoardTransformedEvent<Tuple<int,int>> transformedEvent)
        {
            var coords = Euclid2DCoords.FromTuple (transformedEvent!.Field);
            var transformedTo = transformedEvent.TransformedTo;
            SetFieldToPieceImage (coords, transformedTo);
        }
    }
    override protected void Initialize (int windowsWidth, int windowHeight, String pictureFolder, ITwoDBoardMovablePieces<IPiece> board, 
    IGameInformer<String> game, IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs, bool debugMode)
    {
        base.Initialize (windowsWidth, windowHeight, pictureFolder, board, game, thisGUIusers, AIs, debugMode);
        GameBoard!.BoardInformerEvent += OnBoardInformerEvent;
    }
}