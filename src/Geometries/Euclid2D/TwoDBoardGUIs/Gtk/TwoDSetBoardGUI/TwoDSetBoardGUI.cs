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

public abstract class TwoDSetBoardGUI<Piece> : TwoDBoardGUI<ITwoDSetBoard<Piece>, Piece>
{
    override protected void OnOwnPlayersTurn (int activePlayer)
    {
        base.OnOwnPlayersTurn (activePlayer);
        foreach (Tuple<int,int> coords in board!.PossibleMoves)
        {
            var position = Euclid2DCoords.FromTuple (coords);
            fields![position.X, position.Y].Sensitive = true;
        }
    }
    override protected void OnClick (Object? sender, EventArgs args)
    {
        var button = (DecoratedButton?) sender;
        var position = button!.Position;
        foreach (Tuple<int,int> entry in board!.AllCoords)
        {
            fields![entry.Item1,entry.Item2].Sensitive = false;
        }
        board!.MakeMove (position.AsTuple);
    }
    protected virtual void OnBoardInformerEvent (object? sender, BoardSetEvent<Tuple<int,int>> evnt)
    {
        var position = Euclid2DCoords.FromTuple (evnt.Field);
        setFieldToPieceImage (position, board![position]); 
    }
    override protected void initialize (int windowsWidth, int windowHeight, String pictureFolder, ITwoDSetBoard<Piece> _board, 
    IGameInformer<String> game, IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs)
    {
        base.initialize (windowsWidth, windowHeight, pictureFolder, _board, game, thisGUIusers, AIs);
        board!.BoardInformerEvent += OnBoardInformerEvent;
    }
}