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

namespace Chess;

using Euclid2DGame;
using GameFramework;

public class ChessGtkGUI<Board, Piece> : TwoDBoardGUIMovablePieces<Board, Piece>
where Board : ITwoDBoardMovablePieces<Piece> 
where Piece : IPiece 
{
    override protected Gtk.Builder builder {get;} = new Gtk.Builder ();
    protected override string PlayerToString(int id)
    {
        if (id == 1) 
            return "White";
        else
            return "Black";    
    }
    override protected void OnBoardInformerEvent (object? sender, IBoardMoveEvent evnt)
    {
        base.OnBoardInformerEvent (sender, evnt);
        if (evnt is BoardKingCheckedEvent<Tuple<int,int>>)
        {
            var kingCheckedEvent = evnt as BoardKingCheckedEvent<Tuple<int,int>>;
            var player = kingCheckedEvent!.CheckedPlayer;
            String labelText;
            if (player == 1)
                labelText = "White king checked.";
            else
                labelText = "Black king checked.";            
            SetLabel (2, labelText);
        }
        if (evnt is BoardCastlingEvent<Tuple<int,int>>)
        {
            var castlingEvent = evnt as BoardCastlingEvent<Tuple<int,int>>;
            var moveEvent1 = new BoardMovingEvent<Tuple<int,int>> (castlingEvent!.KingStartPos, castlingEvent.KingDestPos);
            var moveEvent2 = new BoardMovingEvent<Tuple<int,int>> (castlingEvent.RookStartPos, castlingEvent.KingDestPos);
            OnBoardInformerEvent (sender, moveEvent1);
            OnBoardInformerEvent (sender, moveEvent2);
        }
    }
    public ChessGtkGUI (int windowsWidth, int windowHeight, String pictureFolder, String guiFilename, Board _board, 
    IGameInformer<String> _game, int [] thisGUIusers, IEnumerable<AI_Informer> AIs)
    {
        Gtk.Application.Init ();
        builder.AddFromFile (guiFilename);
        builder.Autoconnect (this);
        initialize (windowsWidth, windowHeight, pictureFolder, _board, _game, thisGUIusers, AIs);
    }
}