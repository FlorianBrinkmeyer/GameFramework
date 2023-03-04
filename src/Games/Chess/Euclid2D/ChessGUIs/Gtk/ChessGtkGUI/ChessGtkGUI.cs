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
    public ChessGtkGUI (int windowsWidth, int windowHeight, String pictureFolder, String guiFilename, Board _board, 
    IGameInformer<String> _game, int [] thisGUIusers, IEnumerable<AI_Informer> AIs)
    {
        Gtk.Application.Init ();
        builder.AddFromFile (guiFilename);
        builder.Autoconnect (this);
        initialize (windowsWidth, windowHeight, pictureFolder, _board, _game, thisGUIusers, AIs);
    }
    override protected void OnBoardInformerEvent (object? sender, IBoardInformerEvent evnt)
    {
        base.OnBoardInformerEvent (sender, evnt);
        if (evnt is BoardInformerKingCheckedEvent<Tuple<int,int>>)
        {
            var kingCheckedEvent = evnt as BoardInformerKingCheckedEvent<Tuple<int,int>>;
            var player = kingCheckedEvent!.CheckedPlayer;
            var label = (Gtk.Label) builder.GetObject ("AdditionalGameInformLabel");
            if (player == 1)
                label.Text = "White king checked.";
            else
                label.Text = "Black king checked.";            
        }
    }
}
