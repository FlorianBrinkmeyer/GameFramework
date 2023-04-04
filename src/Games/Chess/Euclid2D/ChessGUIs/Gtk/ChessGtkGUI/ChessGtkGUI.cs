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

using Euclid2D;

namespace Chess;

using Euclid2DGame;
using GameFramework;
using Euclid2D;

public class ChessGtkGUI : TwoDBoardGUIMovablePieces
{
    override protected Gtk.Builder Builder {get;} = new Gtk.Builder ();
    protected override string PlayerToString(int id)
    {
        if (id == 1) 
            return "White";
        else
            return "Black";    
    }
    override protected void OnBoardInformerEvent (object? sender, IBoardMoveEvent evnt)
    {
        if (DebugMode && (evnt is BoardMovingEvent<Tuple<int, int>> movingEvent))
        {
            Console.WriteLine ($"From: {movingEvent!.Start}  To: {movingEvent!.Dest}");
            
        }
        base.OnBoardInformerEvent (sender, evnt);
        if (evnt is BoardKingCheckedEvent<Tuple<int,int>> kingCheckedEvent)
        {
            var player = kingCheckedEvent!.CheckedPlayer;
            String labelText;
            if (player == 1)
                labelText = "White king checked.";
            else
                labelText = "Black king checked.";            
            SetLabel (3, labelText);
        }
        if (evnt is BoardCastlingEvent<Tuple<int,int>> castlingEvent)
        {
            var moveEvent1 = new BoardMovingEvent<Tuple<int,int>> (castlingEvent!.KingStartPos, castlingEvent.KingDestPos);
            var moveEvent2 = new BoardMovingEvent<Tuple<int,int>> (castlingEvent.RookStartPos, castlingEvent.RookDestPos);
            OnBoardInformerEvent (sender, moveEvent1);
            OnBoardInformerEvent (sender, moveEvent2);
        }
    }
    public ChessGtkGUI (int windowsWidth, int windowHeight, String pictureFolder, String guiFilename, ITwoDBoardMovablePieces<IPiece> board, 
    IGameInformer<String> game, IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs, bool debugMode)
    {
        DebugMode = debugMode;
        Gtk.Application.Init ();
        Builder.AddFromFile (guiFilename);
        Builder.Autoconnect (this);
        var mainForm = (Gtk.ApplicationWindow) Builder.GetObject ("Window");
        mainForm.Title = "Chess";
        mainForm.WindowPosition = Gtk.WindowPosition.Center;
        Initialize (windowsWidth, windowHeight, pictureFolder, board, game, thisGUIusers, AIs, debugMode);
        Game!.MoveMade += (sender, move) => {
            SetLabel (3, String.Empty);
        };
    }
}