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
using System.Collections.Generic;
using Euclid2D;
using GameFramework;

public class DecoratedButton : Gtk.Button
{
     public Euclid2DCoords Position {get;}
     public DecoratedButton (Euclid2DCoords position) : base ()
     {
          Position = position;         
     }    
}

public abstract class TwoDBoardGUI<Board, Piece>
where Board : Enumerable2DArray.IEnumerable2DArray<Piece> 
where Piece : IPiece 
{
     abstract protected Gtk.Builder builder {get;}     
     abstract protected void OnClick (Object? sender, EventArgs args);
     private Dictionary<String, String> imageNamesToFilenames = new Dictionary<String, String> ();
     protected DecoratedButton [,]? fields;
     protected void setFieldToPieceImage (Euclid2DCoords coords, IPiece piece)
     {
          var imageName = piece.Kind + piece.Player.ToString ();
          var image = new Gtk.Image (imageNamesToFilenames[imageName]);
          var field = fields![coords.X,coords.Y];
          field.Image = image;
     }     
     protected void SetLabel (int index, String text)
     {
          var label = (Gtk.Label) builder.GetObject ("GameInformLabel" + index.ToString ());
          label.Text = text;
     }
     protected virtual void OnGameOver (string result) => SetLabel (1, result);
     protected virtual void OnOwnPlayersTurn (int activePlayer) => SetLabel (1, $"Player {activePlayer}: It's your turn. Please make a move.");     
     protected virtual void OnAIMessage (Object sender, String message) => SetLabel (2, message);
     protected IEnumerable<int>? ThisGUIusers;
     protected Board? board;
     protected IGameInformer<String>? game;
     protected virtual void initialize (int windowsWidth, int windowHeight, String pictureFolder, Board _board, 
     IGameInformer<String> _game, IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs)
     {
          board = _board;
          game = _game;
          game.GameOver += OnGameOver;
          ThisGUIusers = thisGUIusers;
          var imageFileNames = Directory.GetFiles (pictureFolder);
          foreach (String name in imageFileNames)
          {
               var rawName = Path.GetFileNameWithoutExtension (name);
               imageNamesToFilenames[rawName] = name;
          }
          var form = (Gtk.ApplicationWindow) builder.GetObject ("Window");
          form.WidthRequest = windowsWidth;
          form.HeightRequest = windowHeight;
          fields = new DecoratedButton [board.xDim, board.yDim];
          var grid = (Gtk.Grid) builder.GetObject ("FieldsGrid");
          for (int x = 0; x < board.xDim; x++)
          {
               for (int y = 0; y < board.yDim; y++)
               {
                    var button = new DecoratedButton (new Euclid2DCoords (x,y));
                    button.Visible = true;
                    button.Sensitive = false;
                    button.Clicked += OnClick;
                    grid.Attach (button, x, board.yDim - y - 1, 1, 1);
                    fields[x,y] = button;   
               } 
          }
          foreach (Tuple<Piece,Tuple<int,int>> entry in board.AllEntriesWithCoords)
          {
               var pos = new Euclid2DCoords (entry.Item2);
               var piece = entry.Item1;
               setFieldToPieceImage (pos, piece);
          }
          game.NextPlayer += activePlayer =>
          {
               if (ThisGUIusers!.Any (player => player == activePlayer))
                    OnOwnPlayersTurn (activePlayer);
               else 
                    SetLabel (1, $"Player {activePlayer} is planning the next move.");
          };
          foreach (AI_Informer ai in AIs)
               ai.SendMessage += OnAIMessage;
     }
     int finishedMoves = 0;
     protected void moveFinished () => finishedMoves ++;
     void OnUndoClicked (Object sender, EventArgs args)
     {
          var reversibleGame = (IReversibleGame<String>?) game;
          if (finishedMoves > 0)
          {
               reversibleGame!.Undo ();
               while (!(ThisGUIusers!.Any (player => player == game!.ActivePlayer)))
                    reversibleGame!.Undo ();
               finishedMoves --;
               OnOwnPlayersTurn (game!.ActivePlayer);
          }
     }
     void OnQuit (Object sender, EventArgs args)
     {
          Gtk.Application.Quit ();
     }     
}