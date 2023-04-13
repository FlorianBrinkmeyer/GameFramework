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

using System.Runtime.Loader;

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

public abstract class TwoDBoardGUI<Board, Piece> : IGui
where Board : Enumerable2DArray.IEnumerable2DArray<Piece> 
{
     abstract protected Gtk.Builder Builder {get;}     
     abstract protected void OnClick (Object? sender, EventArgs args);
     private Dictionary<String, String> imageNamesToFilenames = new Dictionary<String, String> ();
     protected DecoratedButton [,]? Fields;
     protected virtual String getImageFileName (Piece piece)
     {
          var ipiece = (IPiece?) piece;
          return ipiece!.Kind + ipiece.Player.ToString ();
     } 
     protected void SetFieldToPieceImage (Euclid2DCoords coords, Piece piece)
     {
          var imageName = getImageFileName (piece);
          Gtk.Application.Invoke ((sender, args) => {
               var image = new Gtk.Image (imageNamesToFilenames[imageName]);
               var field = Fields![coords.X,coords.Y];
               field.Image = image;
          });        
     }     
     protected void FieldClearImage (Euclid2DCoords coords)
     {
          Gtk.Application.Invoke ((sender, args) => {
               Fields![coords.X,coords.Y].Image = null;          
          });
     }
     protected void SetLabel (int index, String text)
     {
          Gtk.Application.Invoke ((sender, args) => {
               var label = (Gtk.Label) Builder.GetObject ("GameInformLabel" + index.ToString ());
               label.Text = text;
          });     
     }
     protected virtual String PlayerToString (int id) => "Player " + id.ToString ();
     protected virtual void OnGameOver (string result) => SetLabel (1, result);
     protected virtual void OnOwnPlayersTurn (int activePlayer) => SetLabel (1, PlayerToString (activePlayer) + ": It's your turn. Please make a move.");     
     protected virtual void OnAIMessage (Object sender, String message) => SetLabel (2, message);
     protected IEnumerable<int>? ThisGUIusers;
     protected Board? GameBoard;
     protected IGameInformer<String>? Game;
     protected bool DebugMode;
     protected void ReInitializeFields()
     {
          foreach (Tuple<Piece,Tuple<int,int>> entry in GameBoard!.AllEntriesWithCoords)
          {
               var pos = Euclid2DCoords.FromTuple (entry.Item2);
               var piece = entry.Item1;
               SetFieldToPieceImage (pos, piece);
          }
          foreach (Tuple<int, int> entry in GameBoard.AllEmptyCoords)
          {
               var pos = Euclid2DCoords.FromTuple(entry);
               FieldClearImage (pos);
          }
     }
     Gtk.Button? previousPlayerButton = null;
     public bool NextMoveLoaded {get => previousPlayerButton != null ? previousPlayerButton.Sensitive : false;}
     public void ReInitializeAIs (IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs)
     {
          foreach (AI_Informer ai in AIs)
               ai.SendMessage += OnAIMessage;
          ThisGUIusers = thisGUIusers;
     }
     protected virtual void Initialize (int windowsWidth, int windowHeight, String pictureFolder, Board board, 
     IGameInformer<String> game, IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs, bool debugMode)
     {
          DebugMode = debugMode;
          GameBoard = board;
          Game = game;
          Game.GameOver += OnGameOver;
          var imageFileNames = Directory.GetFiles (pictureFolder);
          foreach (String name in imageFileNames)
          {
               var rawName = Path.GetFileNameWithoutExtension (name);
               imageNamesToFilenames[rawName] = name;
          }
          var form = (Gtk.ApplicationWindow) Builder.GetObject ("Window");
          form.WidthRequest = windowsWidth;
          form.HeightRequest = windowHeight;
          Fields = new DecoratedButton [GameBoard.xDim, GameBoard.yDim];
          var grid = (Gtk.Grid) Builder.GetObject ("FieldsGrid");
          for (int x = 0; x < GameBoard.xDim; x++)
          {
               for (int y = 0; y < GameBoard.yDim; y++)
               {
                    var button = new DecoratedButton (new Euclid2DCoords (x,y));
                    button.Visible = true;
                    button.Sensitive = false;
                    button.Clicked += OnClick;
                    grid.Attach (button, x, board.yDim - y - 1, 1, 1);
                    Fields[x,y] = button;   
               } 
          }
          foreach (Tuple<Piece,Tuple<int,int>> entry in GameBoard.AllEntriesWithCoords)
          {
               var pos = Euclid2DCoords.FromTuple (entry.Item2);
               var piece = entry.Item1;
               SetFieldToPieceImage (pos, piece);
          }
          game.NextPlayer += (activePlayer) => {
               if (ThisGUIusers!.Any (player => player == activePlayer))
                    Gtk.Application.Invoke ((sender, args) => {
                         OnOwnPlayersTurn (activePlayer);
                    });     
               else 
                    SetLabel (1, PlayerToString (activePlayer) + " is planning the next move.");
          };
          if (game is IPausableGame<String> pausableGame)
          {
               pausableGame.PauseMoveDelivered += (sender, args) =>
               {
                    Gtk.Application.Invoke((sender, args) =>
                    {
                         var singleStepButton = (Gtk.Button) Builder.GetObject("SingleStepButton");
                         singleStepButton.Sensitive = true;
                         var previousButton = (Gtk.Button) Builder.GetObject("PreviousButton");
                         previousButton.Sensitive = true;
                         previousPlayerButton!.Sensitive = true;
                    });
               };
          }
          if (game is IReversibleGame<String> reversibleGame)
               reversibleGame.Undone += (sender, args) => ReInitializeFields ();
          previousPlayerButton = (Gtk.Button) Builder.GetObject("PreviousPlayerButton");
          ReInitializeAIs (thisGUIusers, AIs);
     }
     void pauseGUI ()
     {
          var pauseImage = (Gtk.Image) Builder.GetObject("PlayImage");
          var pausePlayButton = (Gtk.Button)Builder.GetObject("PausePlayButton");
          pausePlayButton.Image = pauseImage;
          for (int x = 0; x < GameBoard!.xDim; x++)
          {
               for (int y = 0; y < GameBoard.yDim; y++)
               {
                    Fields![x,y].Sensitive = false;   
               } 
          }
     }
     void OnPausePlayClicked (Object sender, EventArgs args)
     {
          if (Game is IPausableGame<String> reversibleGame)
          {
               if (reversibleGame.Running)
               {
                    pauseGUI();
                    reversibleGame.Pause ();
               } else {
                    var playImage = (Gtk.Image)Builder.GetObject("PauseImage");
                    (sender as Gtk.Button)!.Image = playImage;
                    var singleStepButton = (Gtk.Button) Builder.GetObject("SingleStepButton");
                    singleStepButton.Sensitive = false;                         
                    var previousButton = (Gtk.Button) Builder.GetObject("PreviousButton");
                    previousButton.Sensitive = false;
                    previousPlayerButton!.Sensitive = false;
                    reversibleGame.Continue ();
               }
          }
     }
     void OnPreviousClicked (Object sender, EventArgs args)
     {
          if ((Game is IReversibleGame<String> reversibleGame) && (reversibleGame.Undoable))
          {
               pauseGUI();
               reversibleGame.Undo();
               SetLabel (1, $"{PlayerToString(reversibleGame.ActivePlayer)}'s turn.");
          }
     }
     void OnPreviousPlayerClicked(Object sender, EventArgs args)
     {
          if (Game is IReversibleGame<String> reversibleGame)
          {
               pauseGUI();
               while (!(ThisGUIusers!.Any(user => user == reversibleGame.ActivePlayer)) && reversibleGame.Undoable)
               {
                    reversibleGame.Undo();
                    SetLabel (1, $"{PlayerToString(reversibleGame.ActivePlayer)}'s turn.");
               }
          }
     }
     void OnSingleStepClicked(Object sender, EventArgs args)
     {
          if (Game is IPausableGame<String> pausableGame)
          {
               pausableGame.SingleStep ();
               (sender as Gtk.Button)!.Sensitive = false;
               var singleStepButton = (Gtk.Button) Builder.GetObject("SingleStepButton");
               singleStepButton.Sensitive = false;
               var previousButton = (Gtk.Button) Builder.GetObject("PreviousButton");
               previousButton.Sensitive = false;
               previousPlayerButton!.Sensitive = false;
          }
     }
     public event EventHandler? Quit;
     void OnQuit (Object sender, EventArgs args)
     {
          var mainForm = (Gtk.ApplicationWindow) Builder.GetObject ("Window");
          mainForm.Dispose ();
          Quit?.Invoke (this, new EventArgs ());
     }     
}