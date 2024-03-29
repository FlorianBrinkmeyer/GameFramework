﻿/*
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
     protected virtual void OnOwnPlayersTurn (int activePlayer) 
     {
          SetLabel (1, PlayerToString (activePlayer) + ": It's your turn. Please make a move.");
          Gtk.Application.Invoke ((sender, args) =>
          {
               previousButton!.Sensitive = true;
               var previousPlayerButton = (Gtk.Button) Builder.GetObject("PreviousPlayerButton");
               previousPlayerButton.Sensitive = true;
               var pausePlayButton = (Gtk.Button)Builder.GetObject("PausePlayButton");
               pausePlayButton.Sensitive = false;
          });
     }
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
     Gtk.Button? previousButton = null;
     public bool NextMoveLoaded {get => previousButton != null ? previousButton.Sensitive : false;}
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
          form.Resizable = true;
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
               UpdateGUI (false);
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
                    if (thisGUIusers.All (player => player != game.ActivePlayer))
                    {
                         Gtk.Application.Invoke ((sender, args) =>
                         {
                              var singleStepButton = (Gtk.Button) Builder.GetObject("SingleStepButton");
                              singleStepButton.Sensitive = true;
                              previousButton!.Sensitive = true;
                              var previousPlayerButton = (Gtk.Button) Builder.GetObject("PreviousPlayerButton");
                              previousPlayerButton.Sensitive = true;
                         });
                    }
               };
          }
          if (game is IReversibleGame<String> reversibleGame)
               reversibleGame.Undone += (sender, args) => 
               {
                    Gtk.Application.Invoke ((sender, args) => 
                    {         
                         ReInitializeFields ();
                         if (ThisGUIusers!.Any (player => player == Game.ActivePlayer))
                         {
                              reversibleGame.Continue ();  
                              OnOwnPlayersTurn (Game.ActivePlayer); 
                         }
                    });
               };
          previousButton = (Gtk.Button) Builder.GetObject("PreviousButton");
          ReInitializeAIs (thisGUIusers, AIs);
     }
     protected virtual void UpdateGUI (bool keepPreviousButtons)
     {          
          if (Game is IPausableGame<String> pausableGame)
          {
               Gtk.Application.Invoke ((sender, args) => 
               {         
                    var pausePlayButton = (Gtk.Button)Builder.GetObject("PausePlayButton");
                    var previousPlayerButton = (Gtk.Button) Builder.GetObject("PreviousPlayerButton");
                    var guiPlayer = ThisGUIusers!.Any (player => player == Game.ActivePlayer);
                    for (int x = 0; x < GameBoard!.xDim; x++)
                    {
                         for (int y = 0; y < GameBoard.yDim; y++)
                         {
                              Fields![x,y].Sensitive = false;   
                         } 
                    }
                    if (guiPlayer)
                    {
                         pausePlayButton.Sensitive = false;
                         previousButton!.Sensitive = true;
                         previousPlayerButton.Sensitive = true;
                    } 
                    else
                    {
                         pausePlayButton.Sensitive = true;
                         if (!keepPreviousButtons)
                         {
                              previousButton!.Sensitive = false;
                              previousPlayerButton.Sensitive = false;
                         }
                    }
                    if (pausableGame.Paused)
                    {
                         var playImage = (Gtk.Image) Builder.GetObject("PlayImage");
                         pausePlayButton.Image = playImage;
                         if (ThisGUIusers!.Any (player => player == Game.ActivePlayer))
                              pausableGame.Continue ();
                    }    
                    else
                    {
                         var pausedImage = (Gtk.Image) Builder.GetObject("PauseImage");
                         pausePlayButton.Image = pausedImage;

                    } 
                    var singleStepButton = (Gtk.Button) Builder.GetObject("SingleStepButton");
                    singleStepButton.Sensitive = false;                         
               });
          }
     }
     protected virtual void OnGameOver (string result)  
     {
          SetLabel (1, result);
          Gtk.Application.Invoke ((sender, args) => 
          {         
               var playImage = (Gtk.Image) Builder.GetObject("PlayImage");
               var pausePlayButton = (Gtk.Button)Builder.GetObject("PausePlayButton");
               pausePlayButton.Image = playImage;
               previousButton!.Sensitive = true;
               var previousPlayerButton = (Gtk.Button) Builder.GetObject("PreviousPlayerButton");
               previousPlayerButton.Sensitive = true;
          });
     }
     void OnPausePlayClicked (Object sender, EventArgs args)
     {
          if (Game!.Running && Game is IPausableGame<String> pausableGame)
          {
               if (pausableGame.Paused)
               {
                    pausableGame.Continue ();
               } 
               else 
               {
                    pausableGame.Pause ();
               }
               UpdateGUI (false);
          }
     }
     void OnPreviousClicked (Object sender, EventArgs args)
     {
          if ((Game is IReversibleGame<String> reversibleGame) && (reversibleGame.Undoable))
          {
               var singleStepButton = (Gtk.Button) Builder.GetObject("SingleStepButton");
               singleStepButton.Sensitive = false;                         
               reversibleGame.Undo();
               SetLabel (1, $"{PlayerToString(reversibleGame.ActivePlayer)}'s turn.");
               UpdateGUI (true);
               reversibleGame.Continue ();  
               OnOwnPlayersTurn (Game.ActivePlayer); 
          }
     }
     void OnPreviousPlayerClicked (Object sender, EventArgs args)
     {
          if ((Game is IReversibleGame<String> reversibleGame) && (reversibleGame.Undoable))
          {
               var singleStepButton = (Gtk.Button) Builder.GetObject("SingleStepButton");
               singleStepButton.Sensitive = false;                         
               reversibleGame.Undo ();
               SetLabel (1, $"{PlayerToString(reversibleGame.ActivePlayer)}'s turn.");
               while (!(ThisGUIusers!.Any(user => user == reversibleGame.ActivePlayer)) && reversibleGame.Undoable)
               {
                    reversibleGame.Undo ();
                    SetLabel (1, $"{PlayerToString(reversibleGame.ActivePlayer)}'s turn.");
               }
               UpdateGUI (true);
               reversibleGame.Continue ();  
               OnOwnPlayersTurn (Game.ActivePlayer); 
          }
     }
     protected void OnSingleStepClicked (Object sender, EventArgs args)
     {
          if (Game is IPausableGame<String> pausableGame)
          {
               pausableGame.SingleStep ();
          }
     }
     void OnInfoClicked(Object sender, EventArgs args)
     {
          var about = new Gtk.AboutDialog ();
          about.ProgramName = "(Board) Game Framework";
          about.Version = "0.9";
          about.Copyright = "(c) 2023 Florian Brinkmeyer";
          about.Website = "https://github.com/FlorianBrinkmeyer/GameFramework";
          about.Run();
          about.Destroy();
     }
     public event EventHandler? Quit;
     void OnQuit (Object sender, EventArgs args)
     {
          var mainForm = (Gtk.ApplicationWindow) Builder.GetObject ("Window");
          mainForm.Dispose ();
          Quit?.Invoke (this, new EventArgs ());
     }     
}