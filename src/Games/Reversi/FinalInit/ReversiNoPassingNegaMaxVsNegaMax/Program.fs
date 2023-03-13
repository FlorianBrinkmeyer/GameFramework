open GameFramework
open Reversi.Init
open Reversi
open Reversi.ResultMapper

let negamax1 = NegaMaxTimeLimited (-1, 2000, 30, false)
let negamax2 = NegaMaxTimeLimitedCaching (1, 2000, 30, false, false)
let gameCompanion, boardCompanion = initZSGame 8 8 (-1) false resultMapper [negamax1; negamax2]
(negamax1 :> AI_Informer).MoveDecisionMade.AddHandler (fun _ chosenMove -> Gtk.Application.Invoke (fun _ _ -> gameCompanion.MakeMove chosenMove))
(negamax2 :> AI_Informer).MoveDecisionMade.AddHandler (fun _ chosenMove -> Gtk.Application.Invoke (fun _ _ -> gameCompanion.MakeMove chosenMove))
let gui = 
    ReversiGtkGUI (800, 900, "../../../../../Resources/ReversiPieceImages", "../../../../../Resources/SimpleTwoDBoardGUI.xml", boardCompanion, gameCompanion, [], [negamax1; negamax2])
gameCompanion.Run ()
Gtk.Application.Run ()