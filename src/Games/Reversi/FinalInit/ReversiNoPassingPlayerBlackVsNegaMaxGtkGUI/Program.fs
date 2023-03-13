open GameFramework
open Reversi.Init
open Reversi
open Reversi.ResultMapper

let negamax = NegaMaxTimeLimitedCaching (1, 5000, 100, false)
let gameCompanion, boardCompanion = initZSGame 8 8 (-1) false resultMapper [negamax]
(negamax :> AI_Informer).MoveDecisionMade.AddHandler (fun _ chosenMove -> Gtk.Application.Invoke (fun _ _ -> gameCompanion.MakeMove chosenMove))
let gui = 
    ReversiGtkGUI (800, 900, "../../../../../Resources/ReversiPieceImages", "../../../../../Resources/SimpleTwoDBoardGUI.xml", boardCompanion, gameCompanion, [-1], [negamax])
gameCompanion.Run ()
Gtk.Application.Run ()