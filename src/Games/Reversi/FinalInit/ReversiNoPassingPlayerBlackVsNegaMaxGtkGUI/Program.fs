open GameFramework
open Reversi.Init
open Reversi
open Reversi.ResultMapper

let negamax = NegaMaxTimeLimitedCaching (1, 2000, 100, false)
let gameCompanion, boardCompanion = initZSGame 8 8 (-1) false resultMapper [negamax]
let gui = 
    ReversiGtkGUI (800, 830, "../../../../../Resources/ReversiPieceImages", "../../../../../Resources/SimpleTwoDBoardGUI.xml", boardCompanion, gameCompanion, [-1], [negamax])
gameCompanion.Run ()
Gtk.Application.Run ()