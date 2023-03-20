open GameFramework
open Reversi.Init
open Reversi
open Reversi.ResultMapper

let negamax1 = NegaMaxTimeLimitedCaching (-1, 5000, 100)
let negamax2 = MonteCarloTreeSearch (1, 5000)
let gameCompanion, boardCompanion = initZSGame 8 8 (-1) false resultMapper [negamax1; negamax2]
let gui = 
    ReversiGtkGUI (800, 830, "../../../../../Resources/ReversiPieceImages", "../../../../../Resources/SimpleTwoDBoardGUI.xml", boardCompanion, gameCompanion, [], [negamax1; negamax2])
gameCompanion.Run ()
Gtk.Application.Run ()