//Amend this file to add futher games or AI agents to the starter GUI. 

module GameFramework.GameInit.Config

open System
open System.IO
open System.Collections
open GameFramework
open GameFramework.GameInit.HelperTypes

let debugMode = false
let debugSearchDepth = 3

let negaMaxIncreaseSearchDepth = false

let resourcesFolder = "../../../Resources"
let GUI = "StartGUI.xml"
let standardAIConsiderationTime = 5000
let standardUsedThreadsByAI = 2

let monteCarloSearchTree = "Monte Carlo tree search with multi-threading"
let negaMaxPruningCaching = "NegaMax time-limited with alpha-beta pruning, caching and multi-threading"
let negaMaxTimeLimited = "NegaMax time-limited"

let aisWithMultiThreading = [monteCarloSearchTree; negaMaxPruningCaching]

let reversiWithPassing = 
    {Name = "Reversi"; 
     SupportedAIs = [monteCarloSearchTree]; 
     UsesInfiniteValues = false;
     PlayerIDsAndTitles = [(1,"White");(-1,"Black")]}    

let reversiNoPassing = 
    {Name = "Reversi without passing"; 
     SupportedAIs = [negaMaxPruningCaching; monteCarloSearchTree; negaMaxTimeLimited];
     UsesInfiniteValues = false;
     PlayerIDsAndTitles = [(1,"White");(-1,"Black")]}    

let standardChess = 
    {Name = "Chess"; 
     SupportedAIs = [negaMaxPruningCaching; monteCarloSearchTree; negaMaxTimeLimited];
     UsesInfiniteValues = true;
     PlayerIDsAndTitles = [(1,"White");(-1,"Black")]}    

let games = [reversiWithPassing; reversiNoPassing; standardChess]

let initGame (game : GameInfo) (ais : AI_Agent []) (aiInformers : AI_Informer []) (humanPlayers : Generic.IEnumerable<int>) =
    if game = reversiWithPassing then
        let gameCompanion, boardCompanion = Reversi.Init.initReversi 8 8 (-1) true Reversi.ResultMapper.resultMapper ais debugMode
        let gui = 
            let imageFolder = Path.Combine [|resourcesFolder; "ReversiPieceImages"|]
            let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
            Reversi.ReversiGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers, debugMode)
        gui :> IGui, gameCompanion :> IGameCompanion
    elif game = reversiNoPassing then
        let gameCompanion, boardCompanion = Reversi.Init.initReversi 8 8 (-1) false Reversi.ResultMapper.resultMapper ais debugMode
        let gui = 
            let imageFolder = Path.Combine [|resourcesFolder; "ReversiPieceImages"|]
            let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
            Reversi.ReversiGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers, debugMode)
        gui, gameCompanion
    elif game = standardChess then
        let startPositionFileName = Path.Combine [|resourcesFolder; "StandardChessStartPosition.csv"|]
        let pieceFactory = fun kind color -> StandardChess.PieceFactory.InitPiece (kind, color)
        let gameCompanion, boardCompanion = Chess.Init.initChess 8 8 1 pieceFactory startPositionFileName Chess.ResultMapper.resultMapper ais debugMode
        let gui = 
            let imageFolder = Path.Combine [|resourcesFolder; "ChessPieceImages"|]
            let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
            Chess.ChessGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers, debugMode)
        gui, gameCompanion
    else
        raise (Exception "Game case distinction incomplete.")