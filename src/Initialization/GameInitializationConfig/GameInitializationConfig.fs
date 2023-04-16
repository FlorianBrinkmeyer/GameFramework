//Amend this file to add futher games or AI agents to the starter GUI. 

module GameFramework.GameInit.Config

open System
open System.IO
open System.Collections
open GameFramework
open GameFramework.GameInit.HelperTypes

let debugMode = false
let debugSearchDepth = 3

let negaMaxIncreaseSearchDepthIfStateUnstable = false

let resourcesFolder = "../../../Resources"
let standardAIConsiderationTime = 10000
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

let initAIs gameUsedInfiniteValues (aiInfos : AIInfo []) =
    aiInfos |> Array.map (fun info ->
        if info.Label = monteCarloSearchTree then
            MonteCarloTreeSearch (info.Player, info.ConsiderationTime, gameUsedInfiniteValues, info.MaybeUsedThreads.Value) :> AI_Agent
        elif info.Label = negaMaxTimeLimited then
           (*if debugMode then
               NegaMax (info.Player, debugSearchDepth)
           else*)
               NegaMaxTimeLimited (info.Player, info.ConsiderationTime, 100)
        elif info.Label = negaMaxPruningCaching then
            Negamax.NegaMaxTimeLimitedPruningCaching (info.Player, info.ConsiderationTime, 100, negaMaxIncreaseSearchDepthIfStateUnstable, debugMode, info.MaybeUsedThreads.Value)    
        else    
            raise (Exception "AI case distinction incomplete.")
    )       

let initGame (game : GameInfo) (aisAsAgents : Generic.IEnumerable<AI_Agent>) (aisAsInformers : Generic.IEnumerable<AI_Informer>) (humanPlayers : Generic.IEnumerable<int>) =
    if game = reversiWithPassing then
        let gameCompanion, boardCompanion = Reversi.Init.initReversi 8 8 (-1) true Reversi.ResultMapper.resultMapper aisAsAgents debugMode
        let gui = 
            let imageFolder = Path.Combine [|resourcesFolder; "ReversiPieceImages"|]
            let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
            Reversi.ReversiGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aisAsInformers, debugMode)
        gui :> IGui, gameCompanion :> IInitGame
    elif game = reversiNoPassing then
        let gameCompanion, boardCompanion = Reversi.Init.initReversi 8 8 (-1) false Reversi.ResultMapper.resultMapper aisAsAgents debugMode
        let gui = 
            let imageFolder = Path.Combine [|resourcesFolder; "ReversiPieceImages"|]
            let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
            Reversi.ReversiGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aisAsInformers, debugMode)
        gui, gameCompanion
    elif game = standardChess then
        let startPositionFileName = Path.Combine [|resourcesFolder; "StandardChessStartPosition.csv"|]
        let pieceFactory = fun kind color -> StandardChess.PieceFactory.InitPiece (kind, color)
        let gameCompanion, boardCompanion = Chess.Init.initChess 8 8 1 pieceFactory startPositionFileName Chess.ResultMapper.resultMapper aisAsAgents debugMode
        let gui = 
            let imageFolder = Path.Combine [|resourcesFolder; "ChessPieceImages"|]
            let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
            Chess.ChessGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aisAsInformers, debugMode)
        gui, gameCompanion
    else
        raise (Exception "Game case distinction incomplete.")