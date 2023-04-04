(*
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
*)

module GameFramework.GameInit

open GameFramework
open System
open System.IO
open System.Collections

let debugMode = true
let debugSearchDepth = 2

let resourcesFolder = "../../Resources"
let GUI = "StartGUI.xml"
let standardAIConsiderationTime = 10000
let standardUsedThreadsByAI = 4

type AIInfo =
    {Label : String;
     Player : int;
     ConsiderationTime : int;
     MaybeUsedThreads : Option<int>}

let monteCarloSearchTree = "Monte Carlo tree search with multi-threading"
let negaMaxPruningCaching = "NegaMax time-limited with alpha-beta pruning, caching and multi-threading"
let negaMaxTimeLimited = "NegaMax time-limited"

let aisWithMultiThreading = [monteCarloSearchTree; negaMaxPruningCaching]

type GameInfo = 
    {Name : String;
     SupportedAIs : List<String>;
     UsesInfiniteValues : bool;
     PlayerIDsAndTitles : List<int * String>}

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

let mutable AIagents = Generic.Dictionary<int, AIInfo> ()
let mutable maybeChosenPlayer : Option<int> = None

type StartGUI () as this =
    let mutable builder : Gtk.Builder = new Gtk.Builder () 
    let setLabel (text : String) (name : String) = (builder.GetObject name :?> Gtk.Label).Text <- text
    let getLabel (name : String) = (builder.GetObject name :?> Gtk.Label).Text
    let getText (name : String) = (builder.GetObject name :?> Gtk.Entry).Text
    let setText (text : String) (name : String) = (builder.GetObject name :?> Gtk.Entry).Text <- text 
    let setSensitive sensitive (name : String) = (builder.GetObject name :?> Gtk.Widget).Sensitive <- sensitive
    let getIfChecked (name : String) = (builder.GetObject name :?> Gtk.CheckButton).Active
    let setChecked check (name : String) = (builder.GetObject name :?> Gtk.CheckButton).Active <- check
    let fillCombo (lines : seq<String>) (name : String) =
        let combo = builder.GetObject name :?> Gtk.ComboBoxText
        combo.RemoveAll ()
        lines |> Seq.iter combo.AppendText 
    let clearCombo (name : String) =
        let combo = builder.GetObject name :?> Gtk.ComboBoxText
        combo.Clear ()
    let getActive (name : String) = 
        (builder.GetObject name :?> Gtk.ComboBoxText).ActiveText
    let maybeCreateNewAIInfo () =
        maybeChosenPlayer |> Option.iter (fun chosenPlayer -> 
            let chosenAI = "ChosenAILabel" |> getLabel
            if not (String.IsNullOrEmpty chosenAI) && "AssignAIToPlayer" |> getIfChecked then
                let considerationTime =
                    let text = "ConsiderationTimeEntry" |> getText
                    if String.IsNullOrEmpty text then
                        standardAIConsiderationTime
                    else
                        text |> Int32.Parse
                let maybeUsedThreads =
                    if aisWithMultiThreading |> List.exists (fun name -> name = chosenAI) then
                        let text = "UsedThreadsEntry" |> getText
                        if String.IsNullOrEmpty text then
                            standardAIConsiderationTime |> Some
                        else
                            text |> Int32.Parse |> Some
                    else
                        None        
                let newAIInfo = {Label = chosenAI; Player = chosenPlayer; ConsiderationTime = considerationTime; MaybeUsedThreads = maybeUsedThreads}
                AIagents[chosenPlayer] <- newAIInfo                                       
        )
    let getAIsFromInfos gameUsedInfiniteValues =
        AIagents.Values |> Seq.toArray |> Array.map (fun info ->
            if info.Label = monteCarloSearchTree then
                MonteCarloTreeSearch (info.Player, info.ConsiderationTime, gameUsedInfiniteValues, info.MaybeUsedThreads.Value) :> AI_Agent
            elif info.Label = negaMaxTimeLimited then
               if debugMode then
                   NegaMax (info.Player, debugSearchDepth)
               else
                   NegaMaxTimeLimited (info.Player, info.ConsiderationTime, 100)
            elif info.Label = negaMaxPruningCaching then
                Negamax.NegaMaxTimeLimitedPruningCaching (info.Player, info.ConsiderationTime, 100, false, debugMode, info.MaybeUsedThreads.Value)    
            else    
                raise (Exception "AI case distinction incomplete.")
        )       
    let deactivateAllAIWidgets () =
        "AIChooser" |> setSensitive false
        "ConsiderationTimeEntry" |> setSensitive false
        "ConsiderationTimeEntry" |> setText String.Empty
        "UsedThreadsEntry" |> setSensitive false
        "UsedThreadsEntry" |> setText String.Empty
    do
        Gtk.Application.Init ()
        let filename = Path.Combine [|resourcesFolder; GUI|]
        builder.AddFromFile filename |> ignore
        builder.Autoconnect this
        "GameChooser" |> fillCombo (games |> List.map (fun game -> game.Name))      
    [<GLib.ConnectBefore>]
    member x.OnQuit (_: Object) (_: EventArgs) = 
        Gtk.Application.Quit ()      
    [<GLib.ConnectBefore>]
    member x.OnGameChanged (sender : Object) (_ : EventArgs) =
        let gameName = (sender :?> Gtk.ComboBoxText).ActiveText 
        if not (String.IsNullOrEmpty gameName) then
            let game = games |> List.find (fun gm -> gm.Name = gameName)
            "PlayerChooser" |> fillCombo (game.PlayerIDsAndTitles |> List.map snd)
            "PlayerChooser" |> setSensitive true
            "StartButton" |> setSensitive true
            AIagents <- Generic.Dictionary<int, AIInfo> ()
            maybeChosenPlayer <- None
            "AssignAIToPlayer" |> setSensitive false
            "AssignAIToPlayer" |> setChecked false
            deactivateAllAIWidgets ()
    [<GLib.ConnectBefore>]
    member x.OnPlayerChanged (sender : Object) (_ : EventArgs) =
        let gameName = "GameChooser" |> getActive
        if not (String.IsNullOrEmpty gameName) then
            let game = games |> List.find (fun gm -> gm.Name = gameName)
            maybeCreateNewAIInfo ()
            let playerTitle = "PlayerChooser" |> getActive
            if not (String.IsNullOrEmpty playerTitle) then
                let playerID = game.PlayerIDsAndTitles |> List.find (fun (_, title) -> title = playerTitle) |> fst
                maybeChosenPlayer <- Some playerID
                "AssignAIToPlayer" |> setSensitive true
                "AIChooser" |> fillCombo game.SupportedAIs
                match AIagents.TryGetValue playerID with
                | true, info ->
                    "AssignAIToPlayer" |> setChecked true
                    "AIChooser" |> setSensitive true
                    "ConsiderationTimeEntry" |> setText (info.ConsiderationTime.ToString ())
                    "ConsiderationTimeEntry" |> setSensitive true
                    "ChosenAILabel" |> setLabel info.Label
                    match info.MaybeUsedThreads with
                    | Some threadsCount ->
                        "UsedThreadsEntry" |> setText (threadsCount.ToString ())
                        "UsedThreadsEntry" |> setSensitive true                    
                    | None -> 
                        "UsedThreadsEntry" |> setText String.Empty
                        "UsedThreadsEntry" |> setSensitive false                   
                | _ -> 
                    "AssignAIToPlayer" |> setChecked false
                    "ChosenAILabel" |> setLabel String.Empty
                    deactivateAllAIWidgets ()
    [<GLib.ConnectBefore>]
    member x.OnAssignAIToPlayerToggled (sender : Object) (_ : EventArgs) =
        deactivateAllAIWidgets ()
        let game = games |> List.find (fun gm -> gm.Name = ("GameChooser" |> getActive))
        if (sender :?> Gtk.CheckButton).Active then
            "AIChooser" |> fillCombo game.SupportedAIs
            "AIChooser" |> setSensitive true
        else
            "ChosenAILabel" |> setLabel String.Empty    
            let playerTitle = "PlayerChooser" |> getActive
            if not (String.IsNullOrEmpty playerTitle) then
                let playerID = game.PlayerIDsAndTitles |> List.find (fun (_, title) -> title = playerTitle) |> fst
                AIagents.Remove playerID |> ignore
    [<GLib.ConnectBefore>]
    member x.OnAIChooserChanged (sender : Object) (_ : EventArgs) =
        let chosenAI = (sender :?> Gtk.ComboBoxText).ActiveText      
        if not (String.IsNullOrEmpty chosenAI) then
            "ConsiderationTimeEntry" |> setText (standardAIConsiderationTime.ToString ())
            "ConsiderationTimeEntry" |> setSensitive true
            match aisWithMultiThreading |> List.tryFind (fun ai -> ai = chosenAI) with
            | Some _ ->
                "UsedThreadsEntry" |> setText (standardUsedThreadsByAI.ToString ())
                "UsedThreadsEntry" |> setSensitive true                  
            | None ->
                "UsedThreadsEntry" |> setText String.Empty
                "UsedThreadsEntry" |> setSensitive false                  
            "ChosenAILabel" |> setLabel chosenAI
    [<GLib.ConnectBefore>]
    member x.OnStartClicked (sender : Object) (_ : EventArgs) =
        let startButton = builder.GetObject "StartButton" :?> Gtk.Button
        let gameChooser = builder.GetObject "GameChooser" :?> Gtk.ComboBoxText
        startButton.Sensitive <- false
        gameChooser.Sensitive <- false
        let game = games |> List.find (fun gm -> gm.Name = ("GameChooser" |> getActive))
        maybeCreateNewAIInfo ()           
        let getAIsAndHumans () =
            let ais = getAIsFromInfos game.UsesInfiniteValues
            let aiInformers = ais |> Array.choose (fun ai ->
                match ai with
                | :? AI_Informer as informer ->
                    Some informer
                | _ -> None    
            )
            let aiPlayers = ais |> Seq.map (fun ai -> ai.Player) |> Set.ofSeq
            let allPlayers = game.PlayerIDsAndTitles |> List.map fst |> Set.ofList
            let humanPlayers = allPlayers - aiPlayers
            ais, aiInformers, humanPlayers
        let ais, aiInformers, humanPlayers = getAIsAndHumans ()
        if game = reversiWithPassing then
            let gameCompanion, boardCompanion = Reversi.Init.initReversi 8 8 (-1) true Reversi.ResultMapper.resultMapper ais debugMode
            let gui = 
                let imageFolder = Path.Combine [|resourcesFolder; "ReversiPieceImages"|]
                let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
                Reversi.ReversiGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers, debugMode)
            gameCompanion.UpdateAIs.AddHandler (fun _ _ ->
                maybeCreateNewAIInfo ()
                let ais, aiInformers, humanPlayers = getAIsAndHumans ()
                gameCompanion.UpdateAIAgents ais
                gui.ReInitializeAIs (humanPlayers, aiInformers)
            )
            gui.Quit.AddHandler (fun _ _ -> 
                gameCompanion.Stop ()
                startButton.Sensitive <- true
                gameChooser.Sensitive <- true
            )
            gameCompanion.Run ()
        elif game = reversiNoPassing then
            let gameCompanion, boardCompanion = Reversi.Init.initReversi 8 8 (-1) false Reversi.ResultMapper.resultMapper ais debugMode
            let gui = 
                let imageFolder = Path.Combine [|resourcesFolder; "ReversiPieceImages"|]
                let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
                Reversi.ReversiGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers, debugMode)
            gameCompanion.UpdateAIs.AddHandler (fun _ _ ->
                maybeCreateNewAIInfo ()
                let ais, aiInformers, humanPlayers = getAIsAndHumans ()
                gameCompanion.UpdateAIAgents ais
                gui.ReInitializeAIs (humanPlayers, aiInformers)
            )
            gui.Quit.AddHandler (fun _ _ -> 
                gameCompanion.Stop ()
                startButton.Sensitive <- true
                gameChooser.Sensitive <- true
            )
            gameCompanion.Run ()
        elif game = standardChess then
            let startPositionFileName = Path.Combine [|resourcesFolder; "StandardChessStartPosition.csv"|]
            let pieceFactory = fun kind color -> StandardChess.PieceFactory.InitPiece (kind, color)
            let gameCompanion, (boardCompanion) = 
                Chess.Init.initChess 8 8 1 pieceFactory startPositionFileName Chess.ResultMapper.resultMapper ais debugMode
            let gui = 
                let imageFolder = Path.Combine [|resourcesFolder; "ChessPieceImages"|]
                let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
                Chess.ChessGtkGUI (800, 880, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers, debugMode)
            gameCompanion.UpdateAIs.AddHandler (fun _ _ ->
                maybeCreateNewAIInfo ()
                let ais, aiInformers, humanPlayers = getAIsAndHumans ()
                gameCompanion.UpdateAIAgents ais
                gui.ReInitializeAIs (humanPlayers, aiInformers)
            )
            gui.Quit.AddHandler (fun _ _ -> 
                gameCompanion.Stop ()
                startButton.Sensitive <- true
                gameChooser.Sensitive <- true
            )
            gameCompanion.Run ()      
        else
            raise (Exception "Game case distinction incomplete.")

[<EntryPoint>]
let main argv =
    let gui = StartGUI ()
    Gtk.Application.Run ()
    0