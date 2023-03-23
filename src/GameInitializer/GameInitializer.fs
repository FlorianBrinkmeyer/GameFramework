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

let resourcesFolder = "../../Resources"
let GUI = "StartGUI.xml"

let monteCarloSearchTree = "Monte Carlo tree search"
let negaMaxTimeLimited = "NegaMax time-limited"
let negaMaxTimeLimitedCaching = "NegaMax time-limited with caching"

type GameInfo = 
    {Name : String;
    SupportedAIs : List<String>;
    PlayerIDsAndTitles : List<int * String>}

let reversiWithPassing = 
    {Name = "Reversi"; 
    SupportedAIs = [monteCarloSearchTree]; 
    PlayerIDsAndTitles = [(1,"White");(-1,"Black")]}    

let reversiNoPassing = 
    {Name = "Reversi without passing"; 
    SupportedAIs = [monteCarloSearchTree; negaMaxTimeLimited; negaMaxTimeLimitedCaching];
    PlayerIDsAndTitles = [(1,"White");(-1,"Black")]}    

let standardChess = 
    {Name = "Chess"; 
    SupportedAIs = [monteCarloSearchTree; negaMaxTimeLimited; negaMaxTimeLimitedCaching]; 
    PlayerIDsAndTitles = [(1,"White");(-1,"Black")]}    

let games = [reversiWithPassing; reversiNoPassing; standardChess]

let mutable AIagents = Generic.Dictionary<int, AI_Agent> ()
let mutable maybeChosenPlayer : Option<int> = None

type StartGUI () as this =
    let mutable builder : Gtk.Builder = new Gtk.Builder () 
    let setLabel (text : String) (name : String) = (builder.GetObject name :?> Gtk.Label).Text <- text
    let getText (name : String) = (builder.GetObject name :?> Gtk.Entry).Text
    let setText (text : String) (name : String) = (builder.GetObject name :?> Gtk.Entry).Text <- text 
    let setSensitive sensitive (name : String) = (builder.GetObject name :?> Gtk.Widget).Sensitive <- sensitive
    let getIfChecked (name : String) = (builder.GetObject name :?> Gtk.CheckButton).Active
    let setChecked check (name : String) = (builder.GetObject name :?> Gtk.CheckButton).Active <- check
    let fillCombo (lines : seq<String>) (name : String) =
        let combo = builder.GetObject name :?> Gtk.ComboBoxText
        combo.RemoveAll ()
        lines |> Seq.iter combo.AppendText 
    let getActive (name : String) = 
        (builder.GetObject name :?> Gtk.ComboBoxText).ActiveText
    let maybeCreateNewAI () =
        maybeChosenPlayer |> Option.iter (fun chosenPlayer -> 
            let chosenAI = "AIChooser" |> getActive
            if not (String.IsNullOrEmpty chosenAI) && "AssignAIToPlayer" |> getIfChecked then
                let considerationTime =
                    let text = "ConsiderationTimeEntry" |> getText
                    if String.IsNullOrEmpty text then
                        5000
                    else
                        text |> Int32.Parse
                let searchDepth =
                    let text = "MaximalSearchDepthEntry" |> getText
                    if String.IsNullOrEmpty text then
                        100
                    else
                        text |> Int32.Parse    
                let newAI =
                    if chosenAI = monteCarloSearchTree then
                        MonteCarloTreeSearch (chosenPlayer, considerationTime) :> AI_Agent
                    elif chosenAI = negaMaxTimeLimited then
                        NegaMaxTimeLimited (chosenPlayer, considerationTime, searchDepth)
                    elif chosenAI = negaMaxTimeLimitedCaching then
                        let justStoreHashes = "AllowHashCollisions" |> getIfChecked
                        let text = "MaximallyCachedStatesEntry" |> getText
                        if String.IsNullOrEmpty text then
                            NegaMaxTimeLimitedCaching (chosenPlayer, considerationTime, searchDepth, justStoreHashes)
                        else
                            NegaMaxTimeLimitedCaching (chosenPlayer, considerationTime, searchDepth, justStoreHashes, text |> Int32.Parse)
                    else
                        raise (Exception "AI case distinction incomplete.")
                AIagents[chosenPlayer] <- newAI                        
        )
    let deactivateMostAIWidgets () =
        "MaximalSearchDepthEntry" |> setSensitive false
        "MaximalSearchDepthEntry" |> setText String.Empty
        "AllowHashCollisions" |> setSensitive false
        "MaximallyCachedStatesEntry" |> setSensitive false
        "MaximallyCachedStatesEntry" |> setText String.Empty
    let deactivateAllAIWidgets () =
        "AIChooser" |> setSensitive false
        "ConsiderationTimeEntry" |> setSensitive false
        "ConsiderationTimeEntry" |> setText String.Empty
        deactivateMostAIWidgets ()
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
            AIagents <- Generic.Dictionary<int, AI_Agent> ()
            maybeChosenPlayer <- None
            "AssignAIToPlayer" |> setSensitive false
            "AssignAIToPlayer" |> setChecked false
            deactivateAllAIWidgets ()
    [<GLib.ConnectBefore>]
    member x.OnPlayerChanged (sender : Object) (_ : EventArgs) =
        maybeCreateNewAI ()
        let gameName = "GameChooser" |> getActive
        if not (String.IsNullOrEmpty gameName) then
            let game = games |> List.find (fun gm -> gm.Name = gameName)
            let playerTitle = "PlayerChooser" |> getActive
            if not (String.IsNullOrEmpty playerTitle) then
                let playerID = game.PlayerIDsAndTitles |> List.find (fun (_, title) -> title = playerTitle) |> fst
                maybeChosenPlayer <- Some playerID
                deactivateMostAIWidgets ()
                "AssignAIToPlayer" |> setSensitive true
                match AIagents.TryGetValue playerID with
                | true, (:? AI_WithConsiderationTime as AI) ->
                    "AssignAIToPlayer" |> setChecked true
                    "ConsiderationTimeEntry" |> setText (AI.ConsiderationTime.ToString ())
                    "ConsiderationTimeEntry" |> setSensitive true
                    match AI with
                    | :? NegaMaxTimeLimited as negaMaxTimeLim ->
                        "ChosenAILabel" |> setLabel negaMaxTimeLimited
                        "MaximalSearchDepthEntry" |> setText (negaMaxTimeLim.MaximalSearchDepth.ToString ())
                        "MaximalSearchDepthEntry" |> setSensitive true
                    | :? NegaMaxTimeLimitedCaching as negaMaxTimeLimCaching ->
                        "ChosenAILabel" |> setLabel negaMaxTimeLimitedCaching
                        "MaximalSearchDepthEntry" |> setText (negaMaxTimeLimCaching.MaximalSearchDepth.ToString ())
                        "MaximalSearchDepthEntry" |> setSensitive true
                        "AllowHashCollisions" |> setChecked negaMaxTimeLimCaching.JustStoreHashes
                        "AllowHashCollisions" |> setSensitive true
                        match negaMaxTimeLimCaching.CacheMaxSize with
                        | Some cacheMaxSize ->
                            "MaximallyCachedStatesEntry" |> setText (cacheMaxSize.ToString ())
                        | None ->
                            "MaximallyCachedStatesEntry" |> setText String.Empty
                        "MaximallyCachedStatesEntry" |> setSensitive true
                    | :? MonteCarloTreeSearch ->
                        "ChosenAILabel" |> setLabel monteCarloSearchTree
                    | _ -> ()
                | _ -> 
                    "AssignAIToPlayer" |> setChecked false
                    deactivateAllAIWidgets ()
    [<GLib.ConnectBefore>]
    member x.OnAssignAIToPlayerToggled (sender : Object) (_ : EventArgs) =
        deactivateAllAIWidgets ()
        if (sender :?> Gtk.CheckButton).Active then
            let game = games |> List.find (fun gm -> gm.Name = ("GameChooser" |> getActive))
            "AIChooser" |> fillCombo game.SupportedAIs
            "AIChooser" |> setSensitive true
    [<GLib.ConnectBefore>]
    member x.OnAIChooserChanged (sender : Object) (_ : EventArgs) =
        let chosenAI = (sender :?> Gtk.ComboBoxText).ActiveText      
        if not (String.IsNullOrEmpty chosenAI) then
            deactivateMostAIWidgets ()
            "ConsiderationTimeEntry" |> setText "5000"
            "ConsiderationTimeEntry" |> setSensitive true
            "ChosenAILabel" |> setLabel chosenAI
            if chosenAI = negaMaxTimeLimited then
                "MaximalSearchDepthEntry" |> setSensitive true
                "MaximalSearchDepthEntry" |> setText "100"
            elif chosenAI = negaMaxTimeLimitedCaching then
                "MaximalSearchDepthEntry" |> setSensitive true
                "MaximalSearchDepthEntry" |> setText "100"
                "AllowHashCollisions" |> setSensitive true
                "AllowHashCollisions" |> setChecked false
                "MaximallyCachedStatesEntry" |> setSensitive true
                "MaximallyCachedStatesEntry" |> setText String.Empty
    [<GLib.ConnectBefore>]
    member x.OnStartClicked (sender : Object) (_ : EventArgs) =
        let mainForm = builder.GetObject "MainForm" :?> Gtk.ApplicationWindow
        mainForm.Hide ()
        maybeCreateNewAI ()
        let game = games |> List.find (fun gm -> gm.Name = ("GameChooser" |> getActive))
        let ais = AIagents.Values
        let aiInformers = ais |> Seq.choose (fun ai ->
            match ai with
            | :? AI_Informer as informer ->
                Some informer
            | _ -> None    
        )
        let aiPlayers = ais |> Seq.map (fun ai -> ai.Player) |> Set.ofSeq
        let allPlayers = game.PlayerIDsAndTitles |> List.map fst |> Set.ofList
        let humanPlayers = allPlayers - aiPlayers
        if game = reversiWithPassing then
            let gameCompanion, boardCompanion = Reversi.Init.initReversi 8 8 (-1) true Reversi.ResultMapper.resultMapper ais
            let gui = 
                let imageFolder = Path.Combine [|resourcesFolder; "ReversiPieceImages"|]
                let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
                Reversi.ReversiGtkGUI (800, 830, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers)
            gameCompanion.Run ()
        elif game = reversiNoPassing then
            let gameCompanion, boardCompanion = Reversi.Init.initReversi 8 8 (-1) false Reversi.ResultMapper.resultMapper ais
            let gui = 
                let imageFolder = Path.Combine [|resourcesFolder; "ReversiPieceImages"|]
                let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
                Reversi.ReversiGtkGUI (800, 830, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers)
            gameCompanion.Run ()
        elif game = standardChess then
            let startPositionFileName = Path.Combine [|resourcesFolder; "StandardChessStartPosition.csv"|]
            let pieceFactory = fun kind color -> StandardChess.PieceFactory.InitPiece (kind, color)
            let gameCompanion, boardCompanion = 
                Chess.Init.initChess 8 8 1 pieceFactory startPositionFileName Chess.ResultMapper.resultMapper ais
            let gui = 
                let imageFolder = Path.Combine [|resourcesFolder; "ChessPieceImages"|]
                let guiBuilder = Path.Combine [|resourcesFolder; "SimpleTwoDBoardGUI.xml"|]
                Chess.ChessGtkGUI (800, 830, imageFolder, guiBuilder, boardCompanion, gameCompanion, humanPlayers, aiInformers)
            gameCompanion.Run ()        
        else
            raise (Exception "Game case distinction incomplete.")

[<EntryPoint>]
let main argv =
    let gui = StartGUI ()
    Gtk.Application.Run ()
    0