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

type StartGUI () as this =
    let mutable builder : Gtk.Builder = new Gtk.Builder () 
    let getText (name : String) = (builder.GetObject name :?> Gtk.Entry).Text
    let getTextAsInt (name : String) = (builder.GetObject name :?> Gtk.Entry).Text |> Int32.Parse
    let setText (text : String) (name : String) = (builder.GetObject name :?> Gtk.Entry).Text <- text 
    let setSensitive sensitive (name : String) = (builder.GetObject name :?> Gtk.Widget).Sensitive <- sensitive
    let fillCombo (lines : seq<String>) (name : String) =
        let combo = builder.GetObject name :?> Gtk.ComboBoxText
        combo.RemoveAll ()
        lines |> Seq.iter combo.AppendText 
    let getActive (name : String) = 
        let text = (builder.GetObject name :?> Gtk.ComboBoxText).ActiveText
        if text <> null then
            text.Trim ()
        else
            null   
    let getIfChecked (name : String) = (builder.GetObject name :?> Gtk.CheckButton).Active
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
        let game = games |> List.find (fun gm -> gm.Name = (sender :?> Gtk.ComboBoxText).ActiveText)
        "PlayerChooser" |> fillCombo (game.PlayerIDsAndTitles |> List.map snd)
        "StartButton" |> setSensitive true
    [<GLib.ConnectBefore>]
    member x.OnPlayerChanged (sender : Object) (_ : EventArgs) =
        ()
    [<GLib.ConnectBefore>]
    member x.OnAssignAIToPlayerToggled (sender : Object) (_ : EventArgs) =
        ()
    [<GLib.ConnectBefore>]
    member x.OnAIChooserChanged (sender : Object) (_ : EventArgs) =
        ()
    [<GLib.ConnectBefore>]
    member x.OnStartClicked (sender : Object) (_ : EventArgs) =
        ()

[<EntryPoint>]
let main argv =
    let gui = StartGUI ()
    Gtk.Application.Run ()
    0