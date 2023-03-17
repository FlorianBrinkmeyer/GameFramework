module Chess.ResultMapper

open GameFramework
open System

let resultMapper (game : ImmutableGame) =
    if game.Value 1 = Double.PositiveInfinity then
        "White has checkmated black: White wins."
    elif game.Value -1 = Double.PositiveInfinity then
        "Black has checkmated white: Black wins."
    else
        if game.ActivePlayer = 1 then
            "White isn't checked, but unable to make a legal move: Tie."
        else
            "Black isn't checked, but unable to make a legal move: Tie."