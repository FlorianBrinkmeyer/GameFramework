module Reversi.ResultMapper

open GameFramework

let resultMapper (game : ImmutableGame) =
    if game.Value 1 > 0 then
        sprintf "White has %A pieces more on the board than black: White wins." (game.Value 1)
    elif game.Value -1 > 0 then
        sprintf "Black has %A pieces more on the board than white: Black wins." (game.Value -1)
    else
        "Tie: Black and white have the same amount of pieces on the board."    