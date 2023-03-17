namespace StandardChess

open Chess.TwoDMoveCalculation
open GameFramework
open ImmutableEnumerable2DArray

let rook = BlockablePiece<ImmutableEnumerable2DArray<IBaseChessPiece<'Board, 'Coords>>>

//let getPiece (kind : String) (color : int) =

