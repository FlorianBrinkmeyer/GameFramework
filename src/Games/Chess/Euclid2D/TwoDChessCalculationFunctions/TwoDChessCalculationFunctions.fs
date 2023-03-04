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

module Chess.TwoDMoveCalculation

open System
open Euclid2D
open GameFramework
open StandardChess
open Euclid2D.Enumerable2DArray

let possibleMovesMode = 1
let blackListMode = -1

let pawnWhiteStandardMove<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) ((x,y) : int*int) =
    if board[x,y+1].IsNone then
        Some (x,y+1)
    else
        None    

let pawnWhiteSpecialStartMove<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) ((x,y) : int*int) =
    if board[x,3].IsNone then
        Some (x,3)
    else
        None    

let pawnWhiteHitMoves<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) ((x,y) : int*int) mode =
    [(x-1,y+1); (x+1,y+1)] |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->  
        match board[xp, yp] with
        | Some piece ->
            piece.Player = (-1) * mode
        | None ->
            false        
    )   

let pawnWhiteEnPassant<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) ((x,y) : int*int) ((otherx,othery) : int*int) =
    if othery = y && otherx = x - 1 && board[x-1, y+1].IsNone then
        Some (x-1, y+1)
    elif othery = y && otherx = x + 1 && board[x+1, y+1].IsNone then
        Some (x+1, y+1)
    else
        None

let pawnWhiteElevationCheck (x,y) = 
    y = 7                

let pawnBlackStandardMove<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) ((x,y) : int*int) =
    if board[x,y-1].IsNone then
        Some (x,y-1)
    else
        None    

let pawnBlackSpecialStartMove<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) ((x,y) : int*int) =
    if board[x,4].IsNone then
        Some (x,4)
    else
        None    

let pawnBlackHitMoves<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) ((x,y) : int*int) mode =
    [(x-1,y-1); (x+1,y-1)] |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->  
        match board[xp, yp] with
        | Some piece ->
            piece.Player = 1 * mode
        | None ->
            false        
    )   

let pawnBlackEnPassant<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) ((x,y) : int*int) ((otherx,othery) : int*int) =
    if othery = y && otherx = x - 1 && board[x-1, y-1].IsNone then
        Some (x-1, y-1)
    elif othery = y && otherx = x + 1 && board[x+1, y-1].IsNone then
        Some (x+1, y-1)
    else
        None         

let pawnBlackElevationCheck (x,y) = 
    y = 0                

let kingStandardMoves<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > ownPlayer (board : 'Board) ((x,y) : int*int) =
    allDirs |> List.map (fun (xdir,ydir) -> x + xdir, y + ydir) |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->
        match board[xp,yp] with
        | Some piece ->
            piece.Player <> ownPlayer
        | None ->
            true    
    )

let kingWhiteCastling =
    let castling1 = {KingDestPos = 6,0; RookStartPos = 7,0; RookDestPos = 5,0; FieldsInBetween = [(4,0);(5,0)]}
    let castling2 = {KingDestPos = 1,0; RookStartPos = 0,0; RookDestPos = 2,0; FieldsInBetween = [(1,0);(2,0)]}
    castling1, castling2                           

let kingBlackCastling =
    let castling1 = {KingDestPos = 6,7; RookStartPos = 7,7; RookDestPos = 5,7; FieldsInBetween = [(4,7);(5,7)]}
    let castling2 = {KingDestPos = 1,7; RookStartPos = 0,7; RookDestPos = 2,7; FieldsInBetween = [(1,7);(2,7)]}
    castling1, castling2                           

let knightPossibleMoves<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > ownPlayer (board : 'Board) ((x,y) : int*int) mode =
    let dirs = [(1,2);(2,1);(-1,2);(2,-1);(1,-2);(-2,1);(-1,-2);(-2,-1)]
    dirs |> List.map (fun (xdir,ydir) -> x + xdir,y + ydir) |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->
        match board[xp,yp] with
        | Some piece ->
            piece.Player <> ownPlayer * mode
        | None ->
            true    
    )

let knightThreateningTest<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (ownPos : int*int) (checkPos : int*int) =
    let diff = (Euclid2DCoords checkPos) - (Euclid2DCoords ownPos)
    let diffAbs = Math.Abs diff.X, Math.Abs diff.Y
    match diffAbs with
    | (1,2) | (2,1) ->
        true
    | _ ->
        false 

let blockingPiecePossibleMoves<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > dirs ownPlayer (board : 'Board) ((x,y) : int*int) mode =
    let nonHitMoves =
        dirs |> Seq.collect (fun dir -> board.ToSeqInDirWithCoords (x,y) dir |> Seq.takeWhile (fun (piece, _) -> piece.IsNone) |> Seq.map snd)
    let hitMoves = 
        let potentialHitMoves = 
            dirs |> Seq.map (fun dir -> board.ToSeqInDirWithCoords (x,y) dir |> Seq.skipWhile (fun (piece, _) -> piece.IsNone) |> Seq.tryHead)
        potentialHitMoves |> Seq.choose (fun move ->
            match move with
            | Some ((Some piece), coords) when piece.Player <> ownPlayer * mode ->
                Some coords
            | _ -> None    
        )
    Seq.concat [nonHitMoves; hitMoves]

let getFieldsBetweenIfDirMatches<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (board : 'Board) (ownPos : int*int) (toCheck : int *int) (dirs : List<int*int>) =
    let diff = (Euclid2DCoords toCheck) - (Euclid2DCoords ownPos)
    let dir = diff.NormalizeCompWise
    let len = Math.Max (diff.X, diff.Y)
    if dir*len = diff && dirs |> Seq.exists ((=) dir.AsTuple) then
        board.ToSeqInDirWithCoords ownPos dir.AsTuple |> Seq.take (len - 1) |> Some
    else
        None    

let blockingPieceThreateningTestReturnsThreatNeutralizingMoves<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (dirs : List<int*int>) (board : 'Board) (ownPos : int*int) (toCheck : int *int) =
    match getFieldsBetweenIfDirMatches board ownPos toCheck dirs with
    | Some fields when fields |> Seq.forall (fun (piece, _) -> piece.IsNone) ->
        let fieldsBetween = fields |> Seq.map snd |> Seq.toList
        ownPos :: fieldsBetween |> List.toSeq |> Some
    | _ ->
        None

let getPotentialWhiteListAndPieceToApply<'Board, 'Piece when 'Board :> IEnumerable2DArray<'Piece> and 'Piece :> IPiece > (dirs : List<int*int>) ownPlayer (board : 'Board) (ownPos : int*int) (toCheck : int *int) =
    match getFieldsBetweenIfDirMatches board ownPos toCheck dirs with
    | Some fields ->
        let otherPieces = fields |> Seq.choose (fun (maybePiece,_) ->
            match maybePiece with
            | Some piece when piece.Player = ownPlayer * (-1) ->
                Some piece
            | _ -> None  
        )
        match otherPieces |> Seq.toList with
        | onlyPiece :: [] ->
            let fieldsWithOwn = ownPos :: (fields |> Seq.map snd |> Seq.toList) |> List.toSeq
            Some (fieldsWithOwn, onlyPiece)
        | _ -> None
    | None -> None            