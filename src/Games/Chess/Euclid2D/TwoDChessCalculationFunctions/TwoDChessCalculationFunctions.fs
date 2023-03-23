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
open Chess
open Euclid2D.Enumerable2DArray

let possibleMovesMode = 1
let blackListMode = -1

let pawnWhiteStandardMove ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    if board[x,y+1].IsNone then
        Some (x,y+1)
    else
        None    

let pawnWhiteSpecialStartMove ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    if board[x,3].IsNone then
        Some (x,3)
    else
        None

let pawnWhiteHitMoves ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    [(x-1,y+1); (x+1,y+1)] |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->  
        match board[xp, yp] with
        | Some piece ->
            piece.Player = (-1)
        | None ->
            false        
    )   

let pawnWhiteKingBlackList ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    [(x-1,y+1); (x+1,y+1)] |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->  
        match board[xp, yp] with
        | Some piece ->
            piece.Player = 1
        | None ->
            true        
    )   

let pawnWhiteEnPassant ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    if y = 4 then
        let enPassant1 =
            if x > 0 then
                match board[x-1, y], board[x-1, y+1] with
                | Some potOtherPawn, None when potOtherPawn.Kind = "Pawn" ->
                    Some {PossibleOtherPawnPos = (x-1, y); PossibleOwnDestPos = (x-1, y+1)} 
                | _ -> None
            else
                None
        let enPassant2 =
            if x < 7 then
                match board[x+1, y], board[x+1, y+1] with
                | Some potOtherPawn, None when potOtherPawn.Kind = "Pawn" ->
                    Some {PossibleOtherPawnPos = (x+1, y); PossibleOwnDestPos = (x+1, y+1)} 
                | _ -> None
            else
                None
        [enPassant1; enPassant2] |> Seq.choose id
    else
        Seq.empty    

let pawnWhiteElevationCheck = Func<int*int, bool> (fun (_,y) -> y = 7)                

let pawnBlackStandardMove ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    if board[x,y-1].IsNone then
        Some (x,y-1)
    else
        None    

let pawnBlackSpecialStartMove ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    if board[x,4].IsNone then
        Some (x,4)
    else
        None    

let pawnBlackHitMoves ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    [(x-1,y-1); (x+1,y-1)] |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->  
        match board[xp, yp] with
        | Some piece ->
            piece.Player = 1
        | None ->
            false        
    )   

let pawnBlackKingBlackList ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    [(x-1,y-1); (x+1,y-1)] |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->  
        match board[xp, yp] with
        | Some piece ->
            piece.Player = -1
        | None ->
            true        
    )   

let pawnBlackEnPassant ((board : IEnumerable2DArray<IPiece>), ((x,y) : int*int)) =
    if y = 3 then
        let enPassant1 =
            if x > 0 then
                match board[x-1, y], board[x-1, y-1] with
                | Some potOtherPawn, None when potOtherPawn.Kind = "Pawn" ->
                    Some {PossibleOtherPawnPos = (x-1, y); PossibleOwnDestPos = (x-1, y-1)} 
                | _ -> None
            else
                None
        let enPassant2 =
            if x < 7 then
                match board[x+1, y], board[x+1, y-1] with
                | Some potOtherPawn, None when potOtherPawn.Kind = "Pawn" ->
                    Some {PossibleOtherPawnPos = (x+1, y); PossibleOwnDestPos = (x+1, y-1)} 
                | _ -> None
            else
                None
        [enPassant1; enPassant2] |> Seq.choose id
    else
        Seq.empty    

let pawnBlackElevationCheck = Func<int*int, bool> (fun (_,y) -> y = 0)             

let kingPreStandardMoves ownPlayer mode (board : IEnumerable2DArray<IPiece>) ((x,y) : int*int) =
    allDirs |> List.map (fun (xdir,ydir) -> x + xdir, y + ydir) |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->
        match board[xp,yp] with
        | Some piece ->
            piece.Player <> ownPlayer * mode
        | None ->
            true    
    )

let whiteKingStandardMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (kingPreStandardMoves 1 possibleMovesMode)
let whiteKingKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (kingPreStandardMoves 1 blackListMode)
let blackKingStandardMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (kingPreStandardMoves -1 possibleMovesMode)
let blackKingKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (kingPreStandardMoves -1 blackListMode)

let kingWhiteCastling =
    let castling1 = {KingDestPos = 6,0; RookStartPos = 7,0; RookDestPos = 5,0; AditionallyUnthreatenedFields = [(5,0);(6,0)]; AditionallyEmptyFields = [] }
    let castling2 = {KingDestPos = 2,0; RookStartPos = 0,0; RookDestPos = 3,0; AditionallyUnthreatenedFields = [(2,0);(3,0)]; AditionallyEmptyFields = [(1,0)]}
    seq [castling1; castling2]                           

let kingBlackCastling =
    let castling1 = {KingDestPos = 6,7; RookStartPos = 7,7; RookDestPos = 5,7; AditionallyUnthreatenedFields = [(5,7);(6,7)]; AditionallyEmptyFields = []}
    let castling2 = {KingDestPos = 2,7; RookStartPos = 0,7; RookDestPos = 3,7; AditionallyUnthreatenedFields = [(2,7);(3,7)]; AditionallyEmptyFields = [(1,7)]}
    seq [castling1; castling2]                           

let knightPrePossibleMoves ownPlayer mode (board : IEnumerable2DArray<IPiece>) ((x,y) : int*int) =
    let dirs = [(1,2);(2,1);(-1,2);(2,-1);(1,-2);(-2,1);(-1,-2);(-2,-1)]
    dirs |> List.map (fun (xdir,ydir) -> x + xdir,y + ydir) |> board.FilterCoordsByBoundaries |> Seq.filter (fun (xp, yp) ->
        match board[xp,yp] with
        | Some piece ->
            piece.Player <> ownPlayer * mode
        | None ->
            true    
    )

let blackKnightPosMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (knightPrePossibleMoves -1 possibleMovesMode) 
let blackKnightKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (knightPrePossibleMoves -1 blackListMode)
let whiteKnightPosMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (knightPrePossibleMoves 1 possibleMovesMode)
let whiteKnightKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (knightPrePossibleMoves 1 blackListMode) 

let knightThreateningTest ((ownPos : int*int), (checkPos : int*int)) =
    let diff = (Euclid2DCoords checkPos) - (Euclid2DCoords ownPos)
    let diffAbs = Math.Abs diff.X, Math.Abs diff.Y
    match diffAbs with
    | (1,2) | (2,1) ->
        true
    | _ ->
        false

let blockingPiecePrePossibleMoves dirs ownPlayer mode (board : IEnumerable2DArray<IPiece>) ((x,y) : int*int) =
    let nonHitMoves =
        dirs |> Seq.collect (fun dir -> 
            board.ToSeqInDirWithCoords (x,y) dir |> Seq.skip 1 |> Seq.takeWhile (fun (piece, _) -> piece.IsNone) |> Seq.map snd
        )
    let hitMoves = 
        let potentialHitMoves = 
            dirs |> Seq.map (fun dir -> 
                board.ToSeqInDirWithCoords (x,y) dir |> Seq.skip 1 |> Seq.skipWhile (fun (piece, _) -> piece.IsNone) |> Seq.tryHead
            )
        potentialHitMoves |> Seq.choose (fun move ->
            match move with
            | Some ((Some piece), coords) when piece.Player <> ownPlayer * mode ->
                Some coords
            | _ -> None    
        )
    Seq.append nonHitMoves hitMoves

let blackRookPossibleMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves orthDirs -1 possibleMovesMode) 
let blackRookKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves orthDirs -1 blackListMode)
let whiteRookPossibleMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves orthDirs 1 possibleMovesMode) 
let whiteRookKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves orthDirs 1 blackListMode)
let blackBishopPossibleMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves diaDirs -1 possibleMovesMode) 
let blackBishopKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves diaDirs -1 blackListMode)
let whiteBishopPossibleMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves diaDirs 1 possibleMovesMode) 
let whiteBishopKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves diaDirs 1 blackListMode)
let blackQueenPossibleMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves allDirs -1 possibleMovesMode) 
let blackQueenKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves allDirs -1 blackListMode)
let whiteQueenPossibleMoves = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves allDirs 1 possibleMovesMode) 
let whiteQueenKingBlackList = Func<IEnumerable2DArray<IPiece>, int*int, seq<int*int>> (blockingPiecePrePossibleMoves allDirs 1 blackListMode)

let getFieldsBetweenIfDirMatches (board : IEnumerable2DArray<IPiece>) (ownPos : int*int) (toCheck : int *int) (dirs : List<int*int>) =
    let diff = (Euclid2DCoords toCheck) - (Euclid2DCoords ownPos)
    let dir = diff.NormalizeCompWise
    let len = Math.Max (Math.Abs diff.X, Math.Abs diff.Y)
    if dir*len = diff && dirs |> Seq.exists ((=) dir.AsTuple) then
        board.ToSeqInDirWithCoords ownPos dir.AsTuple |> Seq.skip 1 |> Seq.take (len - 1) |> Some
    else
        None    

let blockingPieceThreateningTestReturnsThreatNeutralizingMoves (dirs : List<int*int>) (board : IEnumerable2DArray<IPiece>) (ownPos : int*int) (toCheck : int *int) =
    match getFieldsBetweenIfDirMatches board ownPos toCheck dirs with
    | Some fields when fields |> Seq.forall (fun (piece, _) -> piece.IsNone) ->
        let fieldsBetween = fields |> Seq.map snd
        Seq.append fieldsBetween [ownPos] |> Some
    | _ ->
        None

let rookThreateningTest = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int*int>>> (blockingPieceThreateningTestReturnsThreatNeutralizingMoves orthDirs)
let bishopThreateningTest = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int*int>>> (blockingPieceThreateningTestReturnsThreatNeutralizingMoves diaDirs)
let queenThreateningTest = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int*int>>> (blockingPieceThreateningTestReturnsThreatNeutralizingMoves allDirs)

let getPotentialWhiteListAndPieceToApply (dirs : List<int*int>) ownPlayer (board : IEnumerable2DArray<IPiece>) (ownPos : int*int) (toCheck : int *int) =
    match getFieldsBetweenIfDirMatches board ownPos toCheck dirs with
    | Some fields ->
        let otherPieces = fields |> Seq.choose (fun (maybePiece, pos) ->
            match maybePiece with
            | Some piece ->
                Some (piece, pos)
            | _ -> None  
        )
        match otherPieces |> Seq.toList with
        | (onlyPiece, pos) :: [] when onlyPiece.Player <> ownPlayer ->
            let fieldsWithOwn = Seq.append (fields |> Seq.map snd) [ownPos]
            Some (fieldsWithOwn, (onlyPiece, pos))
        | _ -> None
    | None -> None

let blackRookGetWhiteListAndPiece = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int * int> * (IPiece * (int * int))>> (getPotentialWhiteListAndPieceToApply orthDirs -1)           
let whiteRookGetWhiteListAndPiece = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int * int> * (IPiece * (int * int))>> (getPotentialWhiteListAndPieceToApply orthDirs 1)
let blackBishopGetWhiteListAndPiece = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int * int> * (IPiece * (int * int))>> (getPotentialWhiteListAndPieceToApply diaDirs -1)           
let whiteBishopGetWhiteListAndPiece = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int * int> * (IPiece * (int * int))>> (getPotentialWhiteListAndPieceToApply diaDirs 1)
let blackQueenGetWhiteListAndPiece = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int * int> * (IPiece * (int * int))>> (getPotentialWhiteListAndPieceToApply allDirs -1)           
let whiteQueenGetWhiteListAndPiece = 
    Func<IEnumerable2DArray<IPiece>, int*int, int*int, Option<seq<int * int> * (IPiece * (int * int))>> (getPotentialWhiteListAndPieceToApply allDirs 1)                      