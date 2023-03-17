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

namespace StandardChess

open GameFramework
open System
open Chess

type StandardNonBlockablePiece<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and
    'Board :> ImmutableArray<'Coords, IBaseChessPiece<'Board, 'Coords>>> 
    (possibleMoves : Func<'Board, 'Coords, seq<'Coords>>, blackListForKing : Func<'Board, 'Coords, seq<'Coords>>,
    threateningTest : Func<'Coords, 'Coords, bool>, color : int, kind : String, pieceValue : float, whitelist : Option<Set<'Coords>>,
    blacklist : Option<Set<'Coords>>) =
    member x.Color = color
    member x.Kind = kind
    override x.Equals other =
        match other with
        | :? StandardNonBlockablePiece<'Board, 'Coords> as anotherPiece ->
            color = anotherPiece.Color && kind = anotherPiece.Kind
        | _ -> false 
    override x.GetHashCode () = HashCode.Combine (color, kind)
    interface IBaseChessPiece<'Board, 'Coords>
    interface IPiece with
        member x.get_Kind () = kind
        member x.get_Player () = color
    interface IMovablePiece<'Coords> with
        member x.PossibleMoves (game, startField) =
            game.GetMoves startField |> Seq.map (fun indexedMvCm -> indexedMvCm.Cmd.Dest)
        member x.MakeMove (mutableGame, game, start, dest) =
            let index = game.GetMoves start |> Seq.find (fun indexedMvCm -> indexedMvCm.Cmd.Dest = dest) |> fun ind -> ind.Index
            mutableGame.MakeMove index
    interface ISelfEvaluatingPiece<'Board, 'Coords> with
        member x.Value board coords =
            let mobility = (float) (possibleMoves.Invoke (board, coords) |> Seq.length) * 0.1
            pieceValue + mobility
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent> with
        member x.PossibleMoves board coords =
            let moves = possibleMoves.Invoke (board, coords) |> Set.ofSeq
            let set = 
                match whitelist, blacklist with
                | Some white, Some black ->
                    moves |> Set.intersect (white - black)
                | Some white, None ->
                    moves |> Set.intersect white
                | None, Some black ->
                    moves - black
                | None, None ->
                    moves            
            set |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
        member this.ApplyMove board coords moveCmd =
            let nextBoard = board.GetNext coords None
            let finalBoard = nextBoard.GetNext moveCmd.Dest (Some this) :?> 'Board
            let movingEvent = BoardMovingEvent (coords, moveCmd.Dest) :> IBoardMoveEvent
            finalBoard, moveCmd.Dest, [movingEvent]
    interface INonKingChessPiece<'Board, 'Coords> with
        member x.BlackListForKing board coords =
            blackListForKing.Invoke (board, coords)
        member x.IsThreateningField _ ownPos toCheckPos =
            if threateningTest.Invoke (ownPos, toCheckPos) then
                Some (seq [ownPos])
            else
                None
        member x.AugmentByWhiteList board coords newWhitelist =
            let nextWhitelist =
                match whitelist with
                | Some white ->
                    white |> Set.intersect (newWhitelist |> Set.ofSeq) |> Some
                | None ->
                    newWhitelist |> Set.ofSeq |> Some
            let nextPiece = 
                StandardNonBlockablePiece<'Board, 'Coords>(possibleMoves, blackListForKing, threateningTest, color, kind, pieceValue, nextWhitelist,
                    blacklist) :> IBaseChessPiece<'Board, 'Coords> |> Some
            board.GetNext coords nextPiece :?> 'Board
        member x.AugmentByBlackList board coords newBlackList =
            let nextBlackList =
                match blacklist with
                | Some black ->
                    black |> Set.union (newBlackList |> Set.ofSeq) |> Some
                | None ->                      
                    newBlackList |> Set.ofSeq |> Some
            let nextPiece = 
                StandardNonBlockablePiece<'Board, 'Coords>(possibleMoves, blackListForKing, threateningTest, color, kind, pieceValue, whitelist,
                    nextBlackList) :> IBaseChessPiece<'Board, 'Coords> |> Some
            board.GetNext coords nextPiece :?> 'Board

type BlockablePiece<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IBaseChessPiece<'Board, 'Coords>>> 
    (possibleMoves : Func<'Board, 'Coords, seq<'Coords>>, blackListForKing : Func<'Board, 'Coords, seq<'Coords>>,
    threateningTest : Func<'Board, 'Coords, 'Coords, Option<seq<'Coords>>>, 
    getWhiteListAndPiece : Func<'Board, 'Coords, 'Coords, Option<seq<'Coords> * (IBaseChessPiece<'Board, 'Coords> * 'Coords)>>, color : int,
    kind : String, pieceValue : float, notMovedYet, whitelist : Option<Set<'Coords>>, blacklist : Option<Set<'Coords>>) =
    member x.Color = color
    member x.Kind = kind
    member x.NotMovedYet = notMovedYet
    override x.Equals other =
        match other with
        | :? BlockablePiece<'Board, 'Coords> as anotherBlockingPiece ->
            color = anotherBlockingPiece.Color && kind = anotherBlockingPiece.Kind && notMovedYet = anotherBlockingPiece.NotMovedYet
        | _ -> false
    override x.GetHashCode () = HashCode.Combine (color, notMovedYet, kind)
    interface IBaseChessPiece<'Board, 'Coords>
    interface IPiece with
        member x.get_Kind () = kind
        member x.get_Player () = color
    interface IMovablePiece<'Coords> with
        member x.PossibleMoves (game, startField) =
            game.GetMoves startField |> Seq.map (fun indexedMvCm -> indexedMvCm.Cmd.Dest)
        member x.MakeMove (mutableGame, game, start, dest) =
            let index = game.GetMoves start |> Seq.find (fun indexedMvCm -> indexedMvCm.Cmd.Dest = dest) |> fun ind -> ind.Index
            mutableGame.MakeMove index
    interface ISelfEvaluatingPiece<'Board, 'Coords> with
        member x.Value board coords =
            let mobility = (float) (possibleMoves.Invoke (board, coords) |> Seq.length) * 0.1
            pieceValue + mobility
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent> with
        member x.PossibleMoves board coords =
            let moves = possibleMoves.Invoke (board, coords) |> Set.ofSeq
            let set = 
                match whitelist, blacklist with
                | Some white, Some black ->
                    moves |> Set.intersect (white - black)
                | Some white, None ->
                    moves |> Set.intersect white
                | None, Some black ->
                    moves - black
                | None, None ->
                    moves            
            set |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
        member this.ApplyMove board coords moveCmd =
            let nextPiece =
                match notMovedYet with
                | true ->
                    BlockablePiece<'Board, 'Coords> (possibleMoves, blackListForKing, threateningTest, getWhiteListAndPiece, color, kind, pieceValue,
                    false, whitelist, blacklist)
                | false ->
                    this    
            let nextBoard = board.GetNext coords None
            let finalBoard = nextBoard.GetNext moveCmd.Dest (Some nextPiece) :?> 'Board
            let movingEvent = BoardMovingEvent (coords, moveCmd.Dest) :> IBoardMoveEvent
            finalBoard, moveCmd.Dest, [movingEvent]
    interface INonKingChessPiece<'Board, 'Coords> with
        member x.BlackListForKing board coords =
            blackListForKing.Invoke (board, coords)
        member x.IsThreateningField board ownPos toCheckPos =
            threateningTest.Invoke (board, ownPos, toCheckPos)
        member x.AugmentByWhiteList board coords newWhitelist =
            let nextWhitelist =
                match whitelist with
                | Some white ->
                    white |> Set.intersect (newWhitelist |> Set.ofSeq) |> Some
                | None ->
                    newWhitelist |> Set.ofSeq |> Some
            let nextPiece = 
                BlockablePiece<'Board, 'Coords>(possibleMoves, blackListForKing, threateningTest, getWhiteListAndPiece, color, kind, pieceValue,
                notMovedYet, nextWhitelist, blacklist) :> IBaseChessPiece<'Board, 'Coords> |> Some
            board.GetNext coords nextPiece :?> 'Board
        member x.AugmentByBlackList board coords newBlackList =
            let nextBlackList =
                match blacklist with
                | Some black ->
                    black |> Set.union (newBlackList |> Set.ofSeq) |> Some
                | None ->                      
                    newBlackList |> Set.ofSeq |> Some
            let nextPiece = 
                BlockablePiece<'Board, 'Coords>(possibleMoves, blackListForKing, threateningTest, getWhiteListAndPiece, color, kind, pieceValue,
                notMovedYet, whitelist, nextBlackList) :> IBaseChessPiece<'Board, 'Coords> |> Some
            board.GetNext coords nextPiece :?> 'Board
    interface IBlockableChessPiece<'Board, 'Coords> with
        member x.PotentiallyAugmentBlockingPieceByLists board ownPos toCheckPos =
            match getWhiteListAndPiece.Invoke (board, ownPos, toCheckPos) with
            | Some (whitelist, (:? INonKingChessPiece<'Board, 'Coords> as piece, piecePos)) ->
                piece.AugmentByWhiteList board piecePos whitelist
            | _ -> board       

type Pawn<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IBaseChessPiece<'Board, 'Coords>>> 
    (standardMove : Func<'Board, 'Coords, Option<'Coords>>, specialStartMove : Func<'Board, 'Coords, Option<'Coords>>,
    hitMoves : Func<'Board, 'Coords, seq<'Coords>>, blackListForKing : Func<'Board, 'Coords, seq<'Coords>>,
    enPassantCapture : Func<'Board, 'Coords, seq<EnPassantInfo<'Coords>>>, elevationCheck : Func<'Coords, bool>, color : int, notMovedYet,
    whitelist : Option<Set<'Coords>>, blacklist : Option<Set<'Coords>>, queen : IBaseChessPiece<'Board, 'Coords>) =
    member x.Color = color
    member x.NotMovedYet = notMovedYet
    override x.Equals other =
        match other with
        | :? Pawn<'Board, 'Coords> as anotherPawn ->
            color = anotherPawn.Color && notMovedYet = anotherPawn.NotMovedYet
        | _ -> false
    override x.GetHashCode () = HashCode.Combine (color, notMovedYet, "Pawn")
    interface IBaseChessPiece<'Board, 'Coords>
    interface IPiece with
        member x.get_Kind () = "Pawn"
        member x.get_Player () = color
    interface IMovablePiece<'Coords> with
        member x.PossibleMoves (game, startField) =
            game.GetMoves startField |> Seq.map (fun indexedMvCm -> indexedMvCm.Cmd.Dest)
        member x.MakeMove (mutableGame, game, start, dest) =
            let index = game.GetMoves start |> Seq.find (fun indexedMvCm -> indexedMvCm.Cmd.Dest = dest) |> fun ind -> ind.Index
            mutableGame.MakeMove index
    interface ISelfEvaluatingPiece<'Board, 'Coords> with
        member x.Value board coords =
            let pieceValue = 1.0
            let mobility = 
                let standard = standardMove.Invoke (board, coords) |> Option.toArray |> Array.length
                let hit = hitMoves.Invoke (board, coords) |> Seq.length
                let special = 
                    match notMovedYet, specialStartMove.Invoke (board, coords) with
                    | true, Some _ ->
                        1
                    | _ ->
                        0
                let enPassant = 
                    let moves =
                        enPassantCapture.Invoke (board, coords) |> Seq.filter (fun info ->
                            match board.Item info.PossibleOtherPawnPos with
                            | Some (:? Pawn<'Board, 'Coords> as pawn) when pawn.NotMovedYet ->
                                true
                            | _ -> false    
                        ) 
                    moves |> Seq.length
                (float) (standard + hit + special + enPassant) * 0.1           
            pieceValue + mobility
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent> with
        member x.PossibleMoves board coords =
            let nonEnPassant =
                let moves =
                    let standard = standardMove.Invoke (board, coords) |> Option.toList
                    let hit = hitMoves.Invoke (board, coords)
                    let special = 
                        match notMovedYet, specialStartMove.Invoke (board, coords) with
                        | true, Some dest ->
                            [dest]
                        | _ ->
                            []
                    Seq.concat [hit; standard; special] |> Set.ofSeq
                let set = 
                    match whitelist, blacklist with
                    | Some white, Some black ->
                        moves |> Set.intersect (white - black)
                    | Some white, None ->
                        moves |> Set.intersect white
                    | None, Some black ->
                        moves - black
                    | None, None ->
                        moves            
                set |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
            let enPassant =
                let moves =
                    enPassantCapture.Invoke (board, coords) |> Seq.filter (fun info ->
                        match board.Item info.PossibleOtherPawnPos with
                        | Some (:? Pawn<'Board, 'Coords> as pawn) when pawn.NotMovedYet ->
                            true
                        | _ -> false
                    )
                moves |> Seq.map (fun info -> PawnEnPassantCaptureCommand info :> IMoveCommand<'Coords>)
            Seq.concat [nonEnPassant; enPassant]        
        member this.ApplyMove board coords moveCmd =
            match moveCmd with
            | :? PawnEnPassantCaptureCommand<'Coords> as enPassant ->
                let nextBoard = board.GetNext coords None
                let nextNextBoard = nextBoard.GetNext moveCmd.Dest (Some this) 
                let finalBoard = nextNextBoard.GetNext enPassant.Info.PossibleOtherPawnPos None :?> 'Board
                let movingEvent = BoardMovingEvent (coords, moveCmd.Dest) :> IBoardMoveEvent
                let deleteEvent = BoardDestroyedEvent enPassant.Info.PossibleOtherPawnPos
                let events = [movingEvent; deleteEvent]
                finalBoard, moveCmd.Dest, events
            | _ ->
                let transformEvent, nextPiece =
                    if elevationCheck.Invoke coords then
                        let event = BoardTransformedEvent (moveCmd.Dest, queen) :> IBoardMoveEvent |> Some
                        event, queen
                    else
                        let nextPiece =
                            match notMovedYet with
                            | true ->
                                Pawn<'Board, 'Coords> (standardMove, specialStartMove, hitMoves, blackListForKing, enPassantCapture, elevationCheck,
                                color, false, whitelist, blacklist, queen)
                            | false ->
                                this
                        None, nextPiece        
                let nextBoard = board.GetNext coords None
                let finalBoard = nextBoard.GetNext moveCmd.Dest (Some nextPiece) :?> 'Board
                let movingEvent = BoardMovingEvent (coords, moveCmd.Dest) :> IBoardMoveEvent |> Some
                let events = [transformEvent; movingEvent] |> Seq.choose id
                finalBoard, moveCmd.Dest, events
    interface INonKingChessPiece<'Board, 'Coords> with
        member x.BlackListForKing board coords =
            blackListForKing.Invoke (board, coords)
        member x.IsThreateningField board ownPos toCheckPos =
            if hitMoves.Invoke (board, ownPos) |> Seq.exists ((=) toCheckPos) then
                Some (seq [ownPos])
            else
                None    
        member x.AugmentByWhiteList board coords newWhitelist =
            let nextWhitelist =
                match whitelist with
                | Some white ->
                    white |> Set.intersect (newWhitelist |> Set.ofSeq) |> Some
                | None ->
                    newWhitelist |> Set.ofSeq |> Some
            let nextPiece = 
                Pawn<'Board, 'Coords> (standardMove, specialStartMove, hitMoves, blackListForKing, enPassantCapture, elevationCheck, color, notMovedYet,
                nextWhitelist, blacklist, queen) :> IBaseChessPiece<'Board, 'Coords> |> Some
            board.GetNext coords nextPiece :?> 'Board
        member x.AugmentByBlackList board coords newBlackList =
            let nextBlackList =
                match blacklist with
                | Some black ->
                    black |> Set.union (newBlackList |> Set.ofSeq) |> Some
                | None ->                      
                    newBlackList |> Set.ofSeq |> Some
            let nextPiece = 
                Pawn<'Board, 'Coords> (standardMove, specialStartMove, hitMoves, blackListForKing, enPassantCapture, elevationCheck, color, notMovedYet,
                whitelist, nextBlackList, queen) :> IBaseChessPiece<'Board, 'Coords> |> Some
            board.GetNext coords nextPiece :?> 'Board

type King<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IBaseChessPiece<'Board, 'Coords>>> 
    (standardMoves : Func<'Board, 'Coords, seq<'Coords>>, castlingInfos : seq<CastlingInfo<'Coords>>, color : int, notMovedYet, blacklist : Set<'Coords>,
    rook : IBaseChessPiece<'Board, 'Coords>) =
    member x.Color = color
    member x.NotMovedYet = notMovedYet
    override x.Equals other =
        match other with
        | :?  King<'Board, 'Coords> as anotherKing ->
            color = anotherKing.Color && notMovedYet = anotherKing.NotMovedYet
        | _ -> false
    override x.GetHashCode () = HashCode.Combine (color, notMovedYet, "King")    
    interface IBaseChessPiece<'Board, 'Coords>
    interface IPiece with
        member x.get_Kind () = "King"
        member x.get_Player () = color
    interface IMovablePiece<'Coords> with
        member x.PossibleMoves (game, startField) =
            game.GetMoves startField |> Seq.map (fun indexedMvCm -> indexedMvCm.Cmd.Dest)
        member x.MakeMove (mutableGame, game, start, dest) =
            let index = game.GetMoves start |> Seq.find (fun indexedMvCm -> indexedMvCm.Cmd.Dest = dest) |> fun ind -> ind.Index
            mutableGame.MakeMove index
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent> with
        member x.PossibleMoves board coords =
            let standard =
                let moves = standardMoves.Invoke (board, coords)
                let set = (moves |> Set.ofSeq) - blacklist
                set |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
            let castling =
                let infos = castlingInfos |> Seq.filter (fun info -> 
                    let rookNotMovedYet = 
                        match board.Item info.RookStartPos with
                        | Some (:? BlockablePiece<'Board, 'Coords> as posRook) ->
                            posRook.NotMovedYet
                        | _ -> false    
                    let couldBeThreatened = Seq.concat [info.FieldsInBetween; [coords]] |> Set.ofSeq
                    let noThreats = Set.intersect couldBeThreatened blacklist |> Set.isEmpty
                    notMovedYet && rookNotMovedYet && noThreats
                )
                infos |> Seq.map (fun info -> CastlingCommand info :> IMoveCommand<'Coords>)
            Seq.concat [standard; castling]
        member this.ApplyMove board coords moveCmd =
            let nextKing =
                match notMovedYet with
                | true ->
                    King<'Board, 'Coords> (standardMoves, castlingInfos, color, false, Set.empty, rook)
                | false ->
                    this    
            match moveCmd with
            | :? CastlingCommand<'Coords> as cstCmd ->                       
                let nextBoard = board.GetNext coords None
                let nextNextBoard = nextBoard.GetNext cstCmd.Info.KingDestPos (Some nextKing)
                let nextNextNextBoard = nextNextBoard.GetNext cstCmd.Info.RookStartPos None
                let finalBoard = nextNextNextBoard.GetNext cstCmd.Info.RookDestPos (Some rook) :?> 'Board
                let event = BoardCastlingEvent (coords, cstCmd.Info)
                finalBoard, cstCmd.Info.KingDestPos, [event]
            | _ ->
                let nextBoard = board.GetNext coords None
                let finalBoard = nextBoard.GetNext moveCmd.Dest (Some nextKing) :?> 'Board
                let movingEvent = BoardMovingEvent (coords, moveCmd.Dest)
                finalBoard, moveCmd.Dest, seq [movingEvent]
    interface IKing<'Board, 'Coords> with
        member this.AugmentByBlackList board coords newBlacklist =
            let combinedBlackList = Set.union blacklist (newBlacklist |> Set.ofSeq) 
            let newKing = King<'Board, 'Coords> (standardMoves, castlingInfos, color, notMovedYet, combinedBlackList, rook)
            board.GetNext coords (Some newKing) :?> 'Board