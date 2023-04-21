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
open System.Collections

type BasicChessPiece<'Coords, 'State when 'Coords :> IComparable and 'Coords : comparison and 'State : equality>
    (color : int, kind : String, maybeAdditionalState : Option<'State>,  whitelist : Option<Set<'Coords>>, blacklist : Option<Set<'Coords>>) =
    member x.Color = color
    member x.Kind = kind
    member x.MaybeAdditionalState = maybeAdditionalState
    member x.ApplyWhiteAndBlackList (moves : seq<'Coords>) =
        let movesSet = moves |> Set.ofSeq
        if movesSet.IsEmpty then
            Set.empty
        else
            match whitelist, blacklist with
            | Some white, Some black ->
                movesSet |> Set.intersect (white - black)
            | Some white, None ->
                movesSet |> Set.intersect white
            | None, Some black ->
                movesSet - black
            | None, None ->
                movesSet
    member x.NextWhiteList additionalWhitelist =
        match whitelist with
        | Some white ->
            white |> Set.intersect (additionalWhitelist |> Set.ofSeq) |> Some
        | None ->
            additionalWhitelist |> Set.ofSeq |> Some
    member x.NextBlackList additionalBlacklist =
        match blacklist with
        | Some black ->
            black |> Set.union (additionalBlacklist |> Set.ofSeq) |> Some
        | None ->
            additionalBlacklist |> Set.ofSeq |> Some
    member x.CompareTo (other : Object) =
        let thisPieceInfo = (color, kind) :> IComparable
        let otherPiece = other :?> IPiece
        let otherPieceInfo = (otherPiece.Player, otherPiece.Kind) :> IComparable
        thisPieceInfo.CompareTo otherPieceInfo
    override x.Equals other =
        match other with
        | :? BasicChessPiece<'Coords, 'State> as anotherPiece ->
            color = anotherPiece.Color && kind = anotherPiece.Kind && maybeAdditionalState = anotherPiece.MaybeAdditionalState
        | _ -> false
    override x.GetHashCode () = HashCode.Combine (color, maybeAdditionalState, kind)
    override x.ToString () = kind + (color.ToString ())
    interface IPiece with
        member x.get_Kind () = kind
        member x.get_Player () = color
    interface IMovablePiece<'Coords> with
        member x.PossibleMoves (game, startField) =
            game.GetMoves startField |> Seq.map (fun indexedMvCm -> indexedMvCm.Cmd.Dest)
        member x.MakeMove (mutableGame, game, start, dest) =
            let index = game.GetMoves start |> Seq.find (fun indexedMvCm -> indexedMvCm.Cmd.Dest = dest) |> fun ind -> ind.Index
            mutableGame.MakeMove index
    interface IComparable with
        member x.CompareTo other =
            x.CompareTo other

type StandardNonBlockablePiece<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IPiece>> 
    (possibleMoves : Func<'Board, 'Coords, seq<'Coords>>, blackListForKing : Func<'Board, 'Coords, seq<'Coords>>,
    threateningTest : Func<'Coords, 'Coords, bool>, color : int, kind : String, pieceValue : float, whitelist : Option<Set<'Coords>>,
    blacklist : Option<Set<'Coords>>) =
    inherit BasicChessPiece<'Coords, unit> (color, kind, None, whitelist, blacklist)
    interface IPiece with
        member x.get_Kind () = kind
        member x.get_Player () = color
    interface ISelfEvaluatingPiece<'Board, 'Coords> with
        member x.Value board coords =
            let mobility = (float) (possibleMoves.Invoke (board, coords) |> Seq.length) * 0.1
            pieceValue + mobility
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent> with
        member x.PossibleMoves board coords =
            let moves = possibleMoves.Invoke (board, coords) |> x.ApplyWhiteAndBlackList
            moves |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
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
        member x.AugmentByWhiteList board coords addWhitelist =
            let nextWhitelist = x.NextWhiteList addWhitelist
            let nextPiece = 
                StandardNonBlockablePiece<'Board, 'Coords>(possibleMoves, blackListForKing, threateningTest, color, kind, pieceValue, nextWhitelist,
                    blacklist) :> IPiece |> Some
            board.GetNext coords nextPiece :?> 'Board
        member x.AugmentByBlackList board coords addBlackList =
            let nextBlackList = x.NextBlackList addBlackList
            let nextPiece = 
                StandardNonBlockablePiece<'Board, 'Coords>(possibleMoves, blackListForKing, threateningTest, color, kind, pieceValue, whitelist,
                    nextBlackList) :> IPiece |> Some
            board.GetNext coords nextPiece :?> 'Board

type BlockablePiece<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IPiece>> 
    (possibleMoves : Func<'Board, 'Coords, seq<'Coords>>, blackListForKing : Func<'Board, 'Coords, seq<'Coords>>,
    threateningTest : Func<'Board, 'Coords, 'Coords, Option<seq<'Coords>>>, 
    getWhiteListAndPiece : Func<'Board, 'Coords, 'Coords, Option<seq<'Coords> * (IPiece * 'Coords)>>, 
    color : int, kind : String, pieceValue : float, maybeNotMovedYet, whitelist : Option<Set<'Coords>>, blacklist : Option<Set<'Coords>>,
    piecePool : Generic.IDictionary<String * int * Object, Object>) =
    inherit BasicChessPiece<'Coords, Option<bool>> (color, kind, Some maybeNotMovedYet, whitelist, blacklist)
    member x.MaybeNotMovedYet = maybeNotMovedYet
    interface IPiece with
        member x.get_Kind () = kind
        member x.get_Player () = color
    interface ISelfEvaluatingPiece<'Board, 'Coords> with
        member x.Value board coords =
            let mobility = (float) (possibleMoves.Invoke (board, coords) |> Seq.length) * 0.1
            pieceValue + mobility
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent> with
        member x.PossibleMoves board coords =
            let moves = possibleMoves.Invoke (board, coords) |> x.ApplyWhiteAndBlackList
            moves |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
        member this.ApplyMove board coords moveCmd =
            let nextPiece =
                match maybeNotMovedYet with
                | Some true ->
                    let key = kind, color, Some false :> Object
                    match piecePool.TryGetValue key with
                    | true, value ->
                        value :?> BlockablePiece<'Board, 'Coords>
                    | _ ->    
                        let result =
                            BlockablePiece<'Board, 'Coords> (possibleMoves, blackListForKing, threateningTest, getWhiteListAndPiece, color, kind, pieceValue,
                            Some false, whitelist, blacklist, piecePool)
                        piecePool[key] <- result
                        result
                | _ ->
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
        member x.AugmentByWhiteList board coords addWhitelist =
            let nextWhitelist = x.NextWhiteList addWhitelist
            let nextPiece = 
                BlockablePiece<'Board, 'Coords>(possibleMoves, blackListForKing, threateningTest, getWhiteListAndPiece, color, kind, pieceValue,
                maybeNotMovedYet, nextWhitelist, blacklist, piecePool) :> IPiece |> Some
            board.GetNext coords nextPiece :?> 'Board
        member x.AugmentByBlackList board coords addBlackList =
            let nextBlackList = x.NextBlackList addBlackList
            let nextPiece = 
                BlockablePiece<'Board, 'Coords>(possibleMoves, blackListForKing, threateningTest, getWhiteListAndPiece, color, kind, pieceValue,
                maybeNotMovedYet, whitelist, nextBlackList, piecePool) :> IPiece |> Some
            board.GetNext coords nextPiece :?> 'Board
    interface IBlockableChessPiece<'Board, 'Coords> with
        member x.PotentiallyAugmentBlockingPieceByLists board ownPos toCheckPos =
            match getWhiteListAndPiece.Invoke (board, ownPos, toCheckPos) with
            | Some (whitelist, (:? INonKingChessPiece<'Board, 'Coords> as piece, piecePos)) ->
                piece.AugmentByWhiteList board piecePos whitelist
            | _ -> board
    override x.Equals other = base.Equals other
    override x.GetHashCode () = base.GetHashCode ()
    interface IComparable with
        member x.CompareTo other =
            match other with
            | :? BlockablePiece<'Board, 'Coords> as otherPiece when maybeNotMovedYet.IsSome && otherPiece.MaybeNotMovedYet.IsSome ->
                let thisPieceInfo = (color, kind, maybeNotMovedYet.Value) :> IComparable
                let otherPieceInfo = (otherPiece.Color, otherPiece.Kind, otherPiece.MaybeNotMovedYet.Value) :> IComparable
                thisPieceInfo.CompareTo otherPieceInfo
            | _ ->
                base.CompareTo other            

type Pawn<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IPiece>> 
    (standardMove : Func<'Board, 'Coords, Option<'Coords>>, specialStartMove : Func<'Board, 'Coords, Option<'Coords>>,
    hitMoves : Func<'Board, 'Coords, seq<'Coords>>, blackListForKing : Func<'Board, 'Coords, seq<'Coords>>,
    enPassantCapture : Func<'Board, 'Coords, seq<EnPassantInfo<'Coords>>>, elevationCheck : Func<'Coords, bool>, color : int, madeMoves,
    whitelist : Option<Set<'Coords>>, blacklist : Option<Set<'Coords>>, queen : IPiece, piecePool : Generic.IDictionary<String * int * Object, Object>) =
    inherit BasicChessPiece<'Coords, int> (color, "Pawn", Some (if madeMoves >= 2 then 2 else madeMoves), whitelist, blacklist)
    member x.MadeMoves = madeMoves
    interface IPiece with
        member x.get_Kind () = "Pawn"
        member x.get_Player () = color
    interface ISelfEvaluatingPiece<'Board, 'Coords> with
        member x.Value board coords =
            let pieceValue = 1.0
            let mobility = 
                let standard = standardMove.Invoke (board, coords) |> Option.count
                let hit = hitMoves.Invoke (board, coords) |> Seq.length
                let special = 
                    match madeMoves, specialStartMove.Invoke (board, coords) with
                    | 0, Some _ ->
                        1
                    | _ ->
                        0
                let enPassant = 
                    let moves =
                        enPassantCapture.Invoke (board, coords) |> Seq.filter (fun info ->
                            match board.Item info.PossibleOtherPawnPos with
                            | Some (:? Pawn<'Board, 'Coords> as pawn) when pawn.MadeMoves = 1 ->
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
                        match madeMoves, specialStartMove.Invoke (board, coords) with
                        | 0, Some dest ->
                            [dest]
                        | _ ->
                            []
                    Seq.concat [hit; standard; special]
                moves |> x.ApplyWhiteAndBlackList |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)              
            let enPassant =
                let infos =
                    enPassantCapture.Invoke (board, coords) |> Seq.filter (fun info ->
                        match board.Item info.PossibleOtherPawnPos with
                        | Some (:? Pawn<'Board, 'Coords> as pawn) when pawn.MadeMoves = 1 ->
                            true
                        | _ -> false
                    )
                let moves = infos |> Seq.map (fun info -> info.PossibleOwnDestPos) |> x.ApplyWhiteAndBlackList
                infos |> Seq.filter (fun info -> moves |> Set.contains info.PossibleOwnDestPos) 
                    |> Seq.map (fun info -> PawnEnPassantCaptureCommand info :> IMoveCommand<'Coords>)
            Seq.append nonEnPassant enPassant        
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
                    if elevationCheck.Invoke moveCmd.Dest then
                        let event = BoardTransformedEvent (moveCmd.Dest, queen) :> IBoardMoveEvent |> Some
                        event, queen
                    else
                        let nextPiece =
                            if madeMoves >= 2 then
                                this
                            else
                                 let key = "Pawn", color, madeMoves + 1 :> Object
                                 match piecePool.TryGetValue key with
                                 | true, value ->
                                     value :?> Pawn<'Board, 'Coords>
                                 | _ ->    
                                     let result =
                                        Pawn<'Board, 'Coords> (standardMove, specialStartMove, hitMoves, blackListForKing, enPassantCapture, elevationCheck,
                                        color, madeMoves + 1, whitelist, blacklist, queen, piecePool)
                                     piecePool[key] <- result
                                     result                                                             
                        None, nextPiece        
                let nextBoard = board.GetNext coords None
                let finalBoard = nextBoard.GetNext moveCmd.Dest (Some nextPiece) :?> 'Board
                let movingEvent = BoardMovingEvent (coords, moveCmd.Dest) :> IBoardMoveEvent |> Some
                let events = [movingEvent; transformEvent] |> Seq.choose id
                finalBoard, moveCmd.Dest, events
    interface INonKingChessPiece<'Board, 'Coords> with
        member x.BlackListForKing board coords =
            blackListForKing.Invoke (board, coords)
        member x.IsThreateningField board ownPos toCheckPos =
            if hitMoves.Invoke (board, ownPos) |> Seq.exists ((=) toCheckPos) then
                Some (seq [ownPos])
            else
                None    
        member x.AugmentByWhiteList board coords addWhitelist =
            let nextWhitelist = x.NextWhiteList addWhitelist
            let nextPiece =                
                Pawn<'Board, 'Coords> (standardMove, specialStartMove, hitMoves, blackListForKing, enPassantCapture, elevationCheck, color, madeMoves,
                nextWhitelist, blacklist, queen, piecePool) :> IPiece |> Some
            board.GetNext coords nextPiece :?> 'Board
        member x.AugmentByBlackList board coords addBlackList =
            let nextBlackList = x.NextBlackList addBlackList
            let nextPiece = 
                Pawn<'Board, 'Coords> (standardMove, specialStartMove, hitMoves, blackListForKing, enPassantCapture, elevationCheck, color, madeMoves,
                whitelist, nextBlackList, queen, piecePool) :> IPiece |> Some
            board.GetNext coords nextPiece :?> 'Board
    override x.Equals other = base.Equals other
    override x.GetHashCode () = base.GetHashCode ()
    interface IComparable with
        member x.CompareTo other =
            match other with
            | :? Pawn<'Board, 'Coords> as otherPawn ->
                let thisPawnInfo = (color, madeMoves) :> IComparable
                let otherPawnInfo = (otherPawn.Color, otherPawn.MadeMoves) :> IComparable
                thisPawnInfo.CompareTo otherPawnInfo
            | _ ->
                base.CompareTo other            

type King<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IPiece>> 
    (standardMoves : Func<'Board, 'Coords, seq<'Coords>>, blackListForKing : Func<'Board, 'Coords, seq<'Coords>>, 
    castlingInfos : seq<CastlingInfo<'Coords>>, color : int, notMovedYet, blacklist : Set<'Coords>, rook : BlockablePiece<'Board, 'Coords>,
    piecePool : Generic.IDictionary<String * int * Object, Object>) =
    inherit BasicChessPiece<'Coords, bool> (color, "King", Some notMovedYet, None, Some blacklist)
    member x.NotMovedYet = notMovedYet
    interface IPiece with
        member x.get_Kind () = "King"
        member x.get_Player () = color
    interface ISelfEvaluatingPiece<'Board, 'Coords> with
        member x.Value board coords =
            let standard =
                standardMoves.Invoke (board, coords) |> Seq.length
            let castling =
                let infos = castlingInfos |> Seq.filter (fun info -> 
                    let rookNotMovedYet () = 
                        match board.Item info.RookStartPos with
                        | Some (:? BlockablePiece<'Board, 'Coords> as posRook) ->
                            match posRook.MaybeNotMovedYet with
                            | Some true ->
                                true
                            | _ ->
                                false    
                        | _ -> false    
                    notMovedYet && rookNotMovedYet ()
                )
                infos |> Seq.length
            let mobility = (float) (standard + castling) * 0.1
            mobility
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent> with
        member x.PossibleMoves board coords =
            let standard =
                let moves = standardMoves.Invoke (board, coords)
                moves |> x.ApplyWhiteAndBlackList |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
            let castling =
                let infos = castlingInfos |> Seq.filter (fun info -> 
                    let rookNotMovedYet () = 
                        match board.Item info.RookStartPos with
                        | Some (:? BlockablePiece<'Board, 'Coords> as posRook) ->
                            match posRook.MaybeNotMovedYet with
                            | Some true ->
                                true
                            | _ ->
                                false    
                        | _ -> false    
                    let fieldsAreEmpty () = 
                        Seq.concat [info.AditionallyUnthreatenedFields; info.AditionallyEmptyFields;] |> Seq.forall (fun coords -> (board.Item coords).IsNone)
                    let noThreats () =
                        let couldBeThreatened = Seq.append info.AditionallyUnthreatenedFields [coords] |> Set.ofSeq
                        Set.intersect couldBeThreatened blacklist |> Set.isEmpty
                    notMovedYet && fieldsAreEmpty () && rookNotMovedYet () && noThreats ()
                )
                infos |> Seq.map (fun info -> CastlingCommand info :> IMoveCommand<'Coords>)
            Seq.append standard castling
        member this.ApplyMove board coords moveCmd =
            let nextKing =
                match notMovedYet with
                | true ->
                    let key = "King", color, false :> Object
                    match piecePool.TryGetValue key with
                    | true, value ->
                        value :?> King<'Board, 'Coords>
                    | _ ->    
                        let result =
                            King<'Board, 'Coords> (standardMoves, blackListForKing, castlingInfos, color, false, blacklist, rook, piecePool)
                        piecePool[key] <- result
                        result                   
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
                finalBoard, moveCmd.Dest, [movingEvent]
    interface IKing<'Board, 'Coords> with
        member x.BlackListForKing board coords =
            blackListForKing.Invoke (board, coords)
        member x.AugmentByBlackList board coords addBlacklist =
            let newBlackList = Set.union blacklist (addBlacklist |> Set.ofSeq) 
            let newKing = King<'Board, 'Coords> (standardMoves, blackListForKing, castlingInfos, color, notMovedYet, newBlackList, rook, piecePool)
            board.GetNext coords (Some newKing) :?> 'Board
    override x.Equals other = base.Equals other
    override x.GetHashCode () = base.GetHashCode ()
    interface IComparable with
        member x.CompareTo other =
            match other with
            | :? King<'Board, 'Coords> as otherKing ->
                let thisKingInfo = (color, notMovedYet) :> IComparable
                let otherKingInfo = (otherKing.Color, otherKing.NotMovedYet) :> IComparable
                thisKingInfo.CompareTo otherKingInfo
            | _ ->
                base.CompareTo other                      