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

type CastlingInfo<'Coords> = {KingDestPos : 'Coords; RookStartPos : 'Coords; RookDestPos : 'Coords; FieldsInBetween : seq<'Coords>}    

type MoveCommand<'Coords> (dest : 'Coords) =
    interface IMoveCommand<'Coords> with
        member x.Dest = dest

type CastlingCommand<'Coords> (info : CastlingInfo<'Coords>) =
    member x.Info = info
    interface IMoveCommand<'Coords> with
        member x.Dest = info.KingDestPos

type PawnSpecialMoveCommand<'Coords> (dest : 'Coords) =
    inherit MoveCommand<'Coords> (dest)

type IRook =
    abstract NotMovedYet : bool

type King<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IBaseChessPiece<'Board, 'Coords>>> 
    (standardMoves : Func<'Board, 'Coords, seq<'Coords>>, castlingInfos : seq<CastlingInfo<'Coords>>, color : int, notMovedYet, blacklist : Set<'Coords>) =
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
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardInformerEvent> with
        member x.PossibleMoves board coords =
            let standard =
                let moves = standardMoves.Invoke (board, coords)
                let set = (moves |> Set.ofSeq) - blacklist
                set |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
            let castling =
                let infos = castlingInfos |> Seq.filter (fun info -> 
                    let rookNotMovedYet = 
                        match board.Item info.RookStartPos with
                        | Some (:? IRook as rook) ->
                            rook.NotMovedYet
                        | _ -> false    
                    let couldBeThreatened = Seq.concat [info.FieldsInBetween; [coords]] |> Set.ofSeq
                    let noThreats = Set.intersect couldBeThreatened blacklist |> Set.isEmpty
                    notMovedYet && rookNotMovedYet && noThreats
                )
                infos |> Seq.map (fun info -> CastlingCommand info :> IMoveCommand<'Coords>)
            Seq.concat [standard; castling]
        member this.ApplyMove board coords moveCmd =
            match moveCmd with
            | :? CastlingCommand<'Coords> as cstCmd ->
                let nextBoard = board.GetNext coords None
                let nextNextBoard = nextBoard.GetNext cstCmd.Info.KingDestPos (Some this)
                let nextNextNextBoard = nextNextBoard.GetNext cstCmd.Info.RookStartPos None
                let finalBoard = nextNextNextBoard.GetNext cstCmd.Info.RookDestPos (board.Item cstCmd.Info.RookStartPos) :?> 'Board
                let movingEvent1 = BoardInformerMovingEvent (coords, cstCmd.Info.KingDestPos) :> IBoardInformerEvent
                let movingEvent2 = BoardInformerMovingEvent (cstCmd.Info.RookStartPos, cstCmd.Info.RookDestPos) :> IBoardInformerEvent
                let movingEvents = seq [movingEvent1; movingEvent2]
                finalBoard, cstCmd.Info.KingDestPos, movingEvents
            | _ ->
                let nextBoard = board.GetNext coords None
                let finalBoard = nextBoard.GetNext moveCmd.Dest (Some this) :?> 'Board
                let movingEvent = BoardInformerMovingEvent (coords, moveCmd.Dest)
                finalBoard, moveCmd.Dest, seq [movingEvent]
    interface IKing<'Board, 'Coords> with
        member this.AugmentByBlackList board coords newBlacklist =
            let combinedBlackList = Set.union blacklist (newBlacklist |> Set.ofSeq) 
            let newKing = King<'Board, 'Coords> (standardMoves, castlingInfos, color, false, combinedBlackList)
            board.GetNext coords (Some newKing) :?> 'Board

type Knight<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IBaseChessPiece<'Board, 'Coords>>> 
    (possibleMoves : Func<'Board, 'Coords, seq<'Coords>>, threateningTest  castlingInfos : seq<CastlingInfo<'Coords>>, color : int, notMovedYet, blacklist : Set<'Coords>) =
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
    interface ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardInformerEvent> with
        member x.PossibleMoves board coords =
            let standard =
                let moves = standardMoves.Invoke (board, coords)
                let set = (moves |> Set.ofSeq) - blacklist
                set |> Seq.map (fun move -> MoveCommand move :> IMoveCommand<'Coords>)
            let castling =
                let infos = castlingInfos |> Seq.filter (fun info -> 
                    let rookNotMovedYet = 
                        match board.Item info.RookStartPos with
                        | Some (:? IRook as rook) ->
                            rook.NotMovedYet
                        | _ -> false    
                    let couldBeThreatened = Seq.concat [info.FieldsInBetween; [coords]] |> Set.ofSeq
                    let noThreats = Set.intersect couldBeThreatened blacklist |> Set.isEmpty
                    notMovedYet && rookNotMovedYet && noThreats
                )
                infos |> Seq.map (fun info -> CastlingCommand info :> IMoveCommand<'Coords>)
            Seq.concat [standard; castling]
        member this.ApplyMove board coords moveCmd =
            match moveCmd with
            | :? CastlingCommand<'Coords> as cstCmd ->
                let nextBoard = board.GetNext coords None
                let nextNextBoard = nextBoard.GetNext cstCmd.Info.KingDestPos (Some this)
                let nextNextNextBoard = nextNextBoard.GetNext cstCmd.Info.RookStartPos None
                let finalBoard = nextNextNextBoard.GetNext cstCmd.Info.RookDestPos (board.Item cstCmd.Info.RookStartPos) :?> 'Board
                let movingEvent1 = BoardInformerMovingEvent (coords, cstCmd.Info.KingDestPos) :> IBoardInformerEvent
                let movingEvent2 = BoardInformerMovingEvent (cstCmd.Info.RookStartPos, cstCmd.Info.RookDestPos) :> IBoardInformerEvent
                let movingEvents = seq [movingEvent1; movingEvent2]
                finalBoard, cstCmd.Info.KingDestPos, movingEvents
            | _ ->
                let nextBoard = board.GetNext coords None
                let finalBoard = nextBoard.GetNext moveCmd.Dest (Some this) :?> 'Board
                let movingEvent = BoardInformerMovingEvent (coords, moveCmd.Dest)
                finalBoard, moveCmd.Dest, seq [movingEvent]
    interface IKing<'Board, 'Coords> with
        member this.AugmentByBlackList board coords newBlacklist =
            let combinedBlackList = Set.union blacklist (newBlacklist |> Set.ofSeq) 
            let newKing = King<'Board, 'Coords> (standardMoves, castlingInfos, color, false, combinedBlackList)
            board.GetNext coords (Some newKing) :?> 'Board

