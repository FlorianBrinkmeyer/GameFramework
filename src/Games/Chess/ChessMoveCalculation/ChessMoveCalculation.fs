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

module GameFramework.Chess.MoveCalculation

open System
open Chess
open GameFramework

let calculatePossibleMoves<'Board, 'Coords when 'Coords :> IComparable and 'Coords : comparison and 'Board :> ImmutableArray<'Coords, IPiece>>
    (board : 'Board) activePlayer (maybePositionOfLastMovedPiece : Option<'Coords>) =              
        let ownKingPos, ownKing = 
            board.KeyValuePairs |> Seq.pick (fun (pos, piece) -> 
                match piece with
                | :? IKing<'Board, 'Coords> as king when king.Player = activePlayer ->
                    Some (pos, king)
                | _ -> None
            )
        let ownNonKingPieces =
            board.KeyValuePairs |> Seq.choose (fun (pos, piece) ->
                match piece with
                | :? INonKingChessPiece<'Board, 'Coords> as nonKingPiece when piece.Player = activePlayer ->
                    Some (pos, nonKingPiece)
                | _ -> None
            )    
        let otherNonKingPieces = 
            board.KeyValuePairs |> Seq.choose (fun (pos, piece) -> 
                match piece with
                | :? INonKingChessPiece<'Board, 'Coords> as nonKingPiece when piece.Player <> activePlayer ->
                    Some (pos, nonKingPiece)
                | _ -> None   
            ) 
        let boardWithoutKing = board.GetNext ownKingPos None :?> 'Board
        let kingBlackList = 
            otherNonKingPieces |> Seq.map (fun (pos, piece) -> piece.BlackListForKing boardWithoutKing pos |> Set.ofSeq) 
                |> Seq.fold (fun set1 set2 -> Set.union set1 set2) Set.empty
        let boardWithBlackListKing = ownKing.AugmentByBlackList board ownKingPos kingBlackList       
        let otherBlockablePieces = 
            otherNonKingPieces |> Seq.choose (fun (pos, piece) -> 
                match piece with
                | :? IBlockableChessPiece<'Board, 'Coords> as blockablePiece ->
                    Some (pos, blockablePiece)
                | _ -> None    
            ) 
        let castedOtherBlockablePieces = otherBlockablePieces |> Seq.map (fun (pos, piece) -> pos, piece :> INonKingChessPiece<'Board, 'Coords>)
        let maybeLastMovedOtherPiece = 
            maybePositionOfLastMovedPiece |> Option.map (fun positionOfLastMovedPiece -> (board.Item positionOfLastMovedPiece).Value)                   
        let allPotentiallyKingThreateningPieces =
            match maybePositionOfLastMovedPiece, maybeLastMovedOtherPiece with
            | None, None | Some _, Some (:? IKing<'Board, 'Coords>) ->
                castedOtherBlockablePieces
            | Some positionOfLastMovedPiece, Some (:? INonKingChessPiece<'Board, 'Coords> as nonKingPiece) ->     
                Seq.append castedOtherBlockablePieces [(positionOfLastMovedPiece, nonKingPiece)]
            | _ -> raise (Exception "Non-chess piece on board.")    
        let kingThreats =
            let threatsAndNeutralizing = allPotentiallyKingThreateningPieces |> Seq.choose (fun (pos, piece) -> 
                match piece.IsThreateningField board pos ownKingPos with
                | Some fields ->
                    Some (pos, fields)
                | None -> None  
            )
            if threatsAndNeutralizing |> Seq.isEmpty then
                None
            else     
                let threats, lists = threatsAndNeutralizing |> Seq.toList |> List.unzip
                let whitelist = lists |> List.map Set.ofSeq |> List.reduce (fun set1 set2 -> Set.intersect set1 set2)
                Some (threats, whitelist)
        match kingThreats with
        | Some (kingThreatCoords, threatNeutralizingWhiteList) ->
            let boardWithWhiteListPiecesAndBlackListKing = 
                ownNonKingPieces |> Seq.fold (fun nextBoard (pos, piece) -> 
                    piece.AugmentByWhiteList nextBoard pos threatNeutralizingWhiteList
                ) boardWithBlackListKing
            let allNewOwnPieces = 
                boardWithWhiteListPiecesAndBlackListKing.KeyValuePairs |> Seq.filter (fun (_, piece) -> piece.Player = activePlayer)
            let possibleMoves =
                let commands = 
                    allNewOwnPieces |> Seq.collect (fun (pos, piece) -> 
                        let castedPiece = piece :?> ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent>
                        castedPiece.PossibleMoves boardWithWhiteListPiecesAndBlackListKing pos |> Seq.map (fun moveCom -> pos, moveCom)
                    )
                commands |> Seq.mapi (fun index (pos, com) -> {Index = index; StartField = pos; Cmd = com}) |> Seq.toArray
            let additionalEvent = {CheckedPlayer = activePlayer; KingPos = ownKingPos; CheckedBy = kingThreatCoords} :> IBoardMoveEvent
            let moveCalcResult =
                if possibleMoves.Length = 0 then
                    GameOverZSValue (Double.PositiveInfinity * (float) activePlayer * (float) (-1))
                else
                    PossibleMoves possibleMoves
            moveCalcResult, seq [additionalEvent]                
        | None ->
            let boardWithAppliedLists = 
                otherBlockablePieces |> Seq.fold (fun nextBoard (pos, piece) -> 
                    piece.PotentiallyAugmentBlockingPieceByLists nextBoard pos ownKingPos
                ) boardWithBlackListKing
            let allNewOwnPieces = 
                boardWithAppliedLists.KeyValuePairs |> Seq.filter (fun (_, piece) -> piece.Player = activePlayer)
            let possibleMoves = 
                let commands = 
                    allNewOwnPieces |> Seq.collect (fun (pos, piece) -> 
                        let castedPiece = piece :?> ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent>                       
                        castedPiece.PossibleMoves boardWithAppliedLists pos |> Seq.map (fun moveCom -> pos, moveCom)
                    )
                commands |> Seq.mapi (fun index (pos, moveCom) -> {Index = index; StartField = pos; Cmd = moveCom}) |> Seq.toArray
            let moveCalcResult =
                if possibleMoves.Length = 0 then
                    GameOverZSValue 0.0
                else
                    PossibleMoves possibleMoves
            moveCalcResult, Seq.empty            