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
    (board : 'Board) activePlayer nonHitMovesInRow (maybePositionOfLastMovedPiece : Option<'Coords>) =              
    let lastMoveIsHitMove =
        match board.Previous, maybePositionOfLastMovedPiece with
        | Some previousBoard, Some pos ->
            previousBoard.Item pos |> Option.isSome
        | _ -> false
    let nextNonHitMovesInRow =
        let pawnMoved =
            match maybePositionOfLastMovedPiece with
            | Some pos ->
                match board.Item pos with
                | Some piece ->
                    piece.Kind = "Pawn"
                | None -> false      
            | None -> false
        if lastMoveIsHitMove || pawnMoved then
            0
        else
            nonHitMovesInRow + 1        
    if nextNonHitMovesInRow < 50 then
        let ownKingPos, ownKing = 
            let maybePosAndKing=
                board.KeyValuePairs |> Seq.tryPick (fun (pos, piece) -> 
                    match piece with
                    | :? IKing<'Board, 'Coords> as king when king.Player = activePlayer ->
                        Some (pos, king)
                    | _ -> None
                )
            match maybePosAndKing with
            | Some (pos, king) ->
                pos, king
            | None ->
                raise (InvalidOperationException "No king on board: This shouldn't happen")        
        let otherPieces = board.KeyValuePairs |> Seq.filter (fun (_, piece) -> piece.Player <> activePlayer) |> Seq.toList
        let boardWithoutKing = board.GetNext ownKingPos None :?> 'Board
        let kingBlackList = 
            otherPieces |> List.map (fun (pos, piece) -> (piece :?> IChessPiece<'Board, 'Coords>).BlackListForKing boardWithoutKing pos |> Set.ofSeq) 
                |> Seq.fold (fun set1 set2 -> Set.union set1 set2) Set.empty
        let boardWithKingBlackList = ownKing.AugmentByBlackList board ownKingPos kingBlackList       
        let otherBlockablePieces = 
            otherPieces |> List.choose (fun (pos, piece) -> 
                match piece with
                | :? IBlockableChessPiece<'Board, 'Coords> as blockablePiece ->
                    Some (pos, blockablePiece)
                | _ -> None    
            ) 
        let boardWithKingBlackListAndNewlyCheckPreventingWhiteLists = 
            otherBlockablePieces |> List.fold (fun nextBoard (pos, piece) -> 
                piece.PotentiallyAugmentBlockingPieceByLists nextBoard pos ownKingPos
            ) boardWithKingBlackList       
        let castedOtherBlockablePieces = otherBlockablePieces |> List.map (fun (pos, piece) -> pos, piece :> INonKingChessPiece<'Board, 'Coords>)
        let maybeLastMovedOtherPiece = 
            maybePositionOfLastMovedPiece |> Option.map (fun positionOfLastMovedPiece -> (board.Item positionOfLastMovedPiece).Value)                   
        let allPotentiallyKingThreateningPieces =
            match maybePositionOfLastMovedPiece, maybeLastMovedOtherPiece with
            | None, None | Some _, Some (:? IKing<'Board, 'Coords>) ->
                castedOtherBlockablePieces
            | Some positionOfLastMovedPiece, Some (:? INonKingChessPiece<'Board, 'Coords> as nonKingPiece) ->     
                (positionOfLastMovedPiece, nonKingPiece) :: castedOtherBlockablePieces 
            | _ -> raise (InvalidOperationException "Non-chess piece on board.")    
        let kingThreats =
            let threatsAndNeutralizing = allPotentiallyKingThreateningPieces |> List.choose (fun (pos, piece) -> 
                match piece.IsThreateningField board pos ownKingPos with
                | Some fields ->
                    Some (pos, fields)
                | None -> None  
            )
            match threatsAndNeutralizing with
            | [] ->
                None
            | _ ->     
                let threats, lists = threatsAndNeutralizing |> List.unzip
                let whitelist = lists |> List.map Set.ofSeq |> List.reduce (fun set1 set2 -> Set.intersect set1 set2)
                Some (threats |> Set.ofList, whitelist)
        match kingThreats with
        | Some (kingThreatCoords, threatNeutralizingWhiteList) ->
            let ownNonKingPieces = boardWithKingBlackListAndNewlyCheckPreventingWhiteLists.KeyValuePairs |> Seq.choose (fun (pos, piece) -> 
                match piece with
                | :? INonKingChessPiece<'Board, 'Coords> as nonKingPiece when piece.Player = activePlayer ->
                    Some (pos, nonKingPiece)
                | _ -> None    
            )
            let boardWithCheckEndingWhiteList = 
                ownNonKingPieces |> Seq.fold (fun nextBoard (pos, piece) -> 
                    piece.AugmentByWhiteList nextBoard pos threatNeutralizingWhiteList
                ) boardWithKingBlackListAndNewlyCheckPreventingWhiteLists
            let allNewOwnPieces = 
                boardWithCheckEndingWhiteList.KeyValuePairs |> Seq.filter (fun (_, piece) -> piece.Player = activePlayer)
            let possibleMoves =
                let commands = 
                    allNewOwnPieces |> Seq.collect (fun (pos, piece) -> 
                        let castedPiece = piece :?> ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent>
                        castedPiece.PossibleMoves board pos |> Seq.map (fun moveCom -> pos, moveCom)
                    )
                commands |> Seq.mapi (fun index (pos, com) -> {Index = index; StartField = pos; Cmd = com}) |> Seq.toArray
            let additionalEvent = {CheckedPlayer = activePlayer; KingPos = ownKingPos; CheckedBy = kingThreatCoords} :> IBoardMoveEvent
            let moveCalcResult =
                if possibleMoves.Length = 0 then
                    GameOverZSValue (Double.PositiveInfinity * (float) (activePlayer * (-1)))
                else
                    PossibleMoves possibleMoves
            moveCalcResult, seq [additionalEvent], nextNonHitMovesInRow, true                
        | None ->
            let allNewOwnPieces = 
                boardWithKingBlackListAndNewlyCheckPreventingWhiteLists.KeyValuePairs |> Seq.filter (fun (_, piece) -> piece.Player = activePlayer)
            let possibleMoves = 
                let commands = 
                    allNewOwnPieces |> Seq.collect (fun (pos, piece) -> 
                        let castedPiece = piece :?> ISelfCalculatingPiece<'Board, 'Coords, IMoveCommand<'Coords>, IBoardMoveEvent>                       
                        castedPiece.PossibleMoves board pos |> Seq.map (fun moveCom -> pos, moveCom)
                    )
                commands |> Seq.mapi (fun index (pos, moveCom) -> {Index = index; StartField = pos; Cmd = moveCom}) |> Seq.toArray
            let moveCalcResult =
                if possibleMoves.Length = 0 then
                    GameOverZSValue 0.0
                else
                    PossibleMoves possibleMoves
            moveCalcResult, Seq.empty, nextNonHitMovesInRow, lastMoveIsHitMove
    else
        GameOverZSValue 0.0, Seq.empty, nextNonHitMovesInRow, false