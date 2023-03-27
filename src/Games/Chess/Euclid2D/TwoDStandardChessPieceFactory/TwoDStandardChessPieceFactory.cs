/*
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
*/

namespace StandardChess;

using System;
using Chess;
using GameFramework;
using Microsoft.FSharp.Collections;

public class PieceFactory
{
    public static IPiece InitPiece (String kind, int color)
    {
        IPiece? result = null;
        switch (kind)
        {
            case "Pawn":
                if (color==1)
                    result = new Pawn<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>> 
                        (TwoDMoveCalculation.pawnWhiteStandardMove, TwoDMoveCalculation.pawnWhiteSpecialStartMove, TwoDMoveCalculation.pawnWhiteHitMoves,
                        TwoDMoveCalculation.pawnWhiteKingBlackList, TwoDMoveCalculation.pawnWhiteEnPassant, TwoDMoveCalculation.pawnWhiteElevationCheck,
                        1, 0, null, null, InitPiece ("Queen", 1));
                else
                    result = new Pawn<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>> 
                        (TwoDMoveCalculation.pawnBlackStandardMove, TwoDMoveCalculation.pawnBlackSpecialStartMove, TwoDMoveCalculation.pawnBlackHitMoves,
                        TwoDMoveCalculation.pawnBlackKingBlackList, TwoDMoveCalculation.pawnBlackEnPassant, TwoDMoveCalculation.pawnBlackElevationCheck,
                        -1, 0, null, null, InitPiece ("Queen", -1));
                break;
            case "Knight":
                if (color==1)
                    result = new StandardNonBlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                        (TwoDMoveCalculation.whiteKnightPosMoves, TwoDMoveCalculation.whiteKnightKingBlackList, TwoDMoveCalculation.knightThreateningTest,
                        1, "Knight", 3.0, null, null);
                else
                    result = new StandardNonBlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                        (TwoDMoveCalculation.blackKnightPosMoves, TwoDMoveCalculation.blackKnightKingBlackList, TwoDMoveCalculation.knightThreateningTest,
                        -1, "Knight", 3.0, null, null);
                break;
            case "Rook":
                if (color==1)
                    result = new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>> 
                        (TwoDMoveCalculation.whiteRookPossibleMoves, TwoDMoveCalculation.whiteRookKingBlackList, TwoDMoveCalculation.rookThreateningTest,
                        TwoDMoveCalculation.whiteRookGetWhiteListAndPiece, 1, "Rook", 5.0, true, null, null);
                else
                    result = new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>> 
                        (TwoDMoveCalculation.blackRookPossibleMoves, TwoDMoveCalculation.blackRookKingBlackList, TwoDMoveCalculation.rookThreateningTest,
                        TwoDMoveCalculation.blackRookGetWhiteListAndPiece, -1, "Rook", 5.0, true, null, null);
                break;        
            case "Bishop":
                if (color==1)
                    result = new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                        (TwoDMoveCalculation.whiteBishopPossibleMoves, TwoDMoveCalculation.whiteBishopKingBlackList, TwoDMoveCalculation.bishopThreateningTest,
                        TwoDMoveCalculation.whiteBishopGetWhiteListAndPiece, 1, "Bishop", 3.0, null, null, null);
                else                 
                    result = new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                        (TwoDMoveCalculation.blackBishopPossibleMoves, TwoDMoveCalculation.blackBishopKingBlackList, TwoDMoveCalculation.bishopThreateningTest,
                        TwoDMoveCalculation.blackBishopGetWhiteListAndPiece, -1, "Bishop", 3.0, null, null, null);
                break;
            case "Queen":
                if (color==1)
                    result = new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                        (TwoDMoveCalculation.whiteQueenPossibleMoves, TwoDMoveCalculation.whiteQueenKingBlackList, TwoDMoveCalculation.queenThreateningTest,
                        TwoDMoveCalculation.whiteQueenGetWhiteListAndPiece, 1, "Queen", 9.0, null, null, null);
                else            
                    result = new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                        (TwoDMoveCalculation.blackQueenPossibleMoves, TwoDMoveCalculation.blackQueenKingBlackList, TwoDMoveCalculation.queenThreateningTest,
                        TwoDMoveCalculation.blackQueenGetWhiteListAndPiece, -1, "Queen", 9.0, null, null, null);
                break;
            case "King":
                if (color==1)
                    result = new King<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                        (TwoDMoveCalculation.whiteKingStandardMoves, TwoDMoveCalculation.whiteKingKingBlackList, TwoDMoveCalculation.kingWhiteCastling,
                        1, true, SetModule.Empty<Tuple<int, int>> (),
                        (BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>) InitPiece ("Rook", 1));

                else
                    result = new King<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                        (TwoDMoveCalculation.blackKingStandardMoves, TwoDMoveCalculation.blackKingKingBlackList, TwoDMoveCalculation.kingBlackCastling, 
                        -1, true, SetModule.Empty<Tuple<int, int>> (),
                        (BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>) InitPiece ("Rook", -1));
            break;
        }        
        return result!;
    }
}