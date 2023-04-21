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

using System.Collections.Specialized;

namespace StandardChess;

using System;
using Chess;
using GameFramework;
using Microsoft.FSharp.Collections;
using System.Collections.Generic;

public class PieceFactory
{
    public static IPiece InitPiece (IDictionary<Tuple<String, int, Object?>, Object?> piecePool, String kind, int color)
    {
        IPiece? result = null;
        switch (kind)
        {
            case "Pawn":
                Object? res;
                var key = Tuple.Create("Pawn", color, (Object?)0);
                if (piecePool.TryGetValue(key, out res))
                    result = (IPiece?) res;
                else
                {
                    if (color == 1)
                        result =
                            new Pawn<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.pawnWhiteStandardMove, TwoDMoveCalculation.pawnWhiteSpecialStartMove,
                                TwoDMoveCalculation.pawnWhiteHitMoves,
                                TwoDMoveCalculation.pawnWhiteKingBlackList, TwoDMoveCalculation.pawnWhiteEnPassant,
                                TwoDMoveCalculation.pawnWhiteElevationCheck,
                                1, 0, null, null, InitPiece(piecePool, "Queen", 1), piecePool);
                    else
                        result =
                            new Pawn<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.pawnBlackStandardMove, TwoDMoveCalculation.pawnBlackSpecialStartMove,
                                TwoDMoveCalculation.pawnBlackHitMoves,
                                TwoDMoveCalculation.pawnBlackKingBlackList, TwoDMoveCalculation.pawnBlackEnPassant,
                                TwoDMoveCalculation.pawnBlackElevationCheck,
                                -1, 0, null, null, InitPiece(piecePool, "Queen", -1), piecePool);
                    piecePool[key] = result;
                }
                break;
            case "Knight":
                Object? res2;
                var key2 = Tuple.Create("Knight", color, (Object?) null);
                if (piecePool.TryGetValue(key2, out res2))
                    result = (IPiece?) res2;
                else
                {
                    if (color == 1)
                        result =
                            new StandardNonBlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>,
                                Tuple<int, int>>
                            (TwoDMoveCalculation.whiteKnightPosMoves, TwoDMoveCalculation.whiteKnightKingBlackList,
                                TwoDMoveCalculation.knightThreateningTest,
                                1, "Knight", 3.0, null, null);
                    else
                        result =
                            new StandardNonBlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>,
                                Tuple<int, int>>
                            (TwoDMoveCalculation.blackKnightPosMoves, TwoDMoveCalculation.blackKnightKingBlackList,
                                TwoDMoveCalculation.knightThreateningTest,
                                -1, "Knight", 3.0, null, null);
                    piecePool[key2] = result;
                }
                break;
            case "Rook":
                Object? res3;
                var key3 = Tuple.Create("Rook", color, (Object?) true);
                if (piecePool.TryGetValue(key3, out res3))
                    result = (IPiece?) res3;
                else
                {
                    if (color == 1)
                        result =
                            new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.whiteRookPossibleMoves, TwoDMoveCalculation.whiteRookKingBlackList, TwoDMoveCalculation.rookThreateningTest,
                             TwoDMoveCalculation.whiteRookGetWhiteListAndPiece, 1, "Rook", 5.0, true, null, null, piecePool);
                    else
                        result =
                            new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.blackRookPossibleMoves, TwoDMoveCalculation.blackRookKingBlackList, TwoDMoveCalculation.rookThreateningTest,
                             TwoDMoveCalculation.blackRookGetWhiteListAndPiece, -1, "Rook", 5.0, true, null, null, piecePool);
                    piecePool[key3] = result;
                }
                break;        
            case "Bishop":
                Object? res4;
                var key4 = Tuple.Create("Bishop", color, (Object?) null);
                if (piecePool.TryGetValue(key4, out res4))
                    result = (IPiece?) res4;
                else
                {
                    if (color == 1)
                        result =
                            new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.whiteBishopPossibleMoves, TwoDMoveCalculation.whiteBishopKingBlackList, TwoDMoveCalculation.bishopThreateningTest,
                             TwoDMoveCalculation.whiteBishopGetWhiteListAndPiece, 1, "Bishop", 3.0, null, null, null, piecePool);
                    else
                        result =
                            new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.blackBishopPossibleMoves, TwoDMoveCalculation.blackBishopKingBlackList, TwoDMoveCalculation.bishopThreateningTest,
                             TwoDMoveCalculation.blackBishopGetWhiteListAndPiece, -1, "Bishop", 3.0, null, null, null, piecePool);
                    piecePool[key4] = result;
                }
                break;
            case "Queen":
                Object? res5;
                var key5 = Tuple.Create("Queen", color, (Object?) null);
                if (piecePool.TryGetValue(key5, out res5))
                    result = (IPiece?) res5;
                else
                {
                    if (color == 1)
                        result =
                            new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.whiteQueenPossibleMoves, TwoDMoveCalculation.whiteQueenKingBlackList, TwoDMoveCalculation.queenThreateningTest,
                             TwoDMoveCalculation.whiteQueenGetWhiteListAndPiece, 1, "Queen", 9.0, null, null, null, piecePool);
                    else
                        result =
                            new BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.blackQueenPossibleMoves, TwoDMoveCalculation.blackQueenKingBlackList, TwoDMoveCalculation.queenThreateningTest,
                             TwoDMoveCalculation.blackQueenGetWhiteListAndPiece, -1, "Queen", 9.0, null, null, null, piecePool);
                    piecePool[key5] = result;
                }
                break;
            case "King":
                Object? res6;
                var key6 = Tuple.Create("King", color, (Object?) true);
                if (piecePool.TryGetValue(key6, out res6))
                    result = (IPiece?) res6;
                else
                {
                    if (color == 1)
                        result =
                            new King<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.whiteKingStandardMoves, TwoDMoveCalculation.whiteKingKingBlackList, TwoDMoveCalculation.kingWhiteCastling,
                                1, true, SetModule.Empty<Tuple<int, int>>(), 
                            (BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>)InitPiece(piecePool, "Rook", 1), piecePool);

                    else
                        result =
                            new King<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>
                            (TwoDMoveCalculation.blackKingStandardMoves, TwoDMoveCalculation.blackKingKingBlackList, TwoDMoveCalculation.kingBlackCastling,
                            -1, true, SetModule.Empty<Tuple<int, int>>(), 
                            (BlockablePiece<ImmutableEnumerable2DArray.ImmutableEnumerable2DArray<IPiece>, Tuple<int, int>>)InitPiece(piecePool, "Rook", -1), piecePool);
                    piecePool[key6] = result;
                }
                break;
        }        
        return result!;
    }
}