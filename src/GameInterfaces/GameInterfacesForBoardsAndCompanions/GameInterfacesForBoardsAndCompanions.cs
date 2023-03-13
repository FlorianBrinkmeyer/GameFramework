namespace GameFramework;

public interface IGameForBoardOrCompanion<Move>
{
    Move [] PossibleMoves {get;}
}
