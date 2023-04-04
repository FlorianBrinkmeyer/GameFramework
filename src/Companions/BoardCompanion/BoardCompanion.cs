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

namespace GameFramework;

abstract public class BoardCompanion<Board, Evnt> : IBoardInformer<Evnt>
{
    protected IBoardGameCompanion<Board, Evnt> GameCompanion;
    protected IBoardGameForCompanion<Board, Evnt> Game => GameCompanion.Game;
    protected Board GameBoard => Game.GameBoard;    
    public BoardCompanion (IBoardGameCompanion<Board, Evnt> companion)
    {
        GameCompanion = companion;
        GameCompanion.TriggerBoardEvents += (Object? sender, EventArgs args) => 
        {
            foreach (Evnt? boardEvent in Game.BoardEvents)
                BoardInformerEvent?.Invoke (this, boardEvent);
        };
    }
    public event EventHandler<Evnt>? BoardInformerEvent;
}