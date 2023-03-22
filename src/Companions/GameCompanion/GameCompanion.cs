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

using System;
using System.Collections.Generic;

public class GameCompanion<GameResult>: IGameMoveMaker, IReversibleGame<GameResult>
{
    protected ImmutableGame state;
    protected Func<ImmutableGame, GameResult> resultMapper;
    Dictionary<int,AI_Agent>? playerToAIAgent = new Dictionary<int, AI_Agent> ();
    public GameCompanion (ImmutableGame startState, IEnumerable<AI_Agent> agents, Func<ImmutableGame, GameResult> _resultMapper)
    {
        state = startState;
        resultMapper = _resultMapper;
        foreach (AI_Agent agent in agents)
            playerToAIAgent[agent.Player] = agent;
    }
    public bool Running => state.Running;
    public int ActivePlayer => state.ActivePlayer;
    protected virtual void TriggerTriggerBoardEvents () {}
    public event MoveMadeEvent? MoveMadeEvent;
    public event EventHandler? MoveMade;
    public void TriggerMoveMade (int moveIndex)
    {
        MoveMade?.Invoke (this, new EventArgs ());
        MoveMadeEvent?.Invoke (moveIndex);
    }
    public event NextPlayerEvent? NextPlayer;
    public void TriggerNextPlayer (int id) => NextPlayer?.Invoke (id);
    public event GameOverEvent<GameResult>? GameOver;
    public void TriggerGameOver (GameResult result) => GameOver?.Invoke (result);
    public event EventHandler? Undone;
    public virtual void MakeMove (int index)
    {
        state = state.NthMove (index);
        TriggerMoveMade (index);
        TriggerTriggerBoardEvents ();
        if (state.Running)
        {
            AI_Agent? agent;
            if ((playerToAIAgent != null) && (playerToAIAgent.TryGetValue (ActivePlayer, out agent)))
            {
                TriggerNextPlayer (ActivePlayer);
                agent.MakeMove (this, state);
            } else {
                TriggerNextPlayer (ActivePlayer);
            }    
        }     
        else
        {
            TriggerGameOver (resultMapper (state));
        }
    }
    public void Run ()
    {
        AI_Agent? agent;
        if ((playerToAIAgent != null) && (playerToAIAgent.TryGetValue (ActivePlayer, out agent)))
        {
            TriggerNextPlayer (ActivePlayer);
            agent.MakeMove (this, state);
        } else {
            TriggerNextPlayer (ActivePlayer);
        }    
    }
    public bool Undoable => state.Previous != null;
    public void Undo ()
    {
        if (state.Previous != null)
        {
            state = state.Previous.Value;
            Undone?.Invoke (this, new EventArgs ());
        }
        else
        {
            throw new Exception ("Undo impossible: No previous state.");
        }
    }
}

public interface IBoardGameCompanion<out Board, Evnt>
{
    IBoardGameForCompanion<Board, Evnt> Game {get;}
    public event EventHandler? TriggerBoardEvents;    
}

public class BoardGameCompanion<GameResult, Board, Evnt> : GameCompanion<GameResult>, IBoardGameCompanion<Board, Evnt>
{
    public BoardGameCompanion (ImmutableGame startState, IEnumerable<AI_Agent> agents, Func<ImmutableGame, GameResult> _resultMapper) : base (startState, agents, _resultMapper) {}
    public IBoardGameForCompanion<Board, Evnt> Game => (IBoardGameForCompanion<Board, Evnt>) state;
    public event EventHandler? TriggerBoardEvents;
    protected override void TriggerTriggerBoardEvents () => TriggerBoardEvents?.Invoke (this, new EventArgs ());
}