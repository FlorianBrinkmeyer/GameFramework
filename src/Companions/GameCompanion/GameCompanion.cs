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
    protected ImmutableGame State;
    Func<ImmutableGame, GameResult> resultMapper;
    Dictionary<int,AI_Agent>? playerToAIAgent;
    protected bool DebugMode;
    public event EventHandler? UpdateAIs;
    public void UpdateAIAgents (IEnumerable<AI_Agent> agents)
    {
        playerToAIAgent = new Dictionary<int, AI_Agent> ();
        foreach (AI_Agent agent in agents)
            playerToAIAgent![agent.Player] = agent;
    }
    public GameCompanion (ImmutableGame startState, IEnumerable<AI_Agent> agents, Func<ImmutableGame, GameResult> _resultMapper, bool debugMode)
    {
        State = startState;
        resultMapper = _resultMapper;
        DebugMode = debugMode;
        UpdateAIAgents (agents);
    }
    bool running = false;
    public bool Running => running && State.Running;
    public int ActivePlayer => State.ActivePlayer;
    protected virtual void TriggerTriggerBoardEvents () {}
    public event MoveMadeEvent? MoveMadeEvent;
    public event EventHandler? MoveMade;
    public event NextPlayerEvent? NextPlayer;
    public event GameOverEvent<GameResult>? GameOver;
    public event EventHandler? Undone;
    void TriggerMoveMade (int moveIndex)
    {
        MoveMade?.Invoke (this, new EventArgs ());
        MoveMadeEvent?.Invoke (moveIndex);
    }
    void TriggerNextPlayer (int id) => NextPlayer?.Invoke (id);
    void TriggerGameOver (GameResult result) => GameOver?.Invoke (result);
    public virtual void MakeMove (int index)
    {
        if (Running)
        {
            State = State.NthMove (index);
            TriggerMoveMade (index);
            TriggerTriggerBoardEvents ();
            if (DebugMode)
            {
                Console.WriteLine (State.ToString ());
                Console.WriteLine ();
            }
            if (State.Running)
            {
                AI_Agent? agent;
                if ((playerToAIAgent != null) && (playerToAIAgent.TryGetValue (ActivePlayer, out agent)))
                {
                    TriggerNextPlayer (ActivePlayer);
                    agent.MakeMove (this, State);
                } else {
                    TriggerNextPlayer (ActivePlayer);
                }    
            }     
            else
            {
                TriggerGameOver (resultMapper (State));
            }
        }
    }
    public void Run ()
    {
        running = true;
        AI_Agent? agent;
        if ((playerToAIAgent != null) && (playerToAIAgent.TryGetValue (ActivePlayer, out agent)))
        {
            TriggerNextPlayer (ActivePlayer);
            agent.MakeMove (this, State);
        } else {
            TriggerNextPlayer (ActivePlayer);
        }    
    }
    public void Pause ()
    {
        running = false;
        var stopableAIs = playerToAIAgent!.Values.OfType<StopableAI>();
        foreach (StopableAI ai in stopableAIs)
            ai.Stop ();        
        Thread.Sleep (50);
    }
    public void Continue()
    {
        if (!running)
        {
            UpdateAIs?.Invoke (this, EventArgs.Empty);
            Run ();
        }
    }
    public void Stop () => Pause ();
    public bool Undoable => State.Previous != null;
    public void Undo ()
    {
        if (State.Previous != null)
        {
            if (running) 
                Pause();
            State = State.Previous.Value;
            Undone?.Invoke (this, new EventArgs ());
        }
        else
        {
            throw new Exception ("Undo impossible: No previous state.");
        }
    }
    public override bool Equals (object? obj)
    {
        if (obj is ImmutableGame)
            return State.Equals (obj);
        else
            return base.Equals (obj);
    }
    public override int GetHashCode()
    {
        return State.GetHashCode ();
    }
    public override string ToString()
    {
        return State.ToString ()!;
    }
}

public interface IBoardGameCompanion<out Board, Evnt>
{
    IBoardGameForCompanion<Board, Evnt> Game {get;}
    public event EventHandler? TriggerBoardEvents;    
}

public class BoardGameCompanion<GameResult, Board, Evnt> : GameCompanion<GameResult>, IBoardGameCompanion<Board, Evnt>
{
    public BoardGameCompanion (ImmutableGame startState, IEnumerable<AI_Agent> agents, Func<ImmutableGame, GameResult> resultMapper, bool debugMode) 
        : base (startState, agents, resultMapper, debugMode) {}
    public IBoardGameForCompanion<Board, Evnt> Game => (IBoardGameForCompanion<Board, Evnt>) State;
    public event EventHandler? TriggerBoardEvents;
    protected override void TriggerTriggerBoardEvents () => TriggerBoardEvents?.Invoke (this, new EventArgs ());
}