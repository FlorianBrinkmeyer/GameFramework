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

public enum RunState
{
    Running,
    PausingPreserveNextMove,
    PausingIgnoreNextMove,
    Terminated
}

public class GameCompanion<GameResult>: IGameMoveMaker, IReversibleGame<GameResult>, IInitGame
{
    protected ImmutableGame State;
    Func<ImmutableGame, GameResult> resultMapper;
    Dictionary<int,AI_Agent>? playerToAIAgent;
    protected bool DebugMode;
    public event UpdateAIsEvent? UpdateAIs;
    protected RunState runState = RunState.PausingIgnoreNextMove;
    public void UpdateAIAgents (IEnumerable<AI_Agent> agents)
    {
        var stopableAIs = playerToAIAgent?.Values.OfType<StopableAI>();
        if (stopableAIs != null)
        {
            foreach (StopableAI ai in stopableAIs)
                ai.Stop ();        
            //Thread.Sleep (50);
        }
        playerToAIAgent = new Dictionary<int, AI_Agent> ();
        foreach (AI_Agent agent in agents)
            playerToAIAgent![agent.Player] = agent;
        runState = RunState.PausingIgnoreNextMove;
    }
    public GameCompanion (ImmutableGame startState, IEnumerable<AI_Agent> agents, Func<ImmutableGame, GameResult> _resultMapper, bool debugMode)
    {
        State = startState;
        resultMapper = _resultMapper;
        DebugMode = debugMode;
        playerToAIAgent = new Dictionary<int, AI_Agent> ();
        foreach (AI_Agent agent in agents)
            playerToAIAgent![agent.Player] = agent;
    }
    public bool Running => State.Running;
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
    void TriggerGameOver (GameResult result) => GameOver?.Invoke (result);
    protected virtual void TriggerNextPlayer ()
    {
        if (State.Running)
        {
            AI_Agent? agent;
            if ((playerToAIAgent != null) && (playerToAIAgent.TryGetValue (ActivePlayer, out agent)))
            {
                NextPlayer?.Invoke (ActivePlayer);
                agent.MakeMove (this, State);
            } else {
                NextPlayer?.Invoke (ActivePlayer);
            }    
        }
    }
    protected virtual void InternalMakeMove(int index)
    {
        State = State.NthMove (index);
        TriggerMoveMade (index);
        TriggerTriggerBoardEvents ();
        if (DebugMode)
        {
            Console.WriteLine (State.ToString ());
            Console.WriteLine ();
        }
        if (!State.Running)
            TriggerGameOver (resultMapper (State));
        else 
            TriggerNextPlayer ();
    }
    private int? chosenMove = null;
    public event EventHandler? PauseMoveDelivered;
    public virtual void MakeMove(int index)
    {
        if (runState == RunState.PausingPreserveNextMove)
        {
            chosenMove = index;
            PauseMoveDelivered?.Invoke (this, EventArgs.Empty);
        } 
        else if (runState == RunState.Running)
        {
            InternalMakeMove (index);
        }
    }
    public void Run ()
    {
        if (runState == RunState.PausingPreserveNextMove)
        {
            runState = RunState.Running;
            if (chosenMove != null)
            {
                var value = chosenMove.Value;
                chosenMove = null;
                InternalMakeMove (value);
            }
        }
        else
        {
            runState = RunState.Running;
            TriggerNextPlayer ();
        }
    }
    public void Pause ()
    {
        runState = RunState.PausingPreserveNextMove;
        UpdateAIs?.Invoke (false);
        if (runState == RunState.PausingIgnoreNextMove)
        {
            runState = RunState.PausingPreserveNextMove;
            TriggerNextPlayer ();
        }
    }
    public bool Paused => runState == RunState.PausingPreserveNextMove || runState == RunState.PausingIgnoreNextMove;
    public void Continue()
    {
        UpdateAIs?.Invoke (false);
        if (runState == RunState.PausingPreserveNextMove && chosenMove == null)
        {
            runState = RunState.Running;
        }
        else {
            Run ();
        }
    }
    public void Stop ()
    {
        runState = RunState.Terminated;
        var stopableAIs = playerToAIAgent!.Values.OfType<StopableAI>();
        foreach (StopableAI ai in stopableAIs)
            ai.Stop ();        
        //Thread.Sleep (50);
    }
    public void SingleStep()
    {
        if (runState == RunState.PausingPreserveNextMove)
        {
            if  (chosenMove != null)
            {
                var value = chosenMove.Value;
                InternalMakeMove(value);
                chosenMove = null;
            }
        } 
    }
    public bool Undoable => State.Previous != null;
    public void Undo ()
    {
        if (State.Previous != null)
        {
            State = State.Previous.Value;
            Undone?.Invoke (this, new EventArgs ());
            UpdateAIs?.Invoke (true);
        }
        else
        {
            throw new InvalidOperationException ("Undo impossible: No previous state.");
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