namespace GameFramework;

public interface IGui
{
     bool NextMoveLoaded {get;}
     void ReInitializeAIs (IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs);
     event EventHandler? Quit;
}