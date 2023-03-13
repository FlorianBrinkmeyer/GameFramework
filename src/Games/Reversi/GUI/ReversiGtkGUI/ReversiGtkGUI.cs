namespace Reversi;

using GameFramework;
using Euclid2DGame;
using Euclid2D;

public class ReversiGtkGUI<Board> : TwoDSetBoardGUI<Board, int>
where Board : ITwoDSetBoard<int> 
{
    override protected Gtk.Builder builder {get;} = new Gtk.Builder ();
    override protected String getImageFileName (int piece)
    {
        if (piece == 1)
            return "White";
        else
            return "Black";     
    }
    protected override string PlayerToString(int id)
    {
        if (id == 1) 
            return "White";
        else
            return "Black";    
    }
    public ReversiGtkGUI (int windowsWidth, int windowHeight, String pictureFolder, String guiFilename, Board _board, 
    IGameInformer<String> _game, IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs)
    {
        Gtk.Application.Init ();
        builder.AddFromFile (guiFilename);
        builder.Autoconnect (this);
        initialize (windowsWidth, windowHeight, pictureFolder, _board, _game, thisGUIusers, AIs);
    }
}