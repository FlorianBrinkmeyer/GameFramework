namespace Reversi;

using GameFramework;
using Euclid2DGame;
using Euclid2D;

public class ReversiGtkGUI : TwoDSetBoardGUI<int>
{
    override protected Gtk.Builder Builder {get;} = new Gtk.Builder ();
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
    public ReversiGtkGUI (int windowsWidth, int windowHeight, String pictureFolder, String guiFilename, ITwoDSetBoard<int> board, 
    IGameInformer<String> game, IEnumerable<int> thisGUIusers, IEnumerable<AI_Informer> AIs, bool debugMode)
    {
        Gtk.Application.Init ();
        Builder.AddFromFile (guiFilename);
        Builder.Autoconnect (this);
        var mainForm = (Gtk.ApplicationWindow) Builder.GetObject ("Window");
        mainForm.Title = "Reversi";
        mainForm.WindowPosition = Gtk.WindowPosition.Center;
        Initialize (windowsWidth, windowHeight, pictureFolder, board, game, thisGUIusers, AIs, debugMode);
    }
}