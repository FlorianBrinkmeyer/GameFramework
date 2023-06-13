# Overview

The purpose of this program is to provide a framework that allows playing different (board) games, if desired, against AI opponents. <br>
The currently implemented games are (standard) **Chess** and **Reversi**. <br>
The currently implemented AI algorithms are **Negamax** and **MonteCarlo tree search**. <br>

The key objective for the program was to create an architecture with a high degree of code reusability, modularization, and clarity. <br>
Hence, depending on how similar they are to the already provided, implementing further games should be comparatively easy.

Performance, on the other hand, was only a subordinate goal. As a result, the playing strength of the currently implemented AI agents is, at least regarding chess, comparatively weak.

The approach is unusual in that the core of the program widely refrains from side effects: Instead of altering a game state, a new one is created. This results in AI algorithms that could be written in a very functional style, since during simulations of possible games former states are preserved and don't have to be restored.

# How to use

Binaries: [Release](https://github.com/FlorianBrinkmeyer/GameFramework/releases/tag/0.9)

If you want to compile the code yourself, compile and run **/src/Initialization/GameInitializer**. <br>
Alter **/src/Initialization/GameInitializationConfig** in order to adapt the configuration according to your wishes.

# Design Documentation

Will be provided here in the near future.

# Utilized projects

Used pictures of chess pieces by Cburnett: <br>
https://commons.wikimedia.org/wiki/Category:PNG_chess_pieces/Standard_transparent <br>
https://creativecommons.org/licenses/by-sa/3.0/

The GUIs utilize the GTK framework and the GtkSharp wrapper for .NET: <br>
https://www.gtk.org/ <br>
https://github.com/GtkSharp/GtkSharp