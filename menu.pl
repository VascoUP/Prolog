%%-----------------------
%% 		MENUS
%%-----------------------

exitGame.

cls :-
  nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
  nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
  nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
  nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl.


logo:-         	write('|                                                                                 |'), nl,
                write('|       _ _ _ _      _ _ _ _ _ _      _ _ _       _ _ _ _ _ _       _ _ _ _       |'), nl,
                write('|     /    _    \\   |           |   /  _ _  \\    |           |    /    _    \\     |'), nl,
                write('|    |   /   \\   |  |_ _     _ _|  |  |   |  |   |_ _	  _ _|	 |   /   \\   |    |'), nl,
                write('|    |  |     |  |      |   |	   |  |_ _|  |       |   |       |  |     |  |    |'), nl,
                write('|    |  |     |  |      |   |	   |   	    /        |   |       |  |     |  |    |'), nl,
                write('|    |  |     |  |      |   |	   |   |\\   \\	  _ _|   |_ _    |  |     |  |    |'), nl,
                write('|    |   \\ _ /   |      |   |      |   | \\   \\   |           |   |   \\ _ /   |    |'), nl,
                write('|     \\ _ _ _ _ /       | _ |      |_ _|  \\_ _\\  |_ _ _ _ _ _|    \\ _ _ _ _ /     |'), nl,
                write('|                                                                                 |'), nl.


mainMenu:-     	cls,
				write(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '), nl,
				write('|                                                                                 |'), nl,
				logo,
                write('|                                                                                 |'), nl,
				write('|                                                                                 |'), nl,
				write('|                                                                                 |'), nl,
                write('|                                 Welcome!!!	                                  |'), nl,
                write('|   				                                                  |'), nl,
				write('|                       				                          |'), nl,
				write('|   				                                                  |'), nl,
                write('|                   1. Play                                                       |'), nl,
                write('|                   2. Game Rules                                                 |'), nl,
                write('|                   3. Exit                                                       |'), nl,
                write('|                                                                                 |'), nl,
                write(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '), nl,
                nl, write('Option: '), get_char(R), get_char(_), 
                (R = '1' -> playMenu;
				 R = '2' -> gameRules;
				 R = '3' -> exitGame;
				 mainMenu).

playMenu:-     	cls,
				write(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '), nl,
                write('|                                                                                 |'), nl,
                write('|                                                                                 |'), nl,
				write('|                  Game Mode:                                                     |'), nl,
                write('|                                                                                 |'), nl,
                write('|                    1. Human vs Human                                            |'), nl,
                write('|                    2. Human vs Computer                                         |'), nl,
                write('|                    3. Computer vs Computer                                      |'), nl,
                write('|                    4. Return                                                    |'), nl,
                write('|                                                                                 |'), nl,
                write(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '), nl,
                nl, write('Option: '), get_char(G), get_char(_),
                (G = '1' -> cls, game_cicle;
				 G = '4' -> cls, mainMenu;
				 playMenu).

gameRules:-		cls,
				write(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '), nl,
				write('|                                                                                 |'), nl,
				write('|                                                                                 |'), nl,
				write('|                                Game Rules:                                      |'), nl,
				write('|                                                                                 |'), nl,
				write('|                                                                                 |'), nl,
				write('|   -> Each player has 32 pieces, 6 larges, 6 medium and 6 small and each one     |'), nl,
				write('|      of them must put the respective pieces in empty spaces, not being able     |'), nl,
				write('|      to move them after their placement                                         |'), nl,
				write('|                                                                                 |'), nl,
				write('|   -> If a player can not put one of his pieces, the turm changes to the other   |'), nl,
				write('|      player, so that they can place pieces until they reach a OTRIO             |'), nl,
				write('|                                                                                 |'), nl,
				write('|      How to make a OTRIO:                                                       |'), nl,
				write('|                                                                                 |'), nl,
				write('|      1. Have 3 pieces with the same size and color on a board row               |'), nl,
				write('|      2. Have 3 pieces of increasing or decreasing size on a board row           |'), nl,
				write('|      3. Have 3 concentic pieces inthe same place                                |'), nl,
				write('|                                                                                 |'), nl,
				write('|                                                                                 |'), nl,
				write('|                     Press enter to return to the main menu                      |'), nl,
				write('|                                                                                 |'), nl,
				write(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '), nl,
				nl, get_char(_), cls, mainMenu.		