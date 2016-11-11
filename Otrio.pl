player(r).
player(b).

pieces( [s, m , l ] ).

movable_pieces(r, [
			(6, s),
			(6, m),
			(6, l)
		  ]).
movable_pieces(b, [
			(6, s),
			(6, m),
			(6, l)
		  ]).

board( [
    	 [ [e, e, e], [e, e, e], [e, e, e] ],
    	 [ [e, e, e], [e, e, e], [e, e, e] ],
    	 [ [e, e, e], [e, e, e], [e, e, e] ]
	     ]).


%%-----------------------
%% 			FUNCTIONS
%%-----------------------

game_cicle:-
	board(Brd), player(Plr),
	get_movable_pieces(Plr, Mv1, Mv2), !,
    cicle(Brd, Plr, Mv1, Mv2).

%% Mv1 are the available pieces of the current player
%% Mv2 are the available pieces of the other player
cicle(Brd, Plr, Mv1, Mv2):-
	print_player(Plr),
	display_board(Brd),

	nl, write('----------------------'),
	nl, write(Mv1),
	nl, write('----------------------'),
	nl,

	ask_piece(P), !, ask_coords(C, L), !,
	piece_to_player(P, Plr, Pair),
	next_cicle(P, Brd, L, C, Pair, Brd2, Plr, Plr2, Mv1, Mv2, NextMv1, NextMv2), !,
	not(end_game(Brd2, Plr)), !,
	cicle(Brd2, Plr2, NextMv1, NextMv2).

cicle(_, Plr, _, _):-
	next_player(Plr, NPlr),
	NPlr = r, !,
	nl, nl, write('-----------------'),
	nl, write(' RED PLAYER WON'),
	nl, write('-----------------'),
	nl, nl.

cicle(_, _, _, _):-
	nl, nl, write('------------------'),
	nl, write(' BLUE PLAYER WON'),
	nl, write('------------------'),
	nl, nl.

%%--------------------
%% HANDLE GAME CICLES
%%--------------------

next_mv(Mv1, Mv1).

next_cicle(P, Brd, L, C, Pair, Brd2, Plr, Plr2, Mv1, Mv2, NextMv1, NextMv2):-
	replace_board(Brd, L, C, Pair, Brd2), !, remove_piece(Mv1, P, NextMv2), !,

	next_player(Plr, Plr2),
	next_mv(Mv2, NextMv1).

next_cicle(_, Brd, _, _, _, Brd, Plr, Plr, Mv1, Mv2, NextMv1, NextMv2):-
	nl, write('---------------------------------'),
	nl, write('That position is already ocuppied'),
	nl, write('---------------------------------'),
	nl,
	next_mv(Mv1, NextMv1),
	next_mv(Mv2, NextMv2).

next_player(Player, NextPlayer):-
	player(NextPlayer),
	NextPlayer \= Player, !.

piece_to_player(Piece, Player, Pair):-
	player(Player), pTp(Piece, Player, Pair).

pTp(Piece, Player, (Piece, Player)).

get_movable_pieces(Plr1, Mv1, Mv2):-
	movable_pieces(Plr1, Mv1),
	next_player(Plr1, Plr2),
	movable_pieces(Plr2, Mv2).


%%--------
%% INPUTS
%%--------

ask_piece(P):-
  pieces(Ps), repeat,
	ask_input('Piece: ', P), member(P, Ps), nl.

ask_coords(C, L):-
  repeat,
	ask_input('Column: ', C), C > -1, C < 3, nl,
	repeat,
  ask_input('Line: ', L), L > -1, L < 3, nl.

ask_input(X, Y):- write(X), nl, read(Y), !.


%%-------------
%% MOVE PIECES
%%-------------

replace_board([H|T], 0, C, P, [R|T]):- !, replace_line(H, C, P, R).

replace_board([H|T], L, C, P, [H|R]):- L > -1, L1 is L-1, replace_board(T, L1, C, P, R), !.

replace_board(L, _, _, _, _, L).


replace_line([H|T], 0, P, [R|T]):- P = (l, _), replace(H, 2, P, R).

replace_line([H|T], 0, P, [R|T]):- P = (m, _), replace(H, 1, P, R).

replace_line([H|T], 0, P, [R|T]):- P = (s, _), replace(H, 0, P, R).

replace_line([H|T], C, P, [H|R]):- C > -1, C1 is C - 1, replace_line(T, C1, P, R), !.

replace_line(L, _, _, _, L).


replace([H|T], 0, _, [_|T]):- H \= e, !, fail.

replace([_|T], 0, P, [P|T]).

replace([H|T], I, P, [H|R]):- I > -1, NI is I-1, replace(T, NI, P, R), !.

replace(L, _, _, L):-fail.


remove_piece([(N, Pr)|T], Piece, [(X, Pr)|T]):-
	Piece = Pr, !, X is N-1.

remove_piece([H|T], Piece, [H|R]):-
	remove_piece(T, Piece, R).

remove_piece(L, _, L):-fail.


%%-----------------
%% VERIFY END GAME
%%-----------------

end_game(Board, Plr):-
	not(verify_diagonal(Board, Plr)),
	not(verify_columns(Board, Plr)),
	not(verify_board(Board, Plr)), !,
	fail.

end_game(_, Plr):-
	Plr = r,
	nl, nl, write('----------------'),
	nl, write(' RED PLAYER WON'),
	nl, write('----------------'),
	nl, nl.

end_game(_, _):-
	nl, nl, write('-----------------'),
	nl, write(' BLUE PLAYER WON'),
	nl, write('-----------------'),
	nl, nl.

verify_diagonal(Board, Plr):-
	element_board(Board, 0, 0, Position1),
	element_board(Board, 1, 1, Position2),
	element_board(Board, 2, 2, Position3),
	equal_pieces(Position1, Position2, Position3, Plr).

verify_diagonal(Board, Plr):-
	element_board(Board, 2, 0, Position1),
	element_board(Board, 1, 1, Position2),
	element_board(Board, 0, 2, Position3),
	equal_pieces(Position1, Position2, Position3, Plr).

verify_columns(Board, Plr):-
	not(verify_column(Board, Plr, 0)),
	not(verify_column(Board, Plr, 1)),
	not(verify_column(Board, Plr, 2)), !,
	fail.

verify_columns(_, _).

verify_column(Board, Plr, Column):-
	element_board(Board, Column, 0, Position1),
	element_board(Board, Column, 1, Position2),
	element_board(Board, Column, 2, Position3),
	column_pieces(Position1, Position2, Position3, Plr).

column_pieces([(m, Plr)|_], Position3, (Piece, Plr)):-
	!, equal_pieces(Position3, (Piece, Plr)).

column_pieces([_|T], Position3, (Piece, Plr)):-
	column_pieces(T, Position3, (Piece, Plr)).

column_pieces([_], _, _, _):-fail.

column_pieces([(Piece, Plr)|_], Position2, Position3, Plr):-
	wanted_piece(Piece, Piece2), !,
	column_pieces(Position2, Position3, (Piece2, Plr)).

column_pieces([_|T], Position2, Position3, Plr):-
	column_pieces(T, Position2, Position3, Plr).

%%verify_win(Board, Plr, Mv).


%%Verifies position and lines
verify_board([], _):-!, fail.

verify_board([H|T], Plr):-
	not(verify_line(H, 0, Plr)), !,
	verify_board(T, Plr), !.

verify_board(_, _).

verify_position(Position, Plr):-
	count_player_pieces(Position, Plr, Counter),
	Counter = 3, !.

verify_line([], _, _):-!, fail.

verify_line([H|T], 0, Plr):-
	not(verify_position(H,Plr)), !,
	line_iter_position([H|T], 0, H, Plr).

verify_line([H|T], 1, (Piece, Plr)):-
	not(verify_position(H,Plr)), !,
	member((m, Plr), H),
	verify_line(T, 2, (Piece, Plr)), !.

verify_line([H|_], 2, Piece):-
	not(verify_position(H,Piece)), !,
	member(Piece, H).

verify_line(_, _, _):-!, fail.

line_iter_position(_, _, [], _):-fail.

line_iter_position([_|T], Counter, [H|_], Plr):-
	H = (Piece, Plr),
	wanted_piece(Piece, Piece2),
	C1 is Counter + 1,
	verify_line(T, C1, (Piece2, Plr)), !.

line_iter_position(Line, Counter, [_|T], Plr):-
	!, line_iter_position(Line, Counter, T, Plr).

wanted_piece(s, l).

wanted_piece(l, s).

wanted_piece(_, _):-fail.

count_player_pieces([], _, 0).

count_player_pieces([H|T], Plr, Counter):-
	H = (_, Plr), count_player_pieces(T, Plr, C1), Counter is C1 + 1.

count_player_pieces([H|T], Plr, Counter):-
	H \= (_, Plr), count_player_pieces(T, Plr, C1), Counter is C1.

equal_pieces([], _):-fail.

equal_pieces([Piece|_], Piece).

equal_pieces([_|T], Piece):-
	equal_pieces(T, Piece).

equal_pieces([], _, _):-fail.

equal_pieces([Piece|_], Position3, Piece):-
	!, equal_pieces(Position3, Piece).

equal_pieces([_|T], Position3, Piece):-
	equal_pieces(T, Position3, Piece).

equal_pieces([_], _, _, _):-fail.

equal_pieces([(Piece, Plr)|_], Position2, Position3, Plr):-
	equal_pieces(Position2, Position3, (Piece, Plr)).

equal_pieces([_|T], Position2, Position3, Plr):-
	equal_pieces(T, Position2, Position3, Plr).


element_board([H|_], C, 0, Position):-
	element_position(H, C, Position).

element_board([_|T], C, L, Position):-
	L1 is L - 1, element_board(T, C, L1, Position).

element_position([Element|_], 0, Element).

element_position([_|T], C, Element):-
	C1 is C - 1, element_position(T, C1, Element).


%% -----------
%% DRAW BOARD
%% -----------

print_player(Plr):-
	Plr = r, !,
	nl, write('	  RED TURN'), nl, nl.

print_player(Plr):-
	Plr = b, !,
	nl, write('	 BLUE TURN'), nl, nl.

print_player(_):-
	nl, write('	ERROR'), nl, nl.

draw_piece( (T, C), B ):-
	player(C), C = r,
	member( (T, C), B ),
	write('@').

draw_piece( (T, C), B ):-
	player(C), C = b,
	member( (T, C), B ),
	write('&').

draw_piece( _, _ ):- write(' ').

display_board(B):- write('     a       b       c'), nl,
	dB(1, B).

dB(N, [H|T]):-
  N =< 3, !,
  write('  -----------------------'), nl,
	display_line(N, 1, H),
	N1 is N + 1,
	dB(N1, T).

dB(_,[]):-
  write('  -----------------------'), nl.

display_line(L, N, B):-
	N = 3, !,
	N1 is N+1,
	write(L),
	dL(N, B), nl,
	display_line(L, N1, B).

display_line(L, N, B):-
  N < 6, !,
	N1 is N+1,
	dL(N, B), nl,
	display_line(L, N1, B).

display_line(_, _, _).

dL(N, _):- N > 5, nl.

%% draw large at n = 1
dL(N, [H|T]):-
	N = 1,
	member( (l, _), H ), !,

	write(' |  /'),
	draw_piece( (l, _), H),
	write('\\ '),

	dL(1, T).

%% draw empty at n = 1
dL(N, [_|T]):-
	N = 1, !,

	write(' |      '),

	dL(1, T).

%% draw large and medium at n = 2
dL(N, [H|T]):-
	N = 2,
	member( (l, _), H ), !,

	write(' | / '),
	draw_piece( (m, _), H),
	write(' \\'),

	dL(2, T).

%% draw medium at n = 2
dL(N, [H|T]):-
	N = 2, !,

	write(' |   '),
	draw_piece( (m, _), H),
	write('  '),

	dL(2, T).


%% draw empty space at n = 2
dL(N, [H|T]):-
	N = 2,
	member( (m, _), H ), !,

	write(' |   '),
	draw_piece( (m, _), H),
	write('   '),

	dL(2, T).

%% draw large, medium and small at n = 3
dL(N, [H|T]):-
	N = 3,
	T \= [], !,
	write('| '),

	draw_piece( (l, _), H ),
	draw_piece( (m, _), H),
	draw_piece( (s, _), H),
	draw_piece( (m, _), H),
	draw_piece( (l, _), H),

	write(' '),

	dL(3, T).

%% draw large, medium and small at n = 3
dL(N, [H|T]):-
	N = 3, !,
	write('| '),

	draw_piece( (l, _), H ),
	draw_piece( (m, _), H),
	draw_piece( (s, _), H),
	draw_piece( (m, _), H),
	draw_piece( (l, _), H),

	write(''),

	dL(3, T).

%% draw large and medium at n = 4
dL(N, [H|T]):-
	N = 4,
	member( (l, _), H ), !,

	write(' | \\ '),
	draw_piece( (m, _), H),
	write(' /'),

	dL(4, T).

%% draw medium at n = 4
dL(N, [H|T]):-
	N = 4, !,

	write(' |   '),
	draw_piece( (m, _), H),
	write('  '),

	dL(4, T).


%% draw large at n = 5
dL(N, [H|T]):-
	N = 5,
	member( (l, _), H ), !,

	write(' |  \\'),
	draw_piece( (l, _), H),
	write('/ '),

	dL(5, T).

%% draw empty at n = 5
dL(N, [_|T]):-
	N = 5, !,

	write(' |      '),

	dL(5, T).

dL(_, []):- write(' |').

%%-----------------------
%% 		MENUS
%%-----------------------

exitGame.

cls :- write('\e[2J').

logo :-         write('|                                                                                 |'), nl,
                write('|       _ _ _ _      _ _ _ _ _ _      _ _ _       _ _ _ _ _ _       _ _ _ _       |'), nl,
                write('|     /    _    \\   |           |   /  _ _  \\    |           |    /    _    \\     |'), nl,
                write('|    |   /   \\   |  |_ _     _ _|  |  |   |  |   |_ _	  _ _|	 |   /   \\   |    |'), nl,
                write('|    |  |     |  |      |   |	   |  |_ _|  |       |   |       |  |     |  |    |'), nl,
                write('|    |  |     |  |      |   |	   |   	    /        |   |       |  |     |  |    |'), nl,
                write('|    |  |     |  |      |   |	   |   |\\   \\	  _ _|   |_ _    |  |     |  |    |'), nl,
                write('|    |   \\ _ /   |      |   |      |   | \\   \\   |           |   |   \\ _ /   |    |'), nl,
                write('|     \\ _ _ _ _ /       | _ |      |_ _|  \\_ _\\  |_ _ _ _ _ _|    \\ _ _ _ _ /     |'), nl,
                write('|                                                                                 |'), nl.


mainMenu :-     write(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '), nl,
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

playMenu :-     cls,
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

gameRules :-	cls,
				write(' - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - '), nl,
				write('|                                                                                 |'), nl,
				write('|                                                                                 |'), nl,
				write('|                  Game Rules:                                                    |'), nl,
				write('|                                                                                 |'), nl,
				write('|                                                                                 |'), nl,
				write('|   -> Each player has 9 pieces, 3 larges, 3 medium and 3 small and each one      |'), nl,
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