:- include('menu.pl').

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
    	 [ [(s,r), e, e], [e, (m,r), e], [e, e, e] ],
    	 [ [(s,r), (m,r), e], [e, e, e], [e, e, e] ],
    	 [ [e, e, e], [e, e, e], [(s,r), (m,r), e] ]
	     ]).


%%-----------------------
%% 			FUNCTIONS
%%-----------------------

otrio :- mainMenu.

game_cicle:-
	board(Brd), player(Player),
	get_movable_pieces(Player, Mv1, Mv2), !,
    cicle(Brd, Player, Mv1, Mv2).

%% Mv1 are the available pieces of the current player
%% Mv2 are the available pieces of the other player
cicle(Brd, Player, Mv1, Mv2):-
	print_player(Player),
	display_board(Brd),

	nl, write('---------------------------'),
	nl, write('      Avaiable pieces      '), nl,
	nl, write(Mv1), nl,
	nl, write('---------------------------'),
	nl,

	ask_piece(P), !, ask_coords(C, L), !,
	piece_to_player(P, Player, Pair),
	next_cicle(P, Brd, L, C, Pair, Brd2, Player, Player2, Mv1, Mv2, NextMv1, NextMv2), !,
	not(end_game(Brd2, Player)), !,
	cicle(Brd2, Player2, NextMv1, NextMv2).

cicle(_, Player, _, _):-
	next_player(Player, NPlayer),
	NPlayer = r, !,
	nl, nl, write('-----------------'),
	nl, write(' * RED PLAYER WON * '),
	nl, write('-----------------'),
	nl, nl.

cicle(_, _, _, _):-
	nl, nl, write('------------------'),
	nl, write(' * BLUE PLAYER WON * '),
	nl, write('------------------'),
	nl, nl.

%%--------------------
%% HANDLE GAME CICLES
%%--------------------

next_mv(Mv1, Mv1).

next_cicle(P, Brd, L, C, Pair, Brd2, Player, Player2, Mv1, Mv2, NextMv1, NextMv2):-
	replace_board(Brd, L, C, Pair, Brd2), !, remove_piece(Mv1, P, NextMv2), !,

	next_player(Player, Player2),
	next_mv(Mv2, NextMv1).

next_cicle(_, Brd, _, _, _, Brd, Player, Player, Mv1, Mv2, NextMv1, NextMv2):-
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

get_movable_pieces(Player1, Mv1, Mv2):-
	movable_pieces(Player1, Mv1),
	next_player(Player1, Player2),
	movable_pieces(Player2, Mv2).


%%--------
%% INPUTS
%%--------

ask_piece(P):-
	pieces(Ps), repeat,
	ask_pieceName('Piece: ', P), member(P, Ps), nl.

ask_coords(C, L):-
  repeat,
	ask_column('Column: ', C), C \= 'a', C \= 'b', C \= 'c' , nl,
	repeat,
	ask_line('Line: ', L), L > -1, L < 3, nl.

ask_pieceName(X, Y):- nl, write(X), nl, get_char(Y), get_char(_).

ask_column(X, C):- nl, write(X), nl, get_char(Y), get_char(_), 	
					(Y = 'a' -> C = 0; Y = 'b' -> C = 1; Y ='c' -> C = 2).

ask_line(X, L):- nl, write(X), nl, get_char(Y), get_char(_), 
				(Y = '1' -> L = 0; Y = '2' -> L = 1; Y = '3' -> L = 2).



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

%%------
%% "AI"
%%------

equal_pair(Pair, Pair).
equal_line(Line, Line).
equal_column(Column, Column).

next_win(Board, Mv, Player, Line, Column, Pair):-
	win_board(Board, Mv, Player, 0, Line, Column, Pair).

win_board(_, _, _, 3, _, _, _):-!, fail.

win_board(Board, Mv, Player, Line, LineC, ColumnC, PairC):-
	win_line(Board, Mv, Player, Line, 0, ColumnC, PairC),
	equal_line(Line, LineC).

win_board(Board, Mv, Player, Line, LineC, ColumnC, PairC):-
	L1 is Line + 1, !,
	win_board(Board, Mv, Player, L1, LineC, ColumnC, PairC).

win_line(_, _, _, _, 3, _, _):-!, fail.

win_line(Board, Mv, Player, Line, Column, Column2, Pair):-
	win_position(Board, Line, Column, (s, Player), Mv, Pair), !,
	equal_column(Column, Column2).

win_line(Board, Mv, Player, Line, Column, Column2, Pair):-
	C1 is Column + 1, !,
	win_line(Board, Mv, Player, Line, C1, Column2, Pair).

win_position(Board, Line, Column, Pair, Mv, Pair2):-
	replace_board(Board, Line, Column, Pair, Board2),
	Pair = (Piece, Player),
	remove_piece(Mv, Piece, _),
	end_game(Board2, Player),
	equal_pair(Pair, Pair2).

win_position(Board, Line, Column, Pair, Mv, Pair2):-
	Pair = (Piece, Player),
	next_piece(Piece, Piece2, _), !,
	win_position(Board, Line, Column, (Piece2, Player), Mv, Pair2), !.

win_position(_, _, _, _, _, _):-fail.

next_piece(s, m, 1).
next_piece(m, l, 2).
next_piece(_, _, _):-fail.


%%-----------------
%% VERIFY END GAME
%%-----------------

end_game(Board, Player):-
	not(verify_diagonal(Board, Player)),
	not(verify_columns(Board, Player)),
	not(verify_board(Board, Player)), !,
	fail.

end_game(_, _).

verify_diagonal(Board, Player):-
	element_board(Board, 0, 0, Position1),
	element_board(Board, 1, 1, Position2),
	element_board(Board, 2, 2, Position3),
	equal_pieces(Position1, Position2, Position3, Player).

verify_diagonal(Board, Player):-
	element_board(Board, 2, 0, Position1),
	element_board(Board, 1, 1, Position2),
	element_board(Board, 0, 2, Position3),
	equal_pieces(Position1, Position2, Position3, Player).

verify_columns(Board, Player):-
	not(verify_column(Board, Player, 0)),
	not(verify_column(Board, Player, 1)),
	not(verify_column(Board, Player, 2)), !,
	fail.

verify_columns(_, _).

verify_column(Board, Player, Column):-
	element_board(Board, Column, 0, Position1),
	element_board(Board, Column, 1, Position2),
	element_board(Board, Column, 2, Position3),
	column_pieces(Position1, Position2, Position3, Player).

column_pieces([(m, Player)|_], Position3, (Piece, Player)):-
	!, equal_pieces(Position3, (Piece, Player)).

column_pieces([_|T], Position3, (Piece, Player)):-
	column_pieces(T, Position3, (Piece, Player)).

column_pieces([_], _, _, _):-fail.

column_pieces([(Piece, Player)|_], Position2, Position3, Player):-
	wanted_piece(Piece, Piece2), !,
	column_pieces(Position2, Position3, (Piece2, Player)).

column_pieces([_|T], Position2, Position3, Player):-
	column_pieces(T, Position2, Position3, Player).

%%Verifies position and lines
verify_board([], _):-!, fail.

verify_board([H|T], Player):-
	not(verify_line(H, 0, Player)), !,
	verify_board(T, Player), !.

verify_board(_, _).

verify_position(Position, Player):-
	count_player_pieces(Position, Player, Counter),
	Counter = 3, !.

verify_line([], _, _):-!, fail.

verify_line([H|_], _, Player):-
	verify_position(H,Player).

verify_line([H|T], 0, Player):-
	line_win_position([H|T], 0, H, Player).

verify_line([H|T], 1, (Piece, Player)):-
	member((m, Player), H),
	verify_line(T, 2, (Piece, Player)), !.

verify_line([H|_], 2, Piece):-
	member(Piece, H).

verify_line(_, _, _):-!, fail.

line_win_position(_, _, [], _):-fail.

line_win_position([_|T], Counter, [H|_], Player):-
	H = (Piece, Player),
	wanted_piece(Piece, Piece2),
	C1 is Counter + 1,
	verify_line(T, C1, (Piece2, Player)), !.

line_win_position(Line, Counter, [_|T], Player):-
	!, line_win_position(Line, Counter, T, Player).

wanted_piece(s, l).

wanted_piece(l, s).

wanted_piece(_, _):-fail.

count_player_pieces([], _, 0).

count_player_pieces([H|T], Player, Counter):-
	H = (_, Player), count_player_pieces(T, Player, C1), Counter is C1 + 1.

count_player_pieces([H|T], Player, Counter):-
	H \= (_, Player), count_player_pieces(T, Player, C1), Counter is C1.

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

equal_pieces([(Piece, Player)|_], Position2, Position3, Player):-
	equal_pieces(Position2, Position3, (Piece, Player)).

equal_pieces([_|T], Position2, Position3, Player):-
	equal_pieces(T, Position2, Position3, Player).


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

print_player(Player):-
	Player = r, !,
	nl, write('	* RED TURN * '), nl, nl.

print_player(Player):-
	Player = b, !,
	nl, write('	 * BLUE TURN * '), nl, nl.


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