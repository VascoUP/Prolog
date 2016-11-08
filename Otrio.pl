player(r).
player(b).

pieces( [s, m, l] ).

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


%%-----------
%% FUNCTIONS
%%-----------

game_cicle:-
	board(Brd), player(Plr),
	get_movable_pieces(Plr, Mv1, Mv2), !,
  cicle(Brd, Plr, Mv1, Mv2).

%% Mv1 are the available pieces of the current player
%% Mv2 are the available pieces of the other player
cicle(Brd, Plr, Mv1, Mv2):-
	display_board(Brd),

	nl, write('----------------------'),
	nl, write(Mv1),
	nl, write('----------------------'),
	nl,

	ask_piece(P), !, ask_coords(C, L), !,
	verify_piece_to_player(P, Plr, Pair),
	next_cicle(P, Brd, L, C, Pair, Brd2, Plr, Plr2, Mv1, Mv2, NextMv1, NextMv2),
	!, cicle(Brd2, Plr2, NextMv1, NextMv2).


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

verify_piece_to_player(Piece, Player, Pair):-
	player(Player), piece_to_player(Piece, Player, Pair).

piece_to_player(Piece, Player, (Piece, Player)).

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


%% -----------
%% DRAW BOARD
%% -----------

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
