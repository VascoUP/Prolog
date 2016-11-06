player(r).
player(b).

movable_pieces(r, [
			[ s, m, l ],
			[ s, m, l ],
			[ s, m, l ],
			[ s, m, l ],
			[ s, m, l ],
			[ s, m, l ]
		  ]).
movable_pieces(b, [
			[ s, m, l ],
			[ s, m, l ],
			[ s, m, l ],
			[ s, m, l ],
			[ s, m, l ],
			[ s, m, l ]
		  ]).

board( [
    	 [ [e, e, e], [e, e, e], [e, e, e] ],
    	 [ [e, e, (l, r)], [e, e, e], [e, e, e] ],
    	 [ [e, e, e], [e, e, e], [e, e, e] ]
	     ]).

game_cicle:- write('Hello darkness'), nl, board(B), !,
  repeat, display_board(B),
  ask_piece(P), ask_coords(C, L),
  write('Piece '), write(P), write(' to '), write(C), write(' - '), write(L), nl,
	write('Replace'), nl, replace_board(B, L, C, P, X), nl, write('B2'), nl, nl, display_board(X),
  fail, !.

ask_piece(P):-
  ask_input('Piece: ', P), nl.

ask_coords(C, L):-
  ask_input('Column: ', C), nl,
  ask_input('Line: ', L), nl.

ask_input(X, Y):- write(X), nl, read(Y), !.

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
	dB(1, B), !.

dB(N, [H|T]):-
  N =< 3,
  write('  -----------------------'), nl,
	display_line(N, 1, H),
	N1 is N + 1,
	dB(N1, T).

dB(_,[]):-
  write('  -----------------------'), nl.

display_line(L, N, B):-
	N = 3,
	N1 is N+1,
	write(L),
	dL(N, B), nl,
	display_line(L, N1, B).

display_line(L, N, B):-
  N < 6,
	N1 is N+1,
	dL(N, B), nl,
	display_line(L, N1, B).

display_line(_, _, _).

dL(N, _):- N > 5, nl.

%% draw large at n = 1
dL(N, [H|T]):-
	N = 1,
	member( (l, _), H ),

	write(' |  /'),
	draw_piece( (l, _), H),
	write('\\ '),

	dL(1, T).

%% draw empty at n = 1
dL(N, [_|T]):-
	N = 1,

	write(' |      '),

	dL(1, T).

%% draw large and medium at n = 2
dL(N, [H|T]):-
	N = 2,
	member( (l, _), H ),

	write(' | / '),
	draw_piece( (m, _), H),
	write(' \\'),

	dL(2, T).

%% draw medium at n = 2
dL(N, [H|T]):-
	N = 2,

	write(' |   '),
	draw_piece( (m, _), H),
	write('  '),

	dL(2, T).


%% draw empty space at n = 2
dL(N, [H|T]):-
	N = 2,
	member( (m, _), H ),

	write(' |   '),
	draw_piece( (m, _), H),
	write('   '),

	dL(2, T).

%% draw large, medium and small at n = 3
dL(N, [H|T]):-
	N = 3,
	T \= [],
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
	N = 3,
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
	member( (l, _), H ),

	write(' | \\ '),
	draw_piece( (m, _), H),
	write(' /'),

	dL(4, T).

%% draw medium at n = 4
dL(N, [H|T]):-
	N = 4,

	write(' |   '),
	draw_piece( (m, _), H),
	write('  '),

	dL(4, T).


%% draw large at n = 5
dL(N, [H|T]):-
	N = 5,
	member( (l, _), H ),

	write(' |  \\'),
	draw_piece( (l, _), H),
	write('/ '),

	dL(5, T).

%% draw empty at n = 5
dL(N, [_|T]):-
	N = 5,

	write(' |      '),

	dL(5, T).

dL(_, []):- write(' |').


replace_board([H|T], 0, C, P, [R|T]):- !, replace_line(H, C, P, R).

replace_board([H|T], L, C, P, [H|R]):- L > -1, L1 is L-1, replace_board(T, L1, C, P, R), !.

replace_board(L, _, _, _, _, L).


replace_line([H|T], 0, P, [R|T]):- P = (l, _), replace(H, 2, P, R).

replace_line([H|T], 0, P, [R|T]):- P = (m, _), replace(H, 1, P, R).

replace_line([H|T], 0, P, [R|T]):- P = (s, _), replace(H, 0, P, R).

replace_line([H|T], C, P, [H|R]):- C > -1, C1 is C - 1, replace_line(T, C1, P, R), !.

replace_line(L, _, _, _, L).


replace([P|T], 0, P, [_|T]).

replace([_|T], 0, P, [P|T]).

replace([H|T], I, P, [H|R]):- I > -1, NI is I-1, replace(T, NI, P, R), !.

replace(L, _, _, L).
