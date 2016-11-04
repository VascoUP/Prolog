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
    	 [ [e, e, e], [e, e, e], [e, e, e] ],
    	 [ [e, e, e], [e, e, e], [e, e, e] ]
	     ]).

game_cicle:- write('Hello darkness'), nl, board(B), !,
  repeat, display_board(B),
  %%input,
  !.

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
	dB(1, B), !, sleep(1), fail.

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
