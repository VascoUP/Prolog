:- include('menu.pl').
:- use_module(library(random)).

player(r).
player(b).

modeGame(player).
modeGame(computer).

pieces( [s, m , l ] ).

tier(1).
tier(2).
tier(3).

tier_pieces(1, [ (m, 1, 1) ]).
tier_pieces(2, [ (m, 0, 1), (m, 2, 1), (m, 1, 0), (m, 1, 2) ]).
tier_pieces(3, [ (s, 0, 0), (l, 0, 0), (s, 0, 2), (l, 0, 2),
                                                                        (s, 2, 0), (l, 2, 0), (s, 2, 2), (l, 2, 2) ]).

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
%%                      FUNCTIONS
%%-----------------------

otrio :- mainMenu.

game_cicle:-
        board(Board), player(Player),
        get_movable_pieces(Player, Mv1, Mv2), !,
  cicle(player, Board, Player, Mv1, Mv2).

%% Mv1 are the available pieces of the current player
%% Mv2 are the available pieces of the other player
cicle(Mode, Board, Player, Mv1, Mv2):-
        print_player(Player),
        display_board(Board),

        nl, write('---------------------------'),
        nl, write('      Avaiable pieces      '), nl,
        nl, write(Mv1), nl,
        nl, write('---------------------------'),
        nl,

        play(Mode, Board, Player, Mv1, Mv2, Board2, PlayerC, Mv1C, Mv2C), !,
        not(end_game(Board2, Player)), !,
        next_player(PlayerC, Pl2),
        verify_movable_pieces(Board2, PlayerC, Pl2, Mv1C, Mv2C), nl, !,
        (
                ( verify_pieces(Mv1C) ; has_options(Board2, Mv1C, Player2, 0, _, _, _) ) ->
                        equal_player(PlayerC, Player2), next_mv(Mv1C, NextMv1), next_mv(Mv2C, NextMv2)
                ;
                        equal_player(Player, Player2), next_mv(Mv2C, NextMv1), next_mv(Mv1C, NextMv2)
        ), cicle(Mode, Board2, Player2, NextMv1, NextMv2).


%%--------------------
%% HANDLE GAME CICLES
%%--------------------

next_cicle(Board, Line, Column, (Piece, Player), Board2, Player, Player2, Mv1, Mv2, NextMv1, NextMv2):-
        replace_board(Board, Line, Column, (Piece, Player), Board2),
        remove_piece(Mv1, Piece, NextMv2), !,
        next_player(Player, Player2, Mv2, NextMv1).

next_cicle(Board, _, _, _, Board, Player, Player, Mv1, Mv2, NextMv1, NextMv2):-
        nl, write('---------------------------------'),
        nl, write('You are unable to play that piece'),
        nl, write('---------------------------------'),
        nl,
        sleep(2),
        next_mv(Mv1, NextMv1),
        next_mv(Mv2, NextMv2).

next_player(Player, PlayerC, Mv2, Mv1C):-
        next_player(Player, PlayerC),
        next_mv(Mv2, Mv1C).

next_player(Player, Player, Mv1, Mv2, Mv1, Mv2).

next_mv(Mv1, Mv1).

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
        Piece = Pr, N \= 0, !, X is N-1.

remove_piece([H|T], Piece, [H|R]):-
        remove_piece(T, Piece, R).

remove_piece(L, _, L):-fail.

%%------
%% "AI"
%%------

play(player, Board, Player, Mv, Mv2, BoardC, PlayerC, MvC, Mv2C):-
        p_play(Player, Line, Column, Pair),
        next_cicle(Board, Line, Column, Pair, BoardC, Player, PlayerC, Mv, Mv2, MvC, Mv2C), !.

play(computer, Board, Player, Mv, Mv2, BoardC, PlayerC, MvC, Mv2C):-
        e_play(Board, Mv, Player, Line, Column, Pair),
        next_cicle(Board, Line, Column, Pair, BoardC, Player, PlayerC, Mv, Mv2, MvC, Mv2C), !.

p_play(Player, LineC, ColumnC, PairC):-
        ask_piece(Piece), !, ask_coords(ColumnC, LineC), !,
        piece_to_player(Piece, Player, PairC).

e_play(Board, Mv, Player, LineC, ColumnC, PairC):-
        next_win(Board, Mv, Player, LineC, ColumnC, PairC), !.

e_play(Board, Mv, Player, LineC, ColumnC, PairC):-
        next_player(Player, Player2),
        next_win(Board, Mv, Player2, LineC, ColumnC, PairC), !.

e_play(Board, Mv, Player, LineC, ColumnC, PairC):-
        play_tier(1, Board, Mv, Player, LineC, ColumnC, PairC), !.

e_play(Board, Mv, Player, LineC, ColumnC, PairC):-
        analyze_board(50, Board, Player, Mv, LineC, ColumnC, PairC), !.

e_play(Board, Mv, Player, LineC, ColumnC, PairC):-
        has_options(Board, Mv, Player, 0, LineC, ColumnC, PairC), !.

random_piece(P) :- random(0, 2, Y), (Y = 0 -> P = 's'; Y = 1 -> P = 'm'; Y = 2 -> P = 'l').
random_coords(C, L) :- random(0, 2, C), random(0, 2, L).

analyze_board(0, _, _, _, _, _, _):-!, fail.

analyze_board(_, Board, Player, Mv, LineC, ColumnC, PairC) :-
        random_piece(Piece),
        remove_piece(Mv, Piece, _),
        pTp(Piece, Player, PairC),
        random_coords(ColumnC, LineC),
        replace_board(Board, LineC, ColumnC, (Piece, Player), _), !.

analyze_board(N, Board, Player, Mv, LineC, ColumnC, PairC) :-
        N1 is N-1, !,
        analyze_board(N1, Board, Player, Mv, LineC, ColumnC, PairC).

play_tier(Tier, _, _, _, _, _, _):-
        Tier > 3, !, fail.

play_tier(Tier, Board, Mv, Player, LineC, ColumnC, PairC):-
        tier_pieces(Tier, Elems),
        play_tier_elems(Elems, Board, Mv, Player, LineC, ColumnC, PairC), !.

play_tier(Tier, Board, Mv, Player, LineC, ColumnC, PairC):-
        T1 is Tier + 1, !,
        play_tier(T1, Board, Mv, Player, LineC, ColumnC, PairC).

play_tier_elems([], _, _, _, _, _, _):-!, fail.

play_tier_elems([Element|_], Board, Mv, Player, LineC, ColumnC, PairC):-
        Element = (Piece, Line, Column),
        replace_board(Board, Line, Column, (Piece, Player), _),
        remove_piece(Mv, Piece, _), !,
        equal_pair((Piece, Player), PairC),
        equal_line(Line, LineC),
        equal_column(Column, ColumnC).

play_tier_elems([_|T], Board, Mv, Player, LineC, ColumnC, PairC):-
        !, play_tier_elems(T, Board, Mv, Player, LineC, ColumnC, PairC).

random_tier_elem(Tier, Element):-
        tier_pieces(Tier, Pieces),
        length(Pieces, N),
        random(0, N, Element).

equal_player(Player, Player).
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

has_options(_, _, _, 3, _, _, _):-!, fail.

has_options(Board, Mv, Player, Line, Line, ColumnC, PairC):-
        has_options_line(Board, Mv, Player, Line, 0, ColumnC, PairC), !.

has_options(Board, Mv, Player, Line, LineC, ColumnC, PairC):-
        L1 is Line + 1, !,
        has_options(Board, Mv, Player, L1, LineC, ColumnC, PairC).

has_options_line(_, _, _, _, 3, _, _):-!, fail.

has_options_line(Board, Mv, Player, Line, Column, Column, PairC):-
        has_options_position(Board, Line, Column, (s, Player), Mv, PairC), !.

has_options_line(Board, Mv, Player, Line, Column, ColumnC, PairC):-
        C1 is Column + 1, !,
        has_options_line(Board, Mv, Player, Line, C1, ColumnC, PairC).

has_options_position(Board, Line, Column, Pair, Mv, Pair):-
        replace_board(Board, Line, Column, Pair, _),
        Pair = (Piece, _),
        remove_piece(Mv, Piece, _), !.

has_options_position(Board, Line, Column, Pair, Mv, PairC):-
        Pair = (Piece, Player),
        next_piece(Piece, Piece2, _), !,
        has_options_position(Board, Line, Column, (Piece2, Player), Mv, PairC), !.

has_options_position(_, _, _, _, _, _):-!, fail.


%%-----------------
%% VERIFY END GAME
%%-----------------

end_game(Board, Player):-
        not(verify_diagonal(Board, Player)),
        not(verify_columns(Board, Player)),
        not(verify_board(Board, Player)), !,
        fail.

end_game(_, _):-!.

verify_movable_pieces(Board, Player1, Player2, Mv1, Mv2):-
        ( not(verify_pieces(Mv1)) ; not(verify_pieces(Mv2)) ;
                not(has_options(Board, Mv1, Player1, 0, _, _, _)) ; not(has_options(Board, Mv2, Player2, 0, _, _, _))
        ), !.

verify_movable_pieces(_, _, _, _, _):-
        nl,
        write('------------------------------'), nl,
        write('Unable to play any more pieces'), nl,
        write('            DRAW'), nl,
        write('------------------------------'), nl,
        nl,
        sleep(2), !, fail.

verify_pieces([]).

verify_pieces([H|_]):-
        H = (X, _), X \= 0, !, fail.

verify_pieces([_|T]):-
        !, verify_pieces(T).

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

%% Verifies position and lines
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
        nl, write('     * RED TURN * '), nl, nl.

print_player(Player):-
        Player = b, !,
        nl, write('      * BLUE TURN * '), nl, nl.


print_player(_):-
        nl, write('     ERROR'), nl, nl.

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

