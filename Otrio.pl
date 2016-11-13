:- use_module(library(random)).
:- use_module(library(system)).
:- ensure_loaded('menu.pl').

%% Player type (r - red, b - blue)
player(r).
player(b).

%% Game mode (We can play player vs player, player vs computer or computer vs computer)
modeGame(player).
modeGame(computer).

%% Difficulty (player vs computer or computer vs computer can be played in the easy or hard mode)
difficulty(easy).
difficulty(hard).

%% Different pieces (s - small, m - medium, l -large, e - blank space)
pieces( [s, m , l, e] ).

%% Different tiers distributed by importance
tier(1).
tier(2).
tier(3).

%% Different pieces for each tier
tier_pieces(1, [ (m, 1, 1) ]).
tier_pieces(2, [ (m, 0, 0), (m, 2, 0) ]).
tier_pieces(3, [ (m, 0, 1), (m, 1, 0) ]).
tier_pieces(4, [ (s, 0, 0), (l, 0, 0), (s, 0, 2), (l, 0, 2),
                 (s, 2, 0), (l, 2, 0), (s, 2, 2), (l, 2, 2) ]).


%%Player r movable pieces
movable_pieces(r, [
                        (6, s),
                        (6, m),
                        (6, l)
                  ]).

%%Player b movable pieces
movable_pieces(b, [
                        (6, s),
                        (6, m),
                        (6, l)
                  ]).

%%Empty board
board( [
         [ [e, e, e], [e, e, e], [e, e, e] ],
         [ [e, e, e], [e, e, e], [e, e, e] ],
         [ [e, e, e], [e, e, e], [e, e, e] ]
             ]).


%%-----------------------
%%  FUNCTIONS
%%-----------------------

%%Creation of predicate not
not(Arg1):- \+ Arg1.

%%"Main function"
otrio :- mainMenu.

%%Predicate that creates the game cicle
game_cicle(ModeGame1, ModeGame2, Difficulty):-
  board(Board), player(Player),
  get_movable_pieces(Player, Mv1, Mv2), !,
  cicle(Board, Player, Mv1, Mv2, ModeGame1, ModeGame2, Difficulty).

%% Mv1 are the available pieces of the current player
%% Mv2 are the available pieces of the other player
%% Creation of the cicle to play
cicle(Board, Player, Mv1, Mv2, ModeGame1, ModeGame2, Difficulty):-
        display_board(Board),

        print_player(Player),
        display_mv(Mv1),

        play(ModeGame1, Difficulty, Board, Player, Mv1, Mv2, Board2, PlayerC, Mv1C, Mv2C, Replay), !,
        not(cicle_end_game(Board2, Player)), !,
        next_player(PlayerC, Pl2),
        has_movable_pieces(Board2, PlayerC, Pl2, Mv1C, Mv2C), nl, !,
        (
                ( not(Replay), not(verify_pieces(Mv1C)), has_options(Board2, Mv1C, Pl2, 0, _, _, _) ) ->
                        equal_player(PlayerC, Player2), equal_mv(Mv1C, NextMv1), equal_mv(Mv2C, NextMv2),
                        equal_mode(ModeGame2, NextMode1), equal_mode(ModeGame1, NextMode2)
                ;
                        equal_player(Player, Player2), equal_mv(Mv1C, NextMv1), equal_mv(Mv2C, NextMv2),
                        equal_mode(ModeGame1, NextMode1), equal_mode(ModeGame2, NextMode2)
        ), cicle(Board2, Player2, NextMv1, NextMv2, NextMode1, NextMode2, Difficulty).

%%Predicates that creates the turn of each player and allows them to play

play(player, _, Board, Player, Mv, Mv2, BoardC, PlayerC, MvC, Mv2C, Replay):-
        p_play(Player, Line, Column, Pair),
        next_cicle(Board, Line, Column, Pair, BoardC, Player, PlayerC, Mv, Mv2, MvC, Mv2C, Replay), !.

play(computer, Difficulty, Board, Player, Mv, Mv2, BoardC, PlayerC, MvC, Mv2C, false):-
        sleep(2),
        e_play(Difficulty, Board, Mv, Player, Mv2, Line, Column, Pair),
        next_cicle(Board, Line, Column, Pair, BoardC, Player, PlayerC, Mv, Mv2, MvC, Mv2C, _), !.


p_play(Player, LineC, ColumnC, PairC):-
        ask_piece(Piece), !, ask_coords(ColumnC, LineC), !,
        piece_to_player(Piece, Player, PairC).


%%--------------------
%% HANDLE GAME CICLES
%%--------------------

next_cicle(Board, Line, Column, (Piece, Player), Board2, Player, Player2, Mv1, Mv2, NextMv1, NextMv2, false):-
        replace_board(Board, Line, Column, (Piece, Player), Board2),
        remove_piece(Mv1, Piece, NextMv2), !,
        next_player(Player, Player2, Mv2, NextMv1).

next_cicle(Board, _, _, _, Board, Player, Player, Mv1, Mv2, NextMv1, NextMv2, true):-
        nl, write('---------------------------------'),
        nl, write('You are unable to play that piece'),
        nl, write('---------------------------------'),
        nl,
        sleep(2),
        equal_mv(Mv1, NextMv1),
        equal_mv(Mv2, NextMv2).


next_player(Player, PlayerC, Mv2, Mv1C):-
        next_player(Player, PlayerC),
        equal_mv(Mv2, Mv1C).

next_player(Player, Player, Mv1, Mv2, Mv1, Mv2).


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

%%Asks the piece to put on the board
ask_piece(P):-
        pieces(Ps), repeat,
        ask_pieceName('Piece: ', P), member(P, Ps), nl.

%%Asks the coordinates to put the piece
ask_coords(C, L):-
  repeat,
        ask_column('Column: ', C), C \= 'a', C \= 'b', C \= 'c' , nl,
        repeat,
        ask_line('Line: ', L), L > -1, L < 3, nl.

%%Allows the user to choose the piece
ask_pieceName(X, Y):- nl, write(X), nl, get_char(Y), get_char(_).

%%Allows the user to choose the column
ask_column(X, C):- nl, write(X), nl, get_char(Y), get_char(_),
                                        (Y = 'a' -> C = 0; Y = 'b' -> C = 1; Y ='c' -> C = 2).

%%Allows the user yo choose the line
ask_line(X, L):- nl, write(X), nl, get_char(Y), get_char(_),
                (Y = '1' -> L = 0; Y = '2' -> L = 1; Y = '3' -> L = 2).



%%-------------
%% MOVE PIECES
%%-------------

%%Replaces the current board with the new board with the piece chosen by the user

replace_board([H|T], 0, C, P, [R|T]):- !, replace_line(H, C, P, R).

replace_board([H|T], L, C, P, [H|R]):- L > -1, L1 is L-1, replace_board(T, L1, C, P, R), !.

replace_board(L, _, _, _, _, L).


%%Replaces the current line with the new line with the piece chosen by the user

replace_line([H|T], 0, P, [R|T]):- P = (l, _), replace(H, 2, P, R).

replace_line([H|T], 0, P, [R|T]):- P = (m, _), replace(H, 1, P, R).

replace_line([H|T], 0, P, [R|T]):- P = (s, _), replace(H, 0, P, R).

replace_line([H|T], C, P, [H|R]):- C > -1, C1 is C - 1, replace_line(T, C1, P, R), !.

replace_line(L, _, _, _, L).

%%Verifies if the game cell is occupied, if it is returns fail
replace([H|T], 0, _, [_|T]):- H \= e, !, fail.

replace([_|T], 0, P, [P|T]).

replace([H|T], I, P, [H|R]):- I > -1, NI is I-1, replace(T, NI, P, R), !.

replace(L, _, _, L):-fail.

%%Removes the piece chosen from the player list of pieces
remove_piece([(N, Pr)|T], Piece, [(X, Pr)|T]):-
        Piece = Pr, N \= 0, !, X is N-1.

remove_piece([H|T], Piece, [H|R]):-
        remove_piece(T, Piece, R).

remove_piece(L, _, L):-fail.

%%------
%% "AI"
%%------

%% Exclusive to the easy mode
e_play(easy, Board, Mv, Player, _, LineC, ColumnC, PairC):-
        random(0, 100, X), X < 50,
        next_win(Board, Mv, Player, LineC, ColumnC, PairC), !.

%% Exclusive to the hard mode
e_play(hard, Board, Mv, Player, _,  LineC, ColumnC, PairC):-
        next_win(Board, Mv, Player, LineC, ColumnC, PairC), !.

e_play(_, Board, _, Player, Mv2, LineC, ColumnC, (Piece, Player)):-
        next_player(Player, Player2),
        next_win(Board, Mv2, Player2, LineC, ColumnC, (Piece, _)), !.

%% Exclusive to the hard mode
e_play(hard, Board, Mv, Player, _, LineC, ColumnC, (Piece, Player)):-
        play_tier(1, Board, Mv, Player, LineC, ColumnC, (Piece, _)), !.

e_play(_, Board, Mv, Player, _, LineC, ColumnC, PairC):-
        analyze_board(50, Board, Player, Mv, LineC, ColumnC, PairC), !.

e_play(_, Board, Mv, Player, _, LineC, ColumnC, PairC):-
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
        not(tier(Tier)), !, fail.

play_tier(Tier, Board, Mv, Player, LineC, ColumnC, PairC):-
        tier_pieces(Tier, Elems),
        possible_tier_pieces(Board, Mv, Player, Elems, PossibleElems),
        PossibleElems \= [], !,
        random_elem(PossibleElems, Elem),
        Elem = (Piece, LineC, ColumnC),
        equal_pair((Piece, Player), PairC).

play_tier(Tier, Board, Mv, Player, LineC, ColumnC, PairC):-
        !, T1 is Tier+1,
        play_tier(T1, Board, Mv, Player, LineC, ColumnC, PairC).


possible_tier_pieces(_, _, _, [], []).

possible_tier_pieces(Board, Mv, Player, [H|T], [H|R]):-
    H = (Piece, Line, Column),
    replace_board(Board, Line, Column, (Piece, Player), _),
    remove_piece(Mv, Piece, _), !,
    possible_tier_pieces(Board, Mv, Player, T, R).

possible_tier_pieces(Board, Mv, Player, [_|T], R):-
    !, possible_tier_pieces(Board, Mv, Player, T, R).


random_elem(Elements, Elem):-
        length(Elements, N),
        random(0, N, X),
        element_position(Elements, X, Elem).


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

cicle_end_game(Board, Player):-
  !, end_game(Board, Player),
  print_win(Board, Player).


end_game(Board, Player):-
        not(verify_diagonal(Board, Player)),
        not(verify_columns(Board, Player)),
        not(verify_board(Board, Player)), !,
        fail.

end_game(_, _).


has_movable_pieces(Board, Player1, Player2, Mv1, Mv2):-
        ( not(verify_pieces(Mv1)) ; not(verify_pieces(Mv2)) ;
                not(has_options(Board, Mv1, Player1, 0, _, _, _)) ; not(has_options(Board, Mv2, Player2, 0, _, _, _))
        ), !.

has_movable_pieces(Board, _, _, _, _):-
        display_board(Board),
        nl,
        write('------------------------------'), nl,
        write('Unable to play any more pieces'), nl,
        write('            DRAW'), nl,
        write('------------------------------'), nl,
        nl,
        sleep(5), !, fail.


verify_pieces([]).

verify_pieces([H|_]):-
        H = (X, _), X \= 0, !, fail.

verify_pieces([_|T]):-
        !, verify_pieces(T).


verify_diagonal(Board, Player):-
        element_board(Board, 0, 0, Position1),
        element_board(Board, 1, 1, Position2),
        element_board(Board, 2, 2, Position3),
        verify_diagonal(Position1, Position2, Position3, Player).

verify_diagonal(Board, Player):-
        element_board(Board, 2, 0, Position1),
        element_board(Board, 1, 1, Position2),
        element_board(Board, 0, 2, Position3),
        verify_diagonal(Position1, Position2, Position3, Player).

verify_diagonal(Position1, Position2, Position3, Player):-
  equal_pieces(Position1, Position2, Position3, Player).

verify_diagonal(Position1, Position2, Position3, Player):-
  ascending_pieces(Position1, Position2, Position3, Player).


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
        verify_column(Position1, Position2, Position3, Player).

verify_column(Position1, Position2, Position3, Player):-
  equal_pieces(Position1, Position2, Position3, Player).

verify_column(Position1, Position2, Position3, Player):-
  ascending_pieces(Position1, Position2, Position3, Player).

column_pieces([(m, Player)|_], Position3, (Piece, Player)):-
        !, equal_pieces(Position3, (Piece, Player)).

column_pieces([_|T], Position3, (Piece, Player)):-
        column_pieces(T, Position3, (Piece, Player)).

column_pieces([_], _, _, _):-fail.

column_pieces([(Piece, Player)|_], Position2, Position3, Player):-
        wanted_piece(Piece, Piece2), Piece2 \= e,
        column_pieces(Position2, Position3, (Piece2, Player)).

column_pieces([_|T], Position2, Position3, Player):-
        column_pieces(T, Position2, Position3, Player).


%% Verifies position and lines
verify_board([], _):-!, fail.

verify_board([H|_], Player):-
        verify_line_positions(H, Player), !.

verify_board([H|_], Player):-
        verify_line(H, 0, Player), !.

verify_board([_|T], Player):-
        !, verify_board(T, Player).


verify_line_positions([ ], _):-!, fail.

verify_line_positions([H|_], Player):-
        verify_position(H, Player), !.

verify_line_positions([_|T], Player):-
        verify_line_positions(T, Player).


verify_position(Position, Player):-
        count_player_pieces(Position, Player, Counter),
        Counter = 3, !.


verify_line([], _, _):-!, fail.

verify_line([H|T], 0, Player):-
        line_win_position([H|T], H, Player).

verify_line([H|T], 1, (Piece, Player)):-
        member((Piece, Player), H),
        verify_line(T, 2, (Piece, Player)).

verify_line([H|T], 1, (Piece, Player)):-
        member((m, Player), H),
        wanted_piece(Piece, Piece2),
        verify_line(T, 2, (Piece2, Player)).

verify_line([H|_], 2, (Piece, Player)):-

        member((Piece, Player), H).

verify_line(_, _, _):-!, fail.


line_win_position(_, [], _):-!, fail.

line_win_position([_|T], [(Piece, Player)|_], Player):-
        verify_line(T, 1, (Piece, Player)), !.

line_win_position(Line, [_|T], Player):-
        !, line_win_position(Line, T, Player).


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


ascending_pieces([], _):-fail.

ascending_pieces([Piece|_], Piece2):-
        Piece = (Type, Player),
        Piece2 = (Type2, Player),
        wanted_piece(Type, Type2), !,
        write('Yes'), nl.

ascending_pieces([_|T], Piece):-
        !, ascending_pieces(T, Piece).

ascending_pieces([], _, _):-fail.

ascending_pieces([(m, Player)|_], Position3, (Type, Player)):-
        ascending_pieces(Position3, (Type, Player)).

ascending_pieces([_|T], Position3, Piece):-
        !, ascending_pieces(T, Position3, Piece).

ascending_pieces([_], _, _, _):-fail.

ascending_pieces([(Piece, Player)|_], Position2, Position3, Player):-
        ascending_pieces(Position2, Position3, (Piece, Player)).

ascending_pieces([_|T], Position2, Position3, Player):-
        !, ascending_pieces(T, Position2, Position3, Player).


element_board([H|_], C, 0, Position):-
        element_position(H, C, Position).

element_board([_|T], C, L, Position):-
        L1 is L - 1, element_board(T, C, L1, Position).

element_position([Element|_], 0, Element).

element_position([_|T], C, Element):-
        C1 is C - 1, element_position(T, C1, Element).


%%--------
%% EQUALS
%%--------

equal_mode(ModeGame, ModeGame).
equal_player(Player, Player).
equal_mv(Mv, Mv).
equal_pair(Pair, Pair).
equal_line(Line, Line).
equal_column(Column, Column).


%% -----------
%% DRAW BOARD
%% -----------

print_win(Board, Player):-
  !, display_board(Board),
  print_win_player(Player), sleep(5).

print_win_player(Player):-
  Player = r, !,
  nl, write('* PLAYER '),
  write('RED '),
  write('WON THE GAME *'), nl, nl.

print_win_player(_):-
  nl, write('* PLAYER '),
  write('BLUE '),
  write('WON THE GAME *'), nl, nl.


print_player(Player):-
        Player = r, !,
        nl, write('     * RED TURN * '), nl, nl.

print_player(Player):-
        Player = b, !,
        nl, write('     * BLUE TURN * '), nl, nl.


draw_piece( (T, C), B ):-
        player(C), C = r,
        member( (T, C), B ),
        write('@').

draw_piece( (T, C), B ):-
        player(C), C = b,
        member( (T, C), B ),
        write('&').

draw_piece( _, _ ):- write(' ').

display_mv(Mv):-
  nl, write('--------------------'),
  nl, write('  Available pieces'),
  nl, nl, display_mv_info(Mv),
  nl, write('--------------------').

display_mv_info([]).

display_mv_info([H|T]):-
  !, display_type_info(H),
  display_mv_info(T).

display_type_info((N, Type)):-
  write('-> '), write(N),
  Type = s,
  write(' small pieces'), nl.

display_type_info((_, Type)):-
  Type = m,
  write(' medium pieces'), nl.

display_type_info((_, Type)):-
  Type = l,
  write(' large pieces'), nl.

display_board(B):- !, cls, write('     a       b       c'), nl,
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
