:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- include('display.pl').

% create_board(-NCols, -NLines, +Board)
create_board(NLines, NCols, Board) :-
        length(Board2, NLines),
        create_lines(NCols, Board2, Board).

% create_lines(-NCols, -Board, +BoardRes)
create_lines(_, [], []).
create_lines(NCols, [_|R], [N|T]) :-
        length(N, NCols),
        !, create_lines(NCols, R, T).


% add_trees(-TreeCoords, -Board, +BoardRes)
add_trees([], _).
add_trees([(X, Y)|Trees], Board) :-
        nth0(Y, Board, Line),
        nth0(X, Line, 0), !,
        add_trees(Trees, Board).

tents :-
        create_board(6, 6, Board),
        add_trees([(1, 0), (3, 1), (1, 2), (3, 3), (5, 3), (0, 4), (1, 4), (3, 5)], Board),
        display_board(Board).
