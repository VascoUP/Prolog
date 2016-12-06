:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- include('display.pl').

vals_cls([(1, 3), (6, 1)]).
vals_lns([(1, 2), (6, 2)]).
trees([(2, 1), (4, 2), (2, 3), (4, 4), (6, 4), (1, 5), (2, 5), (4, 6)]).

% Tree -  2, tent - 1, empty space - 0

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
        nth1(Y, Board, Line),
        nth1(X, Line, 2), !,
        add_trees(Trees, Board).


% element_position(-X, -Y, -Board, +Value)
element_position(X, Y, Board, Value) :-
        nth1(Y, Board, Line),
        element(X, Line, Value), !.
element_position(_, _, _, _) :- !, fail.

% value_position(-X, -Y, -Board, +Value)
value_position(X, Y, Board, Value) :-
        nth1(Y, Board, Line),
        nth1(X, Line, Value), !.
value_position(_, _, _, _) :- !, fail.


% value_right(-X, -Y, -Board, +Value)
value_right(X, Y, Board, Values, NValues) :-
        X1 is X + 1,
        value_position(X1, Y, Board, Value),
        append([Value], Values, NValues), !.
value_right(_, _, _, Values, Values).

% value_left(-X, -Y, -Board, +Value)
value_left(X, Y, Board, Values, NValues) :-
        X1 is X - 1,
        value_position(X1, Y, Board, Value),
        append([Value], Values, NValues), !.
value_left(_, _, _, Values, Values).

% value_top(-X, -Y, -Board, -Value, +NValues)
value_top(X, Y, Board, Values, NValues) :-
        Y1 is Y - 1,
        value_position(X, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_top(_, _, _, Values, Values).

% value_down(-X, -Y, -Board, +Value)
value_down(X, Y, Board, Values, NValues) :-
        Y1 is Y + 1, 
        value_position(X, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_down(_, _, _, Values, Values).


% value_right_top(-X, -Y, -Board, +Value)
value_right_top(X, Y, Board, Values, NValues) :-
        X1 is X + 1,
        Y1 is Y - 1,
        value_position(X1, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_right_top(_, _, _, Values, Values).

% value_left_top(-X, -Y, -Board, +Value)
value_left_top(X, Y, Board, Values, NValues) :-
        X1 is X - 1,
        Y1 is Y - 1,
        value_position(X1, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_left_top(_, _, _, Values, Values).

% value_right_down(-X, -Y, -Board, -Value, +NValues)
value_right_down(X, Y, Board, Values, NValues) :-
        X1 is X + 1,
        Y1 is Y + 1,
        value_position(X1, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_right_down(_, _, _, Values, Values).

% value_left_down(-X, -Y, -Board, +Value)
value_left_down(X, Y, Board, Values, NValues) :-
        X1 is X - 1,
        Y1 is Y + 1,
        value_position(X1, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_left_down(_, _, _, Values, Values).


% adjancent_values(-X, -Y, -Board, +Values) 
adjancent_values(X, Y, Board, Values) :-
        value_right(X, Y, Board, [], NValues),
        value_left(X, Y, Board, NValues, NValues2),
        value_top(X, Y, Board, NValues2, NValues3),
        value_down(X, Y, Board, NValues3, Values), !.

% all_values_around(-X, -Y, -Board, +Values) 
all_values_around(X, Y, Board, Values) :-
        adjancent_values(X, Y, Board, NValues),
        value_right_top(X, Y, Board, NValues, NValues1),
        value_left_top(X, Y, Board, NValues1, NValues2),
        value_right_down(X, Y, Board, NValues2, NValues3),
        value_left_down(X, Y, Board, NValues3, Values), !.

        
% tents
tents:-
        trees(Trees),
        vals_cls(_),
        vals_lns(_), !,
        
        create_board(6, 6, Board),
        add_trees(Trees, Board),
        fill_empty_board(Board),
        
        display_board(Board), !.