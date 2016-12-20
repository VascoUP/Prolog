:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- include('display.pl').

% Tree -  0, tent - 1, empty space - 0


%board_info(NCols, NLines, Trees, Vals_Cls, Vals_Lns)
board_info(6, 6, 
        [(2, 1), (4, 2), (2, 3), (4, 4), (6, 4), (1, 5), (2, 5), (4, 6)],               %Trees
        [(1, 3), (6, 1)],                                                               %Vals_Cls
        [(1, 2), (6, 2)]).                                                              %Vals_Lns

board_info(7, 7,
        [(3, 1),(6, 1), (1, 2), (2, 3), (5, 3), (4, 5), (1, 6), (4, 6), (7, 7)],        %Trees
        [(1, 1), (2, 2), (3, 1), (4, 2), (5, 1), (6, 1), (7, 1)],                       %Vals_Cls
        [(1, 2), (2, 1), (3, 1), (4, 1), (5, 1), (6, 1), (7, 2)]).                      %Vals_Lns



% ================================
% ======= INITIALIZE BOARD =======
% ================================

%init_tents(-Board, -Trees, -Vals_Cls, -Vals_Lns)
init_board(Board, Trees, Vals_Cls, Vals_Lns) :-
        board_info(NCols, NLines, Trees, Vals_Cls, Vals_Lns),
        
        create_board(NCols, NLines, Board),
        add_trees(Trees, Board),
        fill_impossible_spaces(Board, Trees).
        
% create_board(+NCols, +NLines, -Board)
create_board(NCols, NLines, Board) :-
        length(Board2, NLines),
        create_lines(NCols, Board2, Board).

% create_lines(+NCols, +Board, -BoardRes)
create_lines(_, [], []).
create_lines(NCols, [_|R], [N|T]) :-
        length(N, NCols),
        !, create_lines(NCols, R, T).


% add_trees(+TreeCoords, +Board)
add_trees([], _).
add_trees([(X, Y)|Trees], Board) :-
        value_position(X, Y, Board, 0), !,
        add_trees(Trees, Board).


% fill_impossible_spaces(+Board, +TreeCoords, +Column, +Line)
fill_impossible_spaces(Board, TreeCoords, Column, Line) :-
        nth1(1, Board, L), length(L, N),
        Column > N, % if the column is greater than the size of the line then we should move one line below
        Line1 is Line + 1, !,
        fill_impossible_spaces(Board, TreeCoords, 1, Line1).

fill_impossible_spaces(Board, _, _, Line) :-
        length(Board, N),
        Line > N, !. % if the line is greater than the size of the board then we should end here

fill_impossible_spaces(Board, TreeCoords, Column, Line) :-
        nth1(_, TreeCoords, (Column, Line)), % if this position has a tree in it then we should move 2 position forward
        Column1 is Column + 2, !,
        fill_impossible_spaces(Board, TreeCoords, Column1, Line).

fill_impossible_spaces(Board, TreeCoords, Column, Line) :-
        Column1 is Column - 1,
        Column2 is Column + 1,
        Line1 is Line - 1,
        Line2 is Line + 1,
        \+ nth1(_, TreeCoords, (Column1, Line)),
        \+ nth1(_, TreeCoords, (Column2, Line)),
        \+ nth1(_, TreeCoords, (Column, Line1)),
        \+ nth1(_, TreeCoords, (Column, Line2)),
        % if it has no trees around this position then, this position, will never have a tent
        value_position(Column, Line, Board, 0), !,
        fill_impossible_spaces(Board, TreeCoords, Column2, Line).

fill_impossible_spaces(Board, TreeCoords, Column, Line) :-        
        Column1 is Column + 1, !,
        fill_impossible_spaces(Board, TreeCoords, Column1, Line).
        

% fill_impossible_spaces(+Board, +TreeCoords)
fill_impossible_spaces(Board, TreeCoords) :-
        fill_impossible_spaces(Board, TreeCoords, 1, 1).
        


% ===============================
% ==== GET VALUES FROM BOARD ====
% ===============================


% value_position(+X, +Y, +Board, -Value)
value_position(X, Y, Board, Value) :-
        nth1(Y, Board, Line),
        nth1(X, Line, Value), !.
value_position(_, _, _, _) :- !, fail.


% value_right(+X, +Y, +Board, -Value)
value_right(X, Y, Board, Values, NValues) :-
        X1 is X + 1,
        value_position(X1, Y, Board, Value),
        append([Value], Values, NValues), !.
value_right(_, _, _, Values, Values).

% value_left(+X, +Y, +Board, -Value)
value_left(X, Y, Board, Values, NValues) :-
        X1 is X - 1,
        value_position(X1, Y, Board, Value),
        append([Value], Values, NValues), !.
value_left(_, _, _, Values, Values).

% value_top(+X, +Y, +Board, -Value)
value_top(X, Y, Board, Values, NValues) :-
        Y1 is Y - 1,
        value_position(X, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_top(_, _, _, Values, Values).

% value_down(+X, +Y, +Board, -Value)
value_down(X, Y, Board, Values, NValues) :-
        Y1 is Y + 1, 
        value_position(X, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_down(_, _, _, Values, Values).

% value_bottom_right(+X, +Y, +Board, -Value)
value_bottom_right(X, Y, Board, Values, NValues) :-
        Y1 is Y + 1,
        X1 is X + 1,
        value_position(X1, Y1, Board, Value),
        append([Value], Values, NValues), !.
value_bottom_right(_, _, _, Values, Values).

% value_bottom_left(+X, +Y, +Board, -Value)
value_pos(X, Y, Board, Values, NValues) :-
        value_position(X, Y, Board, Value),
        append([Value], Values, NValues), !.
value_pos(_, _, _, Values, Values).

% adjancent_values(+X, +Y, +Board, -Values) 
adjancent_values(X, Y, Board, Values) :-
        value_right(X, Y, Board, [], NValues),
        value_left(X, Y, Board, NValues, NValues2),
        value_top(X, Y, Board, NValues2, NValues3),
        value_down(X, Y, Board, NValues3, Values), !.

% square_value(+X, +Y, +Board, -Values)
square_value(X, Y, Board, Values) :-
        value_pos(X, Y, Board, [], NValues),
        value_right(X, Y, Board, NValues, NValues2),
        value_down(X, Y, Board, NValues2, NValues3),
        value_bottom_right(X, Y, Board, NValues3, Values), !.

% get_arr_board(+Board, -Res)
get_arr_board([], []).
get_arr_board([H|T], Res) :-
        get_arr_board(T, Res2),
        append(H, Res2, Res). 



% ================================
% ========= RESTRICTIONS =========
% ================================

% sum_trees(+Trees, +Board)
sum_trees([], _):- !.
sum_trees( [(X, Y) | Trees], Board ) :-
        adjancent_values(X, Y, Board, Values),
        sum( Values, #>, 0 ),
        !, sum_trees( Trees, Board ).


% sum_line(+Board, +Value, +Line)
sum_line( Board, Value, Line ) :-
        nth1( Line, Board, H ),
        sum(H, #=, Value).

% sum_lines(+Board, +Vals_Line)
sum_lines( _, [] ).
sum_lines( Board, [(Line, Value) | T] ) :-
       sum_line( Board, Value, Line ),
       !, sum_lines( Board, T ).


% sum_col(+Board, +Value, +Column)
sum_col( Board, Value, Column ) :-
        transpose(Board, Res),
        nth1(Column, Res, Vals),
        sum(Vals, #=, Value).

% sum_cols(+Board, +Vals_Cols)
sum_cols( _, [] ).
sum_cols( Board, [(Column, Value) | T] ) :-
       sum_col( Board, Value, Column ),
       !, sum_cols( Board, T ).


% sum_squares_cols(+Board, +X, +Y)
sum_square(Board, X, Y) :-
        nth1(1, Board, N),
        length(N, NCols),
        X >= NCols,
        Y1 is Y + 1, !,
        sum_square(Board, 1, Y1).
sum_square(Board, _, Y) :-
        length(Board, NLines),
        Y >= NLines, !.
sum_square(Board, X, Y) :-
        square_value(X, Y, Board, Square),
        sum(Square, #<, 2), 
        X1 is X + 1, !,
        sum_square(Board, X1, Y).

% sum_squares(+Board)
sum_square( Board ) :-
        !, sum_square( Board, 1, 1 ).
   
% sum_board(+Board, +TreeCoords)                    
sum_board(Board, TreeCoords) :-
        get_arr_board(Board, Res),
        length(TreeCoords, Total),
        sum(Res, #=, Total).
        


% ==============================
% ============ MISC ============
% ==============================

% domain_board(+Board, +Min, +Max)
domain_board([], _, _).
domain_board([H|T], Min, Max) :-
        domain(H, Min, Max), !,
        domain_board(T, Min, Max).

% labeling_board(+Board)
labeling_board([]).
labeling_board([H|T]) :-
        labeling([], H), !,
        labeling_board(T).


% solve_problem(+Board, +Trees, +Vals_Cls, +Vals_Lns)
solve_problem(Board, Trees, Vals_Cls, Vals_Lns) :-
        domain_board(Board, 0, 1),
        
        sum_board(Board, Trees),
        sum_trees(Trees, Board),
        sum_lines(Board, Vals_Lns),
        sum_cols(Board, Vals_Cls),
        sum_square(Board),
        
        labeling_board(Board).



% ===============================
% ============ TENTS ============
% ===============================

% tents
tents:-
        init_board(Board, Trees, Vals_Cols, Vals_Lines),
        
        display_board(Board, Trees, Vals_Cols, Vals_Lines), !,
        
        solve_problem(Board, Trees, Vals_Cols, Vals_Lines),

        display_board(Board, Trees, Vals_Cols, Vals_Lines).