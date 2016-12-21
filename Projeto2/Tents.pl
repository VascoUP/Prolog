:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- include('display.pl').

% Tree -  0, tent - 1, empty space - 0


%board_info(NCols, NLines, Trees, Vals_Cls, Vals_Lns)
%board_info(1, 1, 
%        [(1, 1)],
%        [],
%        []).

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

%init_board(+NCols, +NLines, +Trees, -Board)
init_board(NCols, NLines, Trees, Board) :-        
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
% ========== GENERATOR ==========
% ===============================

% validate_tree(+NCols, +NLines, +Trees)
validate_tree(NCols, NLines, Trees) :-
        init_board(NCols, NLines, Trees, Board), !,
        solve_problem(Board, Trees, [], []).

% place_tree(+NCols, +NLines, +X, +Y, +Prob, +Trees, -ResProb, -ResTrees)
place_tree(NCols, NLines, X, Y, Prob, Trees, ResProb, ResTrees) :-
        random(1, 100, Value),
        Value =< Prob,

        validate_tree(NCols, NLines, [(X, Y) | Trees]),
        
        ResProb is 10,
        append([(X, Y)], Trees, ResTrees).

place_tree(_, _, _, _, Prob, Trees, ResProb, Trees) :-
        ResProb is Prob + 10.

% random_trees(+NCols, +NLines, +X, +Y, -Prob, -Trees)
random_trees(_, NLines, _, Y, Prob, Trees) :-
        Y > NLines,
        Trees = [],
        Prob is 10.
        
random_trees(NCols, NLines, X, Y, Prob, Trees) :-
        X > NCols,
        Y1 is Y + 1, !,
        random_trees(NCols, NLines, 1, Y1, Prob, Trees).

random_trees(NCols, NLines, X, Y, Prob, Trees) :-
        X1 is X + 1, !,
        random_trees(NCols, NLines, X1, Y, P, T),
        place_tree(NCols, NLines, X, Y, P, T, Prob, Trees).

% random_trees(+NCols, +NLines, -Trees)
random_trees(NCols, NLines, Trees) :-
        random_trees(NCols, NLines, 1, 1, _, Trees).

% generate_board(-Board, -Trees, -Vals_Cls, -Vals_Lns)
generate_board(Board, Trees, [], []) :-
        random(5, 10, NCols),
        random(5, 10, NLines),
        random_trees(NCols, NLines, Trees),
        init_board(NCols, NLines, Trees, Board).



% ===============================
% ==== GET VALUES FROM BOARD ====
% ===============================

% value_position(+X, +Y, +Board, -Value)
value_position(X, Y, Board, Value) :-
        !, nth1(Y, Board, Line),
        nth1(X, Line, Value).

% value_pos(+X, +Y, +Board, -Value)
value_pos(X, Y, Board, Values, NValues) :-
        value_position(X, Y, Board, Value), !,
        append([Value], Values, NValues).
value_pos(_, _, _, Values, Values).


% coord_right(+X, +Y, -Value)
coord_right(X, Y, Values, NValues) :-
        X1 is X + 1,
        append([(X1, Y)], Values, NValues).

% coord_left(+X, +Y, -Value)
coord_left(X, Y, Values, NValues) :-
        X1 is X - 1,
        append([(X1, Y)], Values, NValues).

% coord_top(+X, +Y, -Value)
coord_top(X, Y, Values, NValues) :-
        Y1 is Y - 1,
        append([(X, Y1)], Values, NValues).

% coord_down(+X, +Y, -Value)
coord_down(X, Y, Values, NValues) :-
        Y1 is Y + 1, 
        append([(X, Y1)], Values, NValues).

% coord_bottom_right(+X, +Y, -Value)
coord_bottom_right(X, Y, Values, NValues) :-
        Y1 is Y + 1,
        X1 is X + 1,
        append([(X1, Y1)], Values, NValues).

% coord_bottom_left(+X, +Y, -Value)
coord_bottom_left(X, Y, Values, NValues) :-
        Y1 is Y + 1,
        X1 is X - 1,
        append([(X1, Y1)], Values, NValues).

% coord_top_right(+X, +Y, -Value)
coord_top_right(X, Y, Values, NValues) :-
        Y1 is Y - 1,
        X1 is X + 1,
        append([(X1, Y1)], Values, NValues).

% coord_top_left(+X, +Y, -Value)
coord_top_left(X, Y, Values, NValues) :-
        Y1 is Y - 1,
        X1 is X - 1,
        append([(X1, Y1)], Values, NValues).


% coord_values(+Board, +Coords, -Values) 
coord_values(_, [], []).
coord_values(Board, [(X, Y)|Coords], Values) :-
        !, coord_values(Board, Coords, Vals),
        value_pos(X, Y, Board, Vals, Values).


% adjacent_coords(+X, +Y, -Values) 
adjacent_coords(X, Y, Values) :-
        coord_right(X, Y, [], V1),
        coord_left(X, Y, V1, V2),
        coord_top(X, Y, V2, V3),
        coord_down(X, Y, V3, Values).

% diagonal_coords(+X, +Y, -Values)
diagonal_coords(X, Y, Values) :-
        coord_bottom_right(X, Y, [], V1),
        coord_bottom_left(X, Y, V1, V2),
        coord_top_right(X, Y, V2, V3),
        coord_top_left(X, Y, V3, Values).

% square_coords(+X, +Y, -Values)
square_coords(X, Y, Values) :-
        coord_right(X, Y, [(X, Y)], V1),
        coord_down(X, Y, V1, V2),
        coord_bottom_right(X, Y, V2, Values).


% adjacent_values(+X, +Y, +Board, -Values) 
adjacent_values(X, Y, Board, Values) :-
        adjacent_coords(X, Y, Coords),
        coord_values(Board, Coords, Values).

% square_value(+X, +Y, +Board, -Values)
square_value(X, Y, Board, Values) :-
        square_coords(X, Y, Coords),
        coord_values(Board, Coords, Values).


% adjacent_trees_values(+X, +Y, +X1, +Y1, +Board, -Values) 
adjacent_trees_values(X, Y, X1, Y1, Board, Values) :-
        adjacent_coords(X, Y, Coords1),
        adjacent_coords(X1, Y1, Coords2),
        append(Coords1, Coords2, Coords),
        remove_dups(Coords, CoordsRes),
        coord_values(Board, CoordsRes, Values).


% get_arr_board(+Board, -Res)
get_arr_board([], []).
get_arr_board([H|T], Res) :-
        get_arr_board(T, Res2),
        append(H, Res2, Res).



% ================================
% ========= RESTRICTIONS =========
% ================================

% abs_coord_diff(+X, +Y, +X1, +Y1, -Res)
abs_coord_diff(X, Y, X1, Y1, Res) :-
        Res is abs(X - X1) + abs(Y - Y1).

% sum_adj_trees(+X, +Y, +Board, +Trees)
sum_adj_trees(_, _, _, []).

sum_adj_trees(X, Y, Board, [(X1, Y1)|Trees]) :-
        abs_coord_diff(X, Y, X1, Y1, Res),
        Res = 2, !,
        adjacent_trees_values(X, Y, X1, Y1, Board, Values),
        sum(Values, #>, 1), !,
        sum_adj_trees(X, Y, Board, Trees).

sum_adj_trees(X, Y, Board, [_|Trees]) :-
        !, sum_adj_trees(X, Y, Board, Trees).
        
        
% sum_trees(+Trees, +Board, +ConstTrees)
sum_trees([], _, _):- !.
sum_trees( [(X, Y) | Trees], Board, ConstTrees ) :-
        adjacent_values(X, Y, Board, Values),
        sum( Values, #>, 0 ),
        sum_adj_trees(X, Y, Board, ConstTrees),
        !, sum_trees( Trees, Board, ConstTrees ).


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
        Y >= NLines.
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



% ===============================
% ============ SOLVE ============
% ===============================

% domain_board(+Board, +Min, +Max)
domain_board(Board, Min, Max) :-
        get_arr_board(Board, Res), !,
        domain(Res, Min, Max).


% labeling_board(+Board)
labeling_board(Board) :-
        get_arr_board(Board, Res), !,
        labeling([], Res).


% solve_problem(+Board, +Trees, +Vals_Cls, +Vals_Lns)
solve_problem(Board, Trees, Vals_Cls, Vals_Lns) :-
        domain_board(Board, 0, 1),
        
        sum_board(Board, Trees),
        sum_trees(Trees, Board, Trees),
        sum_lines(Board, Vals_Lns),
        sum_cols(Board, Vals_Cls),
        sum_square(Board), !,
        
        labeling_board(Board).



% ===============================
% ============ TENTS ============
% ===============================

% tents
tents:-
        board_info(NCols, NLines, Trees, Vals_Cls, Vals_Lns),
        init_board(NCols, NLines, Trees, Board),
        % Display unsolved puzzle
        display_board(Board, Trees, Vals_Cls, Vals_Lns), !,

        solve_problem(Board, Trees, Vals_Cls, Vals_Lns),
        % Display solved puzzle
        display_board(Board, Trees, Vals_Cls, Vals_Lns).

tents_auto:-
        generate_board(Board, Trees, Vals_Cls, Vals_Lns),
        % Display unsolved puzzle
        display_board(Board, Trees, Vals_Cls, Vals_Lns), !,

        solve_problem(Board, Trees, Vals_Cls, Vals_Lns),
        % Display solved puzzle
        display_board(Board, Trees, Vals_Cls, Vals_Lns).