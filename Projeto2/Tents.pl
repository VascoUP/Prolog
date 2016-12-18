:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- include('display.pl').

vals_cls([(1, 3), (6, 1)]).
vals_lns([(1, 2), (6, 2)]).
trees([(2, 1), (4, 2), (2, 3), (4, 4), (6, 4), (1, 5), (2, 5), (4, 6)]).

% Tree -  0, tent - 1, empty space - 0

% create_board(+NCols, +NLines, -Board)
create_board(NLines, NCols, Board) :-
        length(Board2, NLines),
        create_lines(NCols, Board2, Board).

% create_lines(+NCols, +Board, -BoardRes)
create_lines(_, [], []).
create_lines(NCols, [_|R], [N|T]) :-
        length(N, NCols),
        !, create_lines(NCols, R, T).


% add_trees(+TreeCoords, +Board, -BoardRes)
add_trees([], _).
add_trees([(X, Y)|Trees], Board) :-
        nth1(Y, Board, Line),
        nth1(X, Line, 0), !,
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
        nth1(Line, Board, L),
        nth1(Column, L, 0), !,
        fill_impossible_spaces(Board, TreeCoords, Column2, Line).

fill_impossible_spaces(Board, TreeCoords, Column, Line) :-        
        Column1 is Column + 1, !,
        fill_impossible_spaces(Board, TreeCoords, Column1, Line).
        

% fill_impossible_spaces(+Board, +TreeCoords)
fill_impossible_spaces(Board, TreeCoords) :-
        fill_impossible_spaces(Board, TreeCoords, 1, 1).
        

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

% all_values_around(+X, +Y, +Board, -Values) 
all_values_around(X, Y, Board, Values) :-
        adjancent_values(X, Y, Board, NValues),
        value_right_top(X, Y, Board, NValues, NValues1),
        value_left_top(X, Y, Board, NValues1, NValues2),
        value_right_down(X, Y, Board, NValues2, NValues3),
        value_left_down(X, Y, Board, NValues3, Values), !.


% sum_trees( +Trees, +Board )
sum_trees([], _):- !.
sum_trees( [(X, Y) | Trees], Board ) :-
        adjancent_values(X, Y, Board, Values),
        sum( Values, #>, 0 ),
        !, sum_trees( Trees, Board ).


% sum_line( +Board, +Value, +Line )
sum_line( Board, Value, Line ) :-
        nth1( Line, Board, H ),
        sum(H, #=, Value).

% sum_lines( +Board, +Vals_Line )
sum_lines( _, [] ).
sum_lines( Board, [(Line, Value) | T] ) :-
       sum_line( Board, Value, Line ),
       !, sum_lines( Board, T ).


% sum_col( +Board, +Value, +Column )
sum_col( Board, Value, Column ) :-
        findall(Col, (
                        length(Board, Len), domain([NumLine], 1, Len), 
                        nth1(NumLine, Board, Line), nth1(Column, Line, Col)), 
                Vals),
        sum(Vals, #=, Value).

% sum_cols( +Board, +Vals_Cols )
sum_cols( _, [] ).
sum_cols( Board, [(Column, Value) | T] ) :-
       sum_col( Board, Value, Column ),
       !, sum_cols( Board, T ).


% get_square( +Board, +X, +Y, -Square )
get_square( Board, X, Y, Square ) :-
        length( Board, LenY ),
        nth1( 1, Board, L ),
        length( L, LenX ), !,
        
        LenY >= Y + 1,
        LenX >= X + 1,
        
        % X and Y
        nth1( Y, Board, H1 ),
        nth1( X, H1, V1 ),
        
        % X+1 and Y
        X1 is X + 1,
        nth1( X1, H1, V2 ),
        
        % X and Y+1
        Y1 is Y + 1,
        nth1( Y1, Board, H2 ),
        nth1( X, H2, V3 ),
        
        % X+1 and Y+1
        nth1( X1, H2, V4 ), !,
        
        append( [V1, V2, V3, V4], [], Square ), write(Square).


% sum_squares_cols( +Board, +X, +Y )
sum_squares_cols( Board, X, Y ) :-
        get_square( Board, X, Y, Square ), !,
        X1 is X + 1,
        sum(Square, #<, 2), !,
        sum_squares_cols( Board, X1, Y ).

% sum_squares( +Board, +X, +Y )
sum_squares( Board, X, Y ) :-
        sum_squares_cols( Board, X, Y ), !,
        Y1 is Y + 1, !,
        sum_squares( Board, X, Y1 ).
sum_squares( _, _, _ ).

% sum_squares( +Board )
sum_squares( Board ) :-
        !, sum_squares( Board, 1, 1 ).


% domain_board( +Board, +Min, +Max )
domain_board([], _, _).
domain_board([H|T], Min, Max) :-
        domain(H, Min, Max), !,
        domain_board(T, Min, Max).

% labeling_board( +Board )
labeling_board([]).
labeling_board([H|T]) :-
        labeling([], H), !,
        labeling_board(T).


% tents
tents:-
        trees(Trees),
        vals_cls(Vals_Cols),
        vals_lns(Vals_Lines), !,
        
        create_board(6, 6, Board),
        add_trees(Trees, Board),
        fill_impossible_spaces(Board, Trees),
        
        display_board(Board, Trees),
        
        domain_board(Board, 0, 1),
        
        sum_trees(Trees, Board),
        sum_lines(Board, Vals_Lines),
        sum_cols(Board, Vals_Cols),
        sum_squares(Board),
        write("Oi"), nl,
        
        labeling_board(Board),
        
        display_board(Board, Trees).



% ---------------------
%       TUDO MAL
% ---------------------

% non_tree(-Arr, +NonTree)
non_tree([], []).
non_tree([Val | Line], NonTree) :-
        Val \= 3, 
        Val = 2, !,
        non_tree(Line, NonTree).
non_tree([Val | Line], [Val | NonTree]) :-
        !, non_tree(Line, NonTree).

non_tree_board([], []).
non_tree_board([Line | Board], NonTree) :-
        non_tree(Line, Un2),
        non_tree_board(Board, Un), !,
        append(Un2, Un, NonTree).


% arr_line(-Line, +UndefinedVal)
arr_line([], []).
arr_line([Val | Line], UndefinedVals) :-
        Val \= 1, !,    % check wether the term is instanciated
        arr_line(Line, UndefinedVals).
arr_line([Val | Line], UndefinedVals) :-
        Val \= 0, !, % check wether the term is instanciated
        arr_line(Line, UndefinedVals).
arr_line([Val | Line], [Val | UndefinedVals]) :-
        !, arr_line(Line, UndefinedVals).
        
% arr_board(-Board, +UndefinedVals)
arr_board([], []).
arr_board([Line | Board], UndefinedVals) :-
        arr_line(Line, Un2),
        arr_board(Board, Un), !,
        append(Un2, Un, UndefinedVals).
   

% sum_elem_line(-Board, -Y, -X)
sum_trees_line(Board, Y, X) :-
        nth1(Y, Board, Line),
        length(Line, Len),
        Len < X, !.
sum_trees_line(Board, Y, X) :-
        value_position(X, Y, Board, Value), 
        Value = 2,
        
        adjancent_values(X, Y, Board, Values),
        arr_line(Values, UninstanciatedValues),
        sum_values(UninstanciatedValues, #>, 0),
                
        X1 is X + 1,
        sum_trees_line(Board, Y, X1).
sum_trees_line(Board, Y, X) :- 
        X1 is X + 1,
        sum_trees_line(Board, Y, X1).

% sum_trees_line(-Board, -Y)
sum_trees_line(Board, Y) :-
        sum_trees_line(Board, Y, 1).

% sum_trees_board(-Board, -Y)
sum_trees_board(Board, Y) :-
        length(Board, Len),
        Len < Y, !.
sum_trees_board(Board, Y) :-
        sum_trees_line(Board, Y),
        Y1 is Y + 1,
        sum_trees_board(Board, Y1).

% sum_trees_board(-Board)
sum_trees_board(Board) :-
        sum_trees_board(Board, 1).
         
sum_values([], _, _).
sum_values(Values, Op, Res) :-
        sum(Values, Op, Res).

% sum_elem_line(-Board, -Y, -X)
sum_adj_tents_line(Board, Y, X) :-
        nth1(Y, Board, Line),
        length(Line, Len),
        Len < X, !.
sum_adj_tents_line(Board, Y, X) :-
        element_position(X, Y, Board, Value), 
        Value #= 1, !,
                
        all_values_around(X, Y, Board, Values),
        arr_line(Values, UninstanciatedValues),
        sum_values(UninstanciatedValues, #=, 0),
        
        X1 is X + 1, !,
        sum_adj_tents_line(Board, Y, X1).
sum_adj_tents_line(Board, Y, X) :- 
        X1 is X + 1, !,
        sum_adj_tents_line(Board, Y, X1).

% sum_trees_line(-Board, -Y)
sum_adj_tents_line(Board, Y) :-
        sum_adj_tents_line(Board, Y, 1).

% sum_trees_board(-Board, -Y)
sum_adj_tents_board(Board, Y) :-
        length(Board, Len),
        Len < Y, !.
sum_adj_tents_board(Board, Y) :-
        sum_adj_tents_line(Board, Y),
        Y1 is Y + 1, !,
        sum_adj_tents_board(Board, Y1).

% sum_trees_board(-Board)
sum_adj_tents_board(Board) :-
        sum_adj_tents_board(Board, 1).


% verify_vals_cols(-Board, -Vals_Cols)
verify_vals_cols(_, []).
verify_vals_cols(Board, [(N, Val)|T]) :-
        findall(Col, (length(Board, Len), domain([NumLine], 1, Len), nth1(NumLine, Board, Line), nth1(N, Line, Col)), Column),
        non_tree(Column, NonTree),
        sum(NonTree, #=, Val), !,
        verify_vals_cols(Board, T).

% verify_vals_lns(-Board, -Vals_Lns)
verify_vals_lns(_, []).
verify_vals_lns(Board, [(N, Val)|T]) :-
        nth1(N, Board, Line),
        non_tree(Line, NonTree),
        sum(NonTree, #=, Val), !,
        verify_vals_cols(Board, T).
