% display_sep_line(+N)
display_sep_line(0):-write('-'), nl.
display_sep_line(N):-
        write('----'),
        
        N1 is N-1,
        display_sep_line(N1), !.


% display_vals_cols(+Vals_Cls, +N, +Column)

display_vals_cols(_, N, Column):-
        Column > N.
        
display_vals_cols(Vals_Cls, N, Column):-
        \+ nth1(_, Vals_Cls, (Column, _)),

        write('    '),
        
        Column1 is Column + 1,
        !, display_vals_cols(Vals_Cls, N, Column1).

display_vals_cols(Vals_Cls, N, Column):-
        nth1(_, Vals_Cls, (Column, Val)),
        
        Val < 10,
        write('  '), write(Val), write(' '),
        
        Column1 is Column + 1,
        !, display_vals_cols(Vals_Cls, N, Column1).

display_vals_cols(Vals_Cls, N, Column):-
        nth1(_, Vals_Cls, (Column, Val)),
        
        Val < 100,
        write(' '), write(Val), write(' '),
        
        Column1 is Column + 1,
        !, display_vals_cols(Vals_Cls, N, Column1).

display_vals_cols(Vals_Cls, N, Column):-
        nth1(_, Vals_Cls, (Column, Val)),

        write(' '), write(Val),
        
        Column1 is Column + 1,
        !, display_vals_cols(Vals_Cls, N, Column1).


% display_board(+Board, +TreeCoords, +Column, +Line)
display_board(Board, TreeCoords, Vals_Lns, Column, Line) :-
        nth1(1, Board, L), length(L, N),
        Column > N,
        
        nth1(_, Vals_Lns, (Line, Val)), 
        
        write('|'), write(' '), write(Val), nl,
        display_sep_line(N),
        
        Line1 is Line + 1, !,        
        display_board(Board, TreeCoords, Vals_Lns, 1, Line1).

display_board(Board, TreeCoords, Vals_Lns, Column, Line) :-
        nth1(1, Board, L), length(L, N),
        Column > N,
        
        write('|'), nl,
        display_sep_line(N),
        
        Line1 is Line + 1, !,        
        display_board(Board, TreeCoords, Vals_Lns, 1, Line1).

display_board(Board, _, _, _, Line) :-
        length(Board, N),
        Line > N, !.

display_board(Board, TreeCoords, Vals_Lns, Column, Line) :-
        nth1(_, TreeCoords, (Column, Line)), % if this position has a tree in it then we should move 2 position forward

        write('| @ '),
        
        Column1 is Column + 1, !,
        display_board(Board, TreeCoords, Vals_Lns, Column1, Line).

display_board(Board, TreeCoords, Vals_Lns, Column, Line) :-
        nth1(Line, Board, L),
        nth1(Column, L, Value),
        Value \= 1, !,
        
        write('|   '),
        
        Column1 is Column + 1, !,
        display_board(Board, TreeCoords, Vals_Lns, Column1, Line).

display_board(Board, TreeCoords, Vals_Lns, Column, Line) :-
        nth1(Line, Board, L),
        nth1(Column, L, Value),
        Value \= 0, !,
        
        write('| T '),
        
        Column1 is Column + 1, !,
        display_board(Board, TreeCoords, Vals_Lns, Column1, Line).

display_board(Board, TreeCoords, Vals_Lns, Column, Line) :-
        write('| ? '),
        
        Column1 is Column + 1, !,
        display_board(Board, TreeCoords, Vals_Lns, Column1, Line).


% display_board(+Board, +TreeCoords, +Vals_Cls, +Vals_Lns)
display_board(Board, TreeCoords, Vals_Cls, Vals_Lns) :-
       write('-->TENTS<--'), nl, nl,
       
       nth1(1, Board, L), length(L, N),
       display_sep_line(N),
       
       display_board(Board, TreeCoords, Vals_Lns, 1, 1), 
       
       display_vals_cols(Vals_Cls, N, 1), nl, nl. 