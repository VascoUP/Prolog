% display_sep_line(+N)
display_sep_line(0):-write('-'), nl.
display_sep_line(N):-
        write('----'),
        
        N1 is N-1,
        display_sep_line(N1), !.

% display_board(+Board, +TreeCoords, +Column, +Line)
display_board(Board, TreeCoords, Column, Line) :-
        nth1(1, Board, L), length(L, N),
        Column > N,
        
        write('|'), nl,
        display_sep_line(N),
        
        Line1 is Line + 1, !,        
        display_board(Board, TreeCoords, 1, Line1).
display_board(Board, _, _, Line) :-
        length(Board, N),
        Line > N, !.

display_board(Board, TreeCoords, Column, Line) :-
        nth1(_, TreeCoords, (Column, Line)), % if this position has a tree in it then we should move 2 position forward

        write('| @ '),
        
        Column1 is Column + 1, !,
        display_board(Board, TreeCoords, Column1, Line).

display_board(Board, TreeCoords, Column, Line) :-
        nth1(Line, Board, L),
        nth1(Column, L, Value),
        Value \= 1, !,
        
        write('|   '),
        
        Column1 is Column + 1, !,
        display_board(Board, TreeCoords, Column1, Line).

display_board(Board, TreeCoords, Column, Line) :-
        nth1(Line, Board, L),
        nth1(Column, L, Value),
        Value \= 0, !,
        
        write('| T '),
        
        Column1 is Column + 1, !,
        display_board(Board, TreeCoords, Column1, Line).

display_board(Board, TreeCoords, Column, Line) :-
        write('| ? '),
        
        Column1 is Column + 1, !,
        display_board(Board, TreeCoords, Column1, Line).

        
% display_board(+Board, +TreeCoords)
display_board(Board, TreeCoords) :-
       write('-->TENTS<--'), nl, nl,
       
       nth1(1, Board, L), length(L, N),
       display_sep_line(N),
       
       display_board(Board, TreeCoords, 1, 1), nl, nl. 