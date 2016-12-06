
display_line([]) :- write('|'), nl.
display_line([H|T]) :-
        H = 0, write('|@'),
        !, display_line(T).
display_line([_|T]) :-
        write('| '),
        !, display_line(T).

display_board([]) :-
        nl.
display_board([H|T]) :-
        nl,
        display_line(H),
        !, display_board(T).