/* Andrew Tate Prison Escape, by Hubert Gołębiowski and Jakub Rozkosz. */

:- dynamic at/1, there_is/2, holding/1.
:- retractall(at(_)), retractall(there_is(_,_)), holding(_).

/* Map definition */
borders(cell1, hallway).
borders(cell2, hallway).
borders(cell3, hallway).
borders(hallway, cell1).
borders(hallway, cell2).
borders(hallway, cell3).
borders(hallway, guard_room).
borders(guard_room, hallway).
borders(guard_room, kitchen).
borders(guard_room, shower_room).
borders(guard_room, gym).
borders(kitchen, guard_room).
borders(shower_room, guard_room).
borders(gym, guard_room).   

at(cell2).

look :-
    at(Place),
    nl, write("You're currently at "), write(Place), nl, nl,
    write("You can go to:"), nl,
    available_destinations(Place), nl.

available_destinations(Place) :-
    borders(Place, Destination),
    write("-> "), write(Destination), nl,
    fail.

available_destinations(_).

go(Destination) :-
    at(Place),
    borders(Place, Destination),
    retract(at(Place)),
    assert(at(Destination)),
    !, look.

go(_) :-
    write("You can,t go there!").

start :-
    nl, write("Welcome to Andrew Tate Prison Escape"), nl, nl,
    look.