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
/* MAPA DO DOKONCZENIA */


/* Objects in rooms definition */
there_is(your_bed, cell2).
there_is(old_mans_bed, cell2).
there_is(toilet, cell2).
there_is(desk, guard_room).
/* WYKONCZENIE POKOI DO DOKONCZENIA */

/* Items locations (in objects) definition */
there_is(poop, toilet).
there_is(coin, toilet).

at(cell2).

/* These rule(s) tell what is going on around you*/

look :-
    at(Place),
    nl, write("You're currently at "), write(Place), nl,
    nl, write("You can see: "), nl,
    list_objects(Place),
    nl, write("You can go to:"), nl,
    available_destinations(Place), nl.

list_objects(Place) :-
    there_is(Object, Place),
    write("* "), write(Object), nl,
    fail.

list_objects(_).

available_destinations(Place) :-
    borders(Place, Destination),
    write("-> "), write(Destination), nl,
    fail.

available_destinations(_).

/* These rule(s) tell(s) how to move to different locations*/

go(Destination) :-
    at(Place),
    borders(Place, Destination),
    retract(at(Place)),
    assert(at(Destination)),
    !, look.

go(_) :-
    write("You can't go there!").

/* These rule(s) tell what you can find in object*/

investigate(Object) :-
    at(Place),
    there_is(Object, Place),
    empty(Object),
    nl, write(Object), write(" is empty."), nl,
    !, nl.

investigate(Object) :-
    at(Place),
    there_is(Object, Place),
    nl, write("In "), write(Object), write(" you can find:"), nl,
    fail.

investigate(Object) :-
    at(Place),
    there_is(Object, Place),
    there_is(Item, Object),
    write("* "), write(Item), nl,
    fail.

investigate(Object) :-
    at(Place),
    there_is(Object, Place),
    !, nl.

investigate(Object) :-
    nl, write("Cannot investigate "), write(Object), nl.

there_is(hammer, bed).

empty(Object) :- 
    \+ there_is(_, Object).

/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write("You're already holding it!"),
        !, nl.

take(X) :-
        i_am_at(Place),
        at(X, Place),
        retract(at(X, Place)),
        assert(holding(X)),
        write("OK."),
        !, nl.

take(_) :-
        write("I don't see it here."),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write("You aren't holding it!"),
        nl.

commands :-
    nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                -- to start the game.'), nl,
        write('go(Destination).      -- to go to selected destination.'), nl,
        write('look.                 -- to look around you again.'), nl,
        write('investigate(Object)   -- to see if there is any item in object'), nl,
        write('take(Item).           -- to pick up an item.'), nl,
        write('drop(Item).           -- to put down an item.'), nl,
        write('commands.             -- to see this message again.'), nl,
        write('halt.                 -- to end the game and quit.'), nl,
        nl.

start :-
    nl, write("Welcome to Andrew Tate Prison Escape"), nl,
    commands,
    look.