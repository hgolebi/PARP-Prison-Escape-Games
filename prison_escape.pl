/* Andrew Tate Prison Escape, by Hubert Gołębiowski and Jakub Rozkosz. */

:- dynamic at/1, there_is/2, holding/1, quest_done/2, cigarettes/1.
:- retractall(at(_)), retractall(there_is(_,_)), holding(_), retractall(cigarettes(_)), retractall(quest_done(_,_)).


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

/* People in rooms definition */
there_is(old_man, cell2).
there_is(gym_guy, gym).
there_is(guard, guard_room).
there_is(sleeping_guy, cell1).
there_is(chef, kitchen).
there_is(showering_prisoner, shower_room).
/* WYKONCZENIE POKOI DO DOKONCZENIA */

/* Items locations (in objects) definition */
there_is(poop, toilet).
there_is(coin, toilet).
there_is(cigarettes, your_bed).

/* Starting in cell2 */
at(cell2).

/* Initialize the number of cigarettes */
cigarettes(0).

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

take(Item) :-
    holding(Item),
    write("You're already holding it!"),
    !, nl.

take(cigarette) :-
    at(Place),
    there_is(Object, Place),
    there_is(cigarette, Object),
    retract(there_is(cigarette, Object)),
    assert(holding(cigarette)),
    increase_cigarettes(1),
    write("OK."),
    !, nl.

take(Item) :-
    at(Place),
    there_is(Object, Place),
    there_is(Item, Object),
    retract(there_is(Item, Object)),
    assert(holding(Item)),
    write("OK."),
    !, nl.

take(_) :-
    write("I don't see it here."),
    nl.


/* These rules describe how to put down an object. */

drop(Item) :-
        holding(Item),
        at(Place),
        there_is(Object, Place),
        retract(holding(Item)),
        assert(there_is(Item, Object)),
        write('OK.'),
        !, nl.

drop(_) :-
        write("You aren't holding it!"),
        nl.


/* These rules describe how to talk to a person. */

talk(Person) :-
    at(Place),
    there_is(Person, Place),
    dialogue(Person),
    !, nl.

talk(Person) :-
    nl, write("There is no one named "), write(Person), write(" here."), nl.

dialogue(old_man) :-
    (\+ quest_done(quest1, old_man)),
    write("You: Psst... I was thinking about escape. Are you in?"), nl,
    write("Old Man: Escape, huh? It won't be easy. I've been here for years and I'm too old for this."), nl,
    write("You: Damn... But you probably know this prison quite well. Do you have any advice?"), nl,
    write("Old Man: Yes, but it will cost. Please bring me 5 cigarettes and we will talk..."), nl.

dialogue(old_man) :-
    quest_done(quest1, old_man),
    (\+ quest_done(quest2, old_man)),
    write("You: Okey, could you give me some advice now?"), nl,
    write("Old Man: Alright. There is a hole in the wall by the 16th lampost on a prison yard."), nl,
    write("You: But wait, the lights are on, everything will be visible."), nl,
    write("Old Man: I don't give free information. Bring 5 more cigarettes."), nl.

dialogue(old_man) :-
    quest_done(quest2, old_man), nl,
    write("You: What about the light?"), nl,
    write("Old Man: You can break the ventilation hole in the hallway and get into the room with fuses, where you turn off the light."), nl,
    write("You: Holy Chicken Trolley,  that's my opportunity!!"), nl.


/* trzeba jakos zrobic, ze te dialogi sie zmieniaja po wykonaniu questow - 
wiec chyba dodac nowe dialogi z old manem, tylko dodac na poczatku sprawdzenie 
czy wykonal poprzedni quest/jest na odpowiednim poziomie gry */

/* DODAC ROZMOWY Z INNYMI POSTACIAMI */

/* These rules describe how to give an item to a person */

give(Item, old_man) :-
    at(Place),
    there_is(old_man, Place),
    holding(Item),
    Item = cigarettes,
    cigarettes(Count),
    Count >= 5,
    NewCount is Count - 5,
    retract(holding(Item)),
    assert(cigarettes(NewCount)),
    (   (\+ quest_done(quest1, old_man))
    ->  assert(quest_done(quest1, old_man))
    ;   assert(quest_done(quest2, old_man))
    ),
    write("Old Man: Ah, you've brought the cigarettes. Good."), nl,
    write("You hand the cigarettes to the Old Man."), nl.

give(Item, old_man) :-
    at(Place),
    there_is(old_man, Place),
    holding(Item),
    Item = cigarettes,
    cigarettes(Count),
    Count < 5,
    write("Old Man: You don't have enough cigarettes."), nl.

give(Item, old_man) :-
    at(Place),
    there_is(old_man, Place),
    holding(Item),
    Item \= cigarettes,
    write("Old Man: I don't want that item."), nl.

give(_, _) :-
    nl, write("This person doesn't want that item."), nl.

/* DODAC WRĘCZANIE PRZEDMIOTÓW INNYM POSTACIOM */


/* This rule describes how the number of picked up cigarettes increases */

increase_cigarettes(N) :-
    retract(cigarettes(Count)),
    NewCount is Count + N,
    assert(cigarettes(NewCount)).



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
        write('give(Item, Person).   -- to give a person the item they wanted'), nl,
        write('talk(Person).         -- to talk to a person'), nl,
        write('commands.             -- to see this message again.'), nl,
        write('halt.                 -- to end the game and quit.'), nl,
        nl.

start :-
    nl, write("Welcome to Andrew Tate Prison Escape"), nl,
    commands,
    look.