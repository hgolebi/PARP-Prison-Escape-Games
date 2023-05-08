/* Andrew Tate Prison Escape, by Hubert Gołębiowski and Jakub Rozkosz. */

:- dynamic at/1, there_is/2, holding/1, quest_done/2, cigarettes/1, locked/1, distracted/1.
:- retractall(at(_)), retractall(there_is(_,_)), retractall(holding(_)), retractall(cigarettes(_)), retractall(quest_done(_,_)), locked(_).


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
borders(ventilation, hallway).
borders(ventilation, shed).
borders(shed, outside).

locked(cell1).
locked(hallway).
locked(ventilation).

/* Objects in rooms definition */
there_is(occupied_bed, cell1).
there_is(small_toilet, cell1).
there_is(teapot, cell1).
there_is(occupied_bed2, cell1).
there_is(table, cell2).
there_is(your_bed, cell2).
there_is(old_mans_bed, cell2).
there_is(toilet, cell2).
there_is(bunk_bed, cell3).
there_is(bed_cabinet, cell3).
there_is(shelf, cell3).
there_is(ventilation_grid, hallway).
there_is(desk, guard_room).
there_is(tv, guard_room).
there_is(coat, guard_room).
there_is(chair, guard_room).
there_is(oven, kitchen).
there_is(chair, kitchen).
there_is(corner, kitchen).
there_is(fridge, kitchen).
there_is(sink, kitchen).
there_is(shower, shower_room).
there_is(shower2, shower_room).
there_is(shower3, shower_room).
there_is(shower4, shower_room).
there_is(cabinet, shower_room).
there_is(treadmill, gym).
there_is(treadmill2, gym).
there_is(bench, gym).
there_is(fuse_box, shed).
there_is(pole1, outside).
there_is(pole2, outside).
there_is(pole3, outside).
there_is(pole4, outside).
there_is(pole5, outside).
there_is(pole6, outside).
there_is(pole7, outside).
there_is(pole8, outside).
there_is(pole9, outside).
there_is(pole10, outside).
there_is(pole11, outside).
there_is(pole12, outside).
there_is(pole13, outside).
there_is(pole14, outside).
there_is(pole15, outside).
there_is(pole16, outside).
there_is(pole17, outside).
there_is(pole18, outside).
there_is(pole19, outside).
there_is(pole20, outside).
there_is(pole21, outside).


/* People in rooms definition */
there_is(old_man, cell2).
there_is(gym_guy, gym).
there_is(guard, guard_room).
there_is(sleeping_guy, cell1).
there_is(sleeping_guy2, cell1).
there_is(chef, kitchen).
there_is(showering_prisoner, shower_room).

/* Items locations (in objects) definition */
there_is(poop, toilet).
there_is(coin, toilet).
there_is(cigarette, your_bed).
there_is(cigarette, corner).
there_is(cigarette, bed_cabinet).
there_is(cigarette, sleeping_guy).
there_is(cigarette, occupied_bed2).
there_is(cigarette, coat).
there_is(cigarette, desk).
there_is(cigarette, fridge).
there_is(cigarette, treadmill).
there_is(cigarette, cabinet).
there_is(cigarette, bench).
there_is(playboy_magazine, shelf).
there_is(flashligth, desk).
there_is(cell1_key, guard).
there_is(towel, bench).
there_is(batteries, pillow).

/* Starting in cell2 */
at(cell2).

/* Initialize the number of cigarettes */
cigarettes(1).

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
go(guard_room) :-
    at(Place),
    borders(Place, guard_room),
    \+ distracted(guard),
    nl, write("Oh no, there is a guard!"), nl,
    write("I probably should've ditracted him first."), nl,
    game_over,
    !, nl.

go(Destination) :-
    at(Place),
    borders(Place, Destination),
    \+ locked(Destination),
    retract(at(Place)),
    assert(at(Destination)),
    !, look.
    
go(Destination) :-
    at(Place),
    borders(Place, Destination),
    locked(Destination),
    nl, write("This place is locked."), nl,
    !, nl.

go(_) :-
    write("You can't go there!").

debuggo(Place) :-
    retract(at(_)),
    assert(at(Place)).

/* This rules tell how to unclock room */
unlock(Room) :-
    at(Place),
    borders(Place, Room),
    \+ locked(Room),
    nl, write("This place is already unlocked."), nl,
    !, nl.

unlock(cell1) :-
    at(hallway),
    locked(cell1),
    holding(cell1_key),
    retract(locked(cell1)),
    nl, write("Used cell1_key to unlock cell1."), nl,
    !, nl.

unlock(cell1) :-
    at(hallway),
    locked(cell1),
    \+ holding(cell1_key),
    nl, write("I need a key to unlock this cell."), nl,
    !, nl.

unlock(hallway) :-
    at(cell2),
    locked(hallway),
    holding(cell2_key),
    retract(locked(hallway)),
    nl, write("Used cell2_key to unlock cell2."), nl,
    !, nl.

unlock(hallway) :-
    at(cell2),
    locked(hallway),
    \+ holding(cell2_key),
    nl, write("I need a key to unlock my cell."), nl,
    !, nl.

unlock(ventilation) :-
    at(hallway),
    locked(ventilation),
    nl, write("Im too weak to unlock this. Maybe someone can help me."), nl,
    !, nl.

unlock(Room) :-
    nl, write("Cannot unlock "), write(Room), nl.

/* For debugging purpous only */
unlockall :-
    retractall(locked(_)).

/* These rule(s) tell what you can find in object*/

investigate(old_man) :-
    at(cell2),
    nl, write("Old Man: You filthy rat, keep your hands close!!"), nl,
    !, nl.


investigate(Object) :-
    at(Place),
    there_is(Object, Place),
    empty(Object),
    nl, write("Nothing to be found here."), nl,
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

empty(Object) :- 
    \+ there_is(_, Object).

/* These rules describe how to pick up an object. */

take(towel, Object) :-
    at(gym),
    there_is(towel, Obejct),
    there_is(gym_guy, gym),
    nl, write("Gym Guy: Hey what do you think you're doing?! That's my towel!"), nl,
    write("You: Can I borrow it?"), nl,
    write("Gym Guy: Forget it. I need it for my workout."), nl,
    !, nl.

take(towel, Object) :-
    at(gym),
    there_is(towel, Object),
    \+ there_is(gym_guy, gym),
    retract(there_is(towel, Object)),
    assert(holding(towel)),
    !, nl.

take(Item, Object) :-
    holding(Item),
    write("You're already holding it!"),
    !, nl.

take(cigarette, Object) :-
    at(Place),
    there_is(Object, Place),
    there_is(cigarette, Object),
    retract(there_is(cigarette, Object)),
    assert(holding(cigarette)),
    increase_cigarettes(1),
    write("OK."),
    !, nl.

take(Item, Object) :-
    at(Place),
    there_is(Object, Place),
    there_is(Item, Object),
    retract(there_is(Item, Object)),
    assert(holding(Item)),
    write("OK."),
    !, nl.

take(_, _) :-
    write("I don't see it here."),
    nl.


/* These rules describe how to put down an object. */

drop(Item, Object) :-
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
    assert(holding(cell2_key)),
    write("You: Psst... I was thinking about escape. Are you in?"), nl,
    write("Old Man: Escape, huh? It won't be easy. I've been here for years and I'm too old for this."), nl,
    write("You: Damn... But you probably know this prison quite well. Do you have any advice?"), nl,
    write("Old Man: Yes, but it will cost. Please bring me 5 cigarettes and we will talk."), nl,
    write("You: I don't have that much.."), nl,
    write("Old Man: Here, take that key. Maybe you will find some outside."), nl,
    write("You: Wait, you had a key to our cell all this time?!"), nl,
    write("Old Man: Maybe I had, maybe I didn't. That's not important now. Just take the key and find me some ciggaretes."), nl,
    nl, write("You received cell2_key."), nl.


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
    write("You: Holy Chicken Trolley, that's my opportunity!!"), nl,
    assert(borders(hallway, ventilation)).

dialogue(gym_guy) :-
    (\+ quest_done(meal_quest, gym_guy)),
    write("You see a strong guy that is exhausted after his training."), nl,
    write("You: Hey! I have a case. Could I do something for you in return for a small favor?"), nl,
    write("Gym Guy: You little man, what would you need help for?"), nl,
    write("You: To break the ventilation hole."), nl,
    write("Gym Guy: It's a piece of cake for me. Bring me a great meal cause I need to refill my carbs. Then I'll do the job."), nl.

dialogue(gym_guy) :-
    quest_done(meal_quest, gym_guy),
    write("You: So, will you help me with yout muscles?"), nl,
    write("Gym Guy: Yeah, the meal was great. Take me to the place."), nl,
    write("You and the Gym Guy went to the ventilation grid and broke it."), nl,
    retract(at(gym)),
    assert(at(hallway)),
    retract(there_is(gym_guy, gym)),
    assert(there_is(gym_guy, hallway)),
    retract(there_is(ventilation_grid, hallway)),
    retract(locked(ventilation)).

dialogue(showering_prisoner) :-
    \+ quest_done(towel_quest, showering_prisoner),
    nl, write("Prisoner: Hey what are you looking at?!"), nl,
    write("You: I was just.."), nl,    
    write("Prisoner: Get out now!! Or actually, wait.. Bring me a towel!"), nl,
    write("You: Why would I?"), nl,
    write("Prisoner: You dare to ask?! Fine, if you decide to help me I'll give you something in return."), nl,   
    !, nl. 

dialogue(showering_prisoner) :-
    quest_done(towel_quest, showering_prisoner),
    nl, write("You: You received your towel. What about my reward?"), nl,
    write("Prisoner: Hmm... I don't have anything on me, but.."), nl,
    write("You: But what?!"), nl,
    write("Prisoners: Do you want some batteries?"), nl,
    write("You: Why would I need batteries?"), nl,
    write("Prisoner: I dunno, maybe to power up a flashlight or something.."), nl,
    write("You: Fine, give me those batteries!"), nl,
    write("Prisoner: You can find them under a pillow in cell 3."), nl,
    write("You: Oh, you don't have them on you, right.."), nl,
    assert(there_is(pillow, cell3)),
    !, nl.


dialogue(chef) :-
    (\+ quest_done(coffee_quest, chef)),
    write("You: Hi! I've heard that you're the best chef in here. Could you make me a signature meal?"), nl,
    write("Chef: Nice words won't be enough. I'am actually pretty tired, if you could bring me some coffee then I'll cook something."), nl,
    write("You: I should have some in my cell, I'll be in a moment."), nl,
    assert(there_is(coffee, table)).

dialogue(chef) :-
    quest_done(coffee_quest, chef),
    write("You: Now you're quite caffenaited, aren't you?"), nl,
    write("Chef: Yeah, thanks. I'll cook something quickly."), nl,
    write("After few minutes chef hands you a hot meal."), nl,
    write("You: Thanks a lot, bye."), nl,
    assert(holding(great_meal)).


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
    write("You hand the cigarettes to the Old Man."), nl,
    !, nl.

give(Item, old_man) :-
    at(Place),
    there_is(old_man, Place),
    holding(Item),
    Item = cigarettes,
    cigarettes(Count),
    Count < 5,
    write("Old Man: You don't have enough cigarettes."), nl,
    !, nl.

give(Item, old_man) :-
    at(Place),
    there_is(old_man, Place),
    holding(Item),
    Item \= cigarettes,
    write("Old Man: I don't want that item."), nl,
    !, nl.

give(Item, gym_guy) :-
    at(Place),
    there_is(gym_guy, Place),
    holding(Item),
    Item = great_meal,
    retract(holding(Item)),
    assert(quest_done(meal_quest, gym_guy)),
    write("Gym Guy: Just on time, give me that."), nl,
    write("You hand the meal to the Gym Guy."), nl,
    !, nl.

give(Item, gym_guy) :-
    at(Place),
    there_is(gym_guy, Place),
    holding(Item),
    Item \= great_meal,
    write("Gym Guy: I don't want that item."), nl,
    !, nl.

give(Item, gym_guy) :-
    at(Place),
    there_is(gym_guy, Place),
    \+ holding(Item),
    write("Gym Guy: You don't have the meal."), nl,
    !, nl.

give(Item, chef) :-
    at(Place),
    there_is(chef, Place),
    holding(Item),
    Item = coffee,
    retract(holding(Item)),
    assert(quest_done(coffee_quest, chef)),
    write("Gym Guy: Oh, you have the coffee. I need a boost of energy."), nl,
    write("You hand the cofffee to the Chef."), nl,
    !, nl.

give(Item, chef) :-
    at(Place),
    there_is(chef, Place),
    holding(Item),
    Item \= coffee,
    write("Chef: I don't want that item."), nl,
    !, nl.

give(Item, gym_guy) :-
    at(Place),
    there_is(chef, Place),
    \+ holding(Item),
    write("Chef: You don't have the coffee."), nl,
    !, nl.

give(_, _) :-
    nl, write("This person doesn't want that item."), nl.


/* This rule describes how the number of picked up cigarettes increases */

inventory :-
    nl, write("You have: "), nl,
    list_items,
    print_cigarettes, nl.

list_items :-
    holding(Item),
    write("* "), write(Item), nl,
    fail.

list_items.

print_cigarettes :-
    cigarettes(Count),
    write("* "), write(Count), write(" cigarettes"), nl.

increase_cigarettes(N) :-
    retract(cigarettes(Count)),
    NewCount is Count + N,
    assert(cigarettes(NewCount)).

restart :-
    make,
    start.

game_over :-
    retract(at(_)),
    retractall(holding(_)),
    assert(at(isolation_ward)),
    nl, write("YOU GOT CAUGHT!"), nl,
    nl, write("type restart. to start over again"), nl.

finish :-
    nl,
    write('The game is over. Please enter the "halt." command.'),
    nl.

commands :-
    nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.                -- to start the game.'), nl,
        write('go(Destination).      -- to go to selected destination.'), nl,
        write('unlock(Destination).  -- to unlock a room.'), nl,
        write('look.                 -- to look around you again.'), nl,
        write('investigate(Object).  -- to see if there is any item in object.'), nl,
        write('take(Item, Object).   -- to pick up an item.'), nl,
        write('drop(Item, Object).   -- to put down an item.'), nl,
        write('inventory.            -- to list the items that you posess.'), nl,
        write('give(Item, Person).   -- to give a person the item they wanted'), nl,
        write('talk(Person).         -- to talk to a person'), nl,
        write('commands.             -- to see this message again.'), nl,
        write('restart .             -- to restart the game.'), nl,
        write('halt.                 -- to end the game and quit.'), nl,
        nl.

start :-
    nl, write("Welcome to Andrew Tate Prison Escape"), nl,
    commands,
    look.