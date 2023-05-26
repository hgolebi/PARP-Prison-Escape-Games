import Data.IORef
import Control.Monad (forM_)
import System.Exit (exitSuccess)

-- Dynamic facts
data DynamicFact = At Location | ThereIs Item Location | Holding Item | QuestDone Item Location | Cigarettes Item | Locked Location

type World = IORef [DynamicFact]

newWorld :: IO World
newWorld = newIORef []

-- Map definition
data Location = Cell1 | Cell2 | Cell3 | Hallway | GuardRoom | Kitchen | ShowerRoom | Gym | Ventilation | Shed | PrisonYard
  deriving (Eq, Show)

borders :: Location -> Location -> Bool
borders Cell1 Hallway = True
borders Cell2 Hallway = True
borders Cell3 Hallway = True
borders Hallway Cell1 = True
borders Hallway Cell2 = True
borders Hallway Cell3 = True
borders Hallway GuardRoom = True
borders GuardRoom Hallway = True
borders GuardRoom Kitchen = True
borders GuardRoom ShowerRoom = True
borders GuardRoom Gym = True
borders Kitchen GuardRoom = True
borders ShowerRoom GuardRoom = True
borders Gym GuardRoom = True
borders Ventilation Hallway = True
borders Ventilation Shed = True
borders Shed PrisonYard = True
borders _ _ = False

initialFacts :: [DynamicFact]
initialFacts =
  [ Locked Cell1
  , Locked Hallway
  , Locked Ventilation
  , Locked WayToFreedomLightsTurnedOff
  ]

initializeWorld :: World -> IO ()
initializeWorld world = writeIORef world initialFacts

-- Objects in rooms definition
data Object = OccupiedBed | SmallToilet | Teapot | OccupiedBed2 | Table | YourBed | OldMansBed | Toilet | BunkBed | BedCabinet | Shelf
  | VentilationGrid | Desk | TV | Coat | Chair | Oven | Corner | Fridge | Sink | Shower | Shower2 | Shower3 | Shower4 | Cabinet
  | Treadmill | Treadmill2 | Bench | FuseBox | Pole1 | Pole2 | Pole3 | Pole4 | Pole5 | Pole6 | Pole7 | Pole8 | Pole9 | Pole10
  | Pole11 | Pole12 | Pole13 | Pole14 | Pole15 | Pole16 | Pole17 | Pole18 | Pole19 | Pole20 | Pole21
  deriving (Eq, Show)

objectsInRooms :: [(Object, Location)]
objectsInRooms =
  [ (OccupiedBed, Cell1)
  , (SmallToilet, Cell1)
  , (Teapot, Cell1)
  , (OccupiedBed2, Cell1)
  , (Table, Cell2)
  , (YourBed, Cell2)
  , (OldMansBed, Cell2)
  , (Toilet, Cell2)
  , (BunkBed, Cell3)
  , (BedCabinet, Cell3)
  , (Shelf, Cell3)
  , (VentilationGrid, Hallway)
  , (Desk, GuardRoom)
  , (TV, GuardRoom)
  , (Coat, GuardRoom)
  , (Chair, GuardRoom)
  , (Oven, Kitchen)
  , (Chair, Kitchen)
  , (Corner, Kitchen)
  , (Fridge, Kitchen)
  , (Sink, Kitchen)
  , (Shower, ShowerRoom)
  , (Shower2, ShowerRoom)
  , (Shower3, ShowerRoom)
  , (Shower4, ShowerRoom)
  , (Cabinet, ShowerRoom)
  , (Treadmill, Gym)
  , (Treadmill2, Gym)
  , (Bench, Gym)
  , (FuseBox, Shed)
  , (Pole1, PrisonYard)
  , (Pole2, PrisonYard)
  , (Pole3, PrisonYard)
  , (Pole4, PrisonYard)
  , (Pole5, PrisonYard)
  , (Pole6, PrisonYard)
  , (Pole7, PrisonYard)
  , (Pole8, PrisonYard)
  , (Pole9, PrisonYard)
  , (Pole10, PrisonYard)
  , (Pole11, PrisonYard)
  , (Pole12, PrisonYard)
  , (Pole13, PrisonYard)
  , (Pole14, PrisonYard)
  , (Pole15, PrisonYard)
  , (Pole16, PrisonYard)
  , (Pole17, PrisonYard)
  , (Pole18, PrisonYard)
  , (Pole19, PrisonYard)
  , (Pole20, PrisonYard)
  , (Pole21, PrisonYard)
  ]

initializeObjects :: World -> IO ()
initializeObjects world = do
  let facts = map (\(item, location) -> ThereIs item location) objectsInRooms
  modifyIORef world (\facts' -> facts ++ facts')


-- W przepisanej wersji użyłem typów danych i referencji IORef, aby odzwierciedlić dynamiczne fakty gry. 
-- Funkcja newWorld tworzy nowy stan świata gry, a initializeWorld inicjalizuje go początkowymi faktami.
-- Zdefiniowałem również typy danych Location dla lokacji w grze. Funkcja borders określa, czy dwie lokacje sąsiadują ze sobą.
-- Dodatkowo, utworzyłem listę initialFacts, która zawiera początkowe fakty gry. Funkcja initializeWorld zapisuje te fakty w stanie świata.
-- Należy pamiętać, że ta wersja w Haskellu nie uwzględnia jeszcze interakcji ani logiki gry. 
-- Przepisany kod stanowi jedynie podstawę dla dalszego rozwoju gry w Haskellu.


-- People definition
data Person = OldMan | GymGuy | ShoweringPrisoner | Chef | SleepingGuy | SleepingGuy2
  deriving (Eq, Show)

-- People in rooms definition
peopleInRooms :: [(Person, Location)]
peopleInRooms =
  [ (OldMan, Cell2)
  , (GymGuy, Gym)
  , (Guard, GuardRoom)
  , (SleepingGuy, Cell1)
  , (SleepingGuy2, Cell1)
  , (Chef, Kitchen)
  , (ShoweringPrisoner, ShowerRoom)
  ]

initializePeople :: World -> IO ()
initializePeople world = do
  let facts = map (\(person, location) -> ThereIs person location) peopleInRooms
  modifyIORef world (\facts' -> facts ++ facts')

-- Item definition
data Item = Poop | Coin | Cigarette | PlayboyMagazine | Flashlight | Cell1Key | Towel | Batteries | GreatMeal | Coffee
  deriving (Eq, Show)

-- Items locations (in objects) definition
itemLocations :: [(Item, Object)]
itemLocations =
  [ (Poop, Toilet)
  , (Coin, Toilet)
  , (Cigarette, YourBed)
  , (Cigarette, Corner)
  , (Cigarette, BedCabinet)
  , (Cigarette, SleepingGuy)
  , (Cigarette, OccupiedBed2)
  , (Cigarette, Coat)
  , (Cigarette, Desk)
  , (Cigarette, Fridge)
  , (Cigarette, Treadmill)
  , (Cigarette, Cabinet)
  , (Cigarette, Bench)
  , (PlayboyMagazine, Shelf)
  , (Flashlight, Desk)
  , (Cell1Key, Guard)
  , (Towel, Bench)
  , (Batteries, Pillow)
  ]

initializeItemLocations :: World -> IO ()
initializeItemLocations world = do
  let facts = map (\(item, location) -> ThereIs item location) itemLocations
  modifyIORef world (\facts' -> facts ++ facts')

-- Starting in cell2
startLocation :: Location
startLocation = Cell2

-- Initialize the number of cigarettes
initializeCigarettes :: World -> IO ()
initializeCigarettes world = modifyIORef world (\facts -> (Cigarettes 0) : facts)

-- Rule: Look
look :: World -> IO ()
look world = do
  currentLocation <- readIORef world >>= return . getLocation
  putStrLn $ "You're currently at " ++ show currentLocation
  putStrLn "You can see:"
  listObjects currentLocation
  putStrLn "You can go to:"
  availableDestinations currentLocation

listObjects :: Location -> IO ()
listObjects place = do
  objects <- readIORef world >>= return . getObjects
  putStrLn $ intercalate "\n" $ map (\obj -> "* " ++ show obj) (filter (\(_, loc) -> loc == place) objects)

availableDestinations :: Location -> IO ()
availableDestinations place = do
  destinations <- readIORef world >>= return . getDestinations
  putStrLn $ intercalate "\n" $ map (\dest -> "-> " ++ show dest) (filter (\(src, _) -> src == place) destinations)

-- Rule: Go to guard_room
go :: Location -> World -> IO ()
go GuardRoom world = do
  currentLocation <- readIORef world >>= return . getLocation
  guardDistracted <- readIORef world >>= return . isGuardDistracted
  if currentLocation `elem` borders && not guardDistracted then do
    putStrLn "Oh no, there is a guard!"
    putStrLn "I probably should've distracted him first."
    gameOver
  else do
    putStrLn "You can't go there!"
go GuardRoom _ = return ()

-- Rule: Go to ventilation
go :: Location -> World -> IO ()
go Ventilation world = do
  currentLocation <- readIORef world >>= return . getLocation
  ventilationLocked <- readIORef world >>= return . isVentilationLocked
  flashlightHeld <- readIORef world >>= return . isFlashlightHeld
  batteriesHeld <- readIORef world >>= return . isBatteriesHeld
  if currentLocation == Hallway && currentLocation `elem` borders && not ventilationLocked && flashlightHeld && batteriesHeld then do
    modifyIORef world (\world' -> world' { location = Ventilation, borders = borders ++ [(Ventilation, Shed)] })
    look world
  else if currentLocation == Hallway && currentLocation `elem` borders && not ventilationLocked then do
    modifyIORef world (\world' -> world' { location = Ventilation, borders = filter (/= (Ventilation, Shed)) borders })
    putStrLn "It is too dark in here. You cannot see anything. Maybe with a working flashlight you will be able to see more."
    look world
  else
    putStrLn "You can't go there!"

-- Rule: Go to other destinations
go :: Location -> World -> IO ()
go destination world = do
  currentLocation <- readIORef world >>= return . getLocation
  if currentLocation `elem` borders && not (isLocked destination) then do
    modifyIORef world (\world' -> world' { location = destination })
    look world
  else if currentLocation `elem` borders && isLocked destination then
    putStrLn "This place is locked."
  else
    putStrLn "You can't go there!"

-- Debug: Teleport to a location
debugGo :: Location -> World -> IO ()
debugGo place world = do
  modifyIORef world (\world' -> world' { location = place })
  look world

-- Odblokuj pomieszczenie
unlock :: Location -> World -> IO ()
unlock location world = do
  currentLocation <- getLocation world
  case (currentLocation, location) of
    (place, _) | not (isLocked location world) -> putStrLn "To miejsce jest już odblokowane."
    (hallway, Cell1) | isLocked Cell1 world && hasItem Cell1Key world -> do
      modifyIORef world (\world' -> unlockRoom Cell1 world')
      putStrLn "Użyto klucza do odblokowania Cell1."
    (hallway, Cell1) | isLocked Cell1 world && not (hasItem Cell1Key world) -> putStrLn "Potrzebujesz klucza, aby odblokować tę celę."
    (cell2, Hallway) | isLocked Hallway world && hasItem Cell2Key world -> do
      modifyIORef world (\world' -> unlockRoom Hallway world')
      putStrLn "Użyto klucza do odblokowania Hallway."
    (cell2, Hallway) | isLocked Hallway world && not (hasItem Cell2Key world) -> putStrLn "Potrzebujesz klucza, aby odblokować te drzwi."
    (hallway, Ventilation) | isLocked Ventilation world -> putStrLn "Jestem za słaby, żeby to odblokować. Może ktoś silny mi pomoże."
    _ -> putStrLn ("Nie można odblokować " ++ show location ++ ".")
  putStrLn ""

-- Ucieczka z więzienia
escape :: World -> IO ()
escape world = do
  currentLocation <- getLocation world
  case currentLocation of
    PrisonYard | not (isLocked WayToFreedomLightsTurnedOff world) -> do
      putStrLn "Było całkowicie ciemno i udało ci się uciec z więzienia!"
      putStrLn "Gratulacje, wygrałeś grę!"
      finish world
    PrisonYard | isLocked WayToFreedomLightsTurnedOff world -> do
      putStrLn "Wszystcy strażnicy zobaczyli twoje ruchy, gdy światła zostały włączone."
      game_over world
    _ -> putStrLn "Ha ha ha, nie tak szybko... ucieczka nie będzie taka łatwa."
  putStrLn ""

-- Wyłączanie bezpieczników
blowFuses :: World -> IO ()
blowFuses world = do
  currentLocation <- getLocation world
  if currentLocation == Shed
    then do
      modifyIORef world (\world' -> unlockRoom WayToFreedomLightsTurnedOff world')
      putStrLn "Wyłączyłeś zasilanie w więzieniu."
    else putStrLn "Nie możesz tu wyłączyć bezpieczników."
  putStrLn ""

-- Badanie obiektów
investigate :: Object -> World -> IO ()
investigate old_man world = do
  currentLocation <- getLocation world
  if currentLocation == Cell2
    then putStrLn "Stary człowiek: Ty brudny szczurze, trzymaj ręce przy sobie!!"
    else putStrLn "Nic do odkrycia tutaj."
  putStrLn ""

investigate pole16 world = do
  currentLocation <- getLocation world
  if currentLocation == PrisonYard
    then putStrLn "Obok słupa 16 jest dziura w murze. Wpisz 'escape.' aby uciec z więzienia."
    else putStrLn "Nic do odkrycia tutaj."
  putStrLn ""

investigate fuse_box world = do
  currentLocation <- getLocation world
  if currentLocation == Shed
    then putStrLn "Wewnątrz skrzynki z bezpiecznikami znajdują się przełączniki do wyłączania światła. Wpisz 'blow_fuses.' aby odciąć zasilanie."
    else putStrLn "Nic do odkrycia tutaj."
  putStrLn ""

investigate object world = do
  currentLocation <- getLocation world
  let items = getItems object currentLocation world
  case items of
    [] -> putStrLn "Nic do odkrycia tutaj."
    _ -> do
      putStrLn $ "W " ++ object ++ " znajdujesz:"
      mapM_ (\item -> putStrLn $ "* " ++ item) items
  putStrLn ""

-- Sprawdza, czy obiekt jest pusty
isEmpty :: Object -> World -> Bool
isEmpty object world = not $ any (thereIs object) (getObjectLocations world)

-- Zwraca listę przedmiotów znajdujących się w danym obiekcie
getItems :: Object -> Location -> World -> [Item]
getItems object location world = case thereIs object location world of
  True -> filter (/= "empty") (getItemsInObject object world)
  False -> []

-- Zwraca listę przedmiotów w danym obiekcie
getItemsInObject :: Object -> World -> [Item]
getItemsInObject object world = getItemList object (getObjectsWithItems world)

-- Sprawdza, czy istnieje przedmiot w danym obiekcie
thereIs :: Object -> Location -> World -> Bool
thereIs object location world = object `elem` getObjectItems location world

-- Zwraca listę obiektów zawierających przedmioty
getObjectsWithItems :: World -> [Object]
getObjectsWithItems world = nub $ concatMap (getObjectItemsInLocation world) (getObjectLocations world)

-- Zwraca listę przedmiotów w danym obiekcie
getObjectItems :: Object -> World -> [Item]
getObjectItems object world = case M.lookup object (items world) of
  Just items -> items
  Nothing -> []

-- Zwraca listę miejsc, w których znajduje się obiekt
getObjectLocations :: World -> [Location]
getObjectLocations world = M.keys $ objects world

-- Zwraca listę przedmiotów w danym miejscu
getObjectItemsInLocation :: World -> Location -> [Object]
getObjectItemsInLocation world location = M.keys $ M.filter (\items -> location `elem` items) (items world)

-- Podniesienie obiektu
take :: Item -> Object -> World -> IO World
take "flashlight" object world = do
  currentLocation <- getLocation world
  if thereIs object currentLocation world && thereIs "flashlight" object world
    then do
      let updatedWorld = removeItemFromObject "flashlight" object world
      putStrLn "Podniosłeś latarkę. Wygląda na to, że potrzebuje baterii, aby działać."
      return $ addToInventory "flashlight" updatedWorld
    else do
      putStrLn "Nie widzę tego tutaj."
      return world

take "playboy_magazine" object world = do
  currentLocation <- getLocation world
  if thereIs object currentLocation world && thereIs "playboy_magazine" object world
    then do
      let updatedWorld = removeItemFromObject "playboy_magazine" object world
      putStrLn "Ty: Fajny magazyn, może będę mógł go użyć, żeby rozproszyć strażnika."
      putStrLn "Wpisz 'leave(playboy_magazine, Miejsce).' aby zostawić magazyn w jakimś miejscu."
      putStrLn "Musisz być w sąsiednim pomieszczeniu."
      return $ addToInventory "playboy_magazine" updatedWorld
    else do
      putStrLn "Nie widzę tego tutaj."
      return world

take "towel" object world = do
  currentLocation <- getLocation world
  if currentLocation == Gym && thereIs object currentLocation world && thereIs "towel" object world
    then do
      let updatedWorld = removeItemFromObject "towel" object world
      putStrLn "Facet na siłowni: Hej, co ty tam robisz?! To moje ręcznik!"
      putStrLn "Ty: Czy mogę go pożyczyć?"
      putStrLn "Facet na siłowni: Zapomnij. Potrzebuję go do treningu."
      return updatedWorld
    else if currentLocation == Gym && thereIs object currentLocation world && not (thereIs "gym_guy" Gym world)
      then do
        let updatedWorld = removeItemFromObject "towel" object world
        putStrLn "OK."
        return $ addToInventory "towel" updatedWorld
      else do
        putStrLn "Nie widzę tego tutaj."
        return world

take item object world = do
  currentLocation <- getLocation world
  if thereIs object currentLocation world && thereIs item object world
    then do
      let updatedWorld = removeItemFromObject item object world
      putStrLn "OK."
      return $ addToInventory item updatedWorld
    else do
      putStrLn "Nie widzę tego tutaj."
      return world

-- Usuwa przedmiot z obiektu
removeItemFromObject :: Item -> Object -> World -> World
removeItemFromObject item object world = case M.lookup object (items world) of
  Just items -> world { items = M.insert object (delete item items) (items world) }
  Nothing -> world

-- Dodaje przedmiot do inwentarza gracza
addToInventory :: Item -> World -> World
addToInventory item world = world { inventory = item : inventory world }

-- Odkładanie obiektu
leave :: Item -> Location -> World -> IO World
leave "playboy_magazine" "guard_room" world = do
  currentLocation <- getLocation world
  if isHolding "playboy_magazine" world && currentLocation == Hallway
    then do
      let updatedWorld = removeFromInventory "playboy_magazine" world
      putStrLn "Zostawiłeś magazyn w pokoju strażnika. Wygląda na to, że strażnik jest rozproszony ;)"
      return $ addDistractedGuard updatedWorld
    else do
      putStrLn "Prawdopodobnie nie powinienem zostawiać tego tam."
      return world

leave "playboy_magazine" destination world = do
  currentLocation <- getLocation world
  if currentLocation /= destination || not (isAdjacent currentLocation destination)
    then do
      putStrLn "Jesteś zbyt daleko."
      return world
    else do
      putStrLn "Nie masz tego przedmiotu."
      return world

leave _ _ world = do
  putStrLn "Nie masz tego przedmiotu."
  return world

-- Dodaje atrybut "distracted" do strażnika
addDistractedGuard :: World -> World
addDistractedGuard world = world { attributes = S.insert "distracted" (attributes world) }

-- Rozmowa z postacią
talk :: Person -> World -> IO World
talk "sleeping_guy" world = do
  putStrLn "Sleeping guy: Zzzzz..."
  return world

talk "sleeping_guy2" world = do
  putStrLn "Sleeping guy: Zzzzzzzz..."
  return world

talk "guard" world = do
  putStrLn "Guard: Wait, what are you doing here?!"
  game_over
  return world

talk "old_man" world
  | not (quest_done "quest1" "old_man") && not (isHolding "cell2_key" world) = do
    putStrLn "You: Psst... I was thinking about escape. Are you in?"
    putStrLn "Old Man: Escape, huh? It won't be easy. I've been here for years and I'm too old for this."
    putStrLn "You: Damn... But you probably know this prison quite well. Do you have any advice?"
    putStrLn "Old Man: Yes, but it will cost. Please bring me 5 cigarettes and we will talk."
    putStrLn "You: I don't have that much.."
    putStrLn "Old Man: Here, take that key. Maybe you will find some outside."
    putStrLn "You: Wait, you had a key to our cell all this time?!"
    putStrLn "Old Man: Maybe I had, maybe I didn't. That's not important now. Just take the key and find me some cigarettes."
    putStrLn "You received cell2_key."
    return $ addToInventory "cell2_key" world

talk "old_man" world
  | not (isWaitingFor "cigarettes" world) && quest_done "quest1" "old_man" && not (quest_done "quest2" "old_man") = do
    putStrLn "You: Okey, could you give me some advice now?"
    putStrLn "Old Man: Alright. There is a hole in the wall by the 16th pole on a prison yard."
    putStrLn "You: But wait, the lights are on, everything will be visible."
    putStrLn "Old Man: I don't give free information. Bring 5 more cigarettes."
    return $ setWaitingFor "cigarettes" world

talk "old_man" world
  | not (quest_done "all_quests" "old_man") && quest_done "quest2" "old_man" = do
    putStrLn "You: What about the light?"
    putStrLn "Old Man: You can break the ventilation hole in the hallway and get into the room with fuses, where you turn off the light."
    putStrLn "You: Holy Chicken Trolley, that's my opportunity!!"
    return $ setBorders "hallway" "ventilation" world

talk "old_man" world
  | quest_done "all_quests" "old_man" = do
    putStrLn "You: Hi, I..."
    putStrLn "Old Man: Don't have time for you now, get lost."
    return world

talk "old_man" world = do
  putStrLn "There is no one named Old Man here."
  return world

talk "gym_guy" world
  | not (isWaitingFor "meal" world) && quest_done "quest2" "old_man" && not (quest_done "meal_quest" "gym_guy") = do
    putStrLn "You see a strong guy that is exhausted after his training."
    putStrLn "You: Hey! I have a case. Could I do something for you in return for a small favor?"
    putStrLn "Gym Guy: You little man, what would you need help for?"
    putStrLn "You: To break the ventilation hole."
    putStrLn "Gym Guy: It's a piece of cake for me. Bring me a great meal cause I need to refill my carbs. Then I'll do the job."
    return $ setWaitingFor "meal" world

talk "gym_guy" world
  | not (quest_done "all_quests" "gym_guy") && quest_done "meal_quest" "gym_guy" = do
    putStrLn "You: So, will you help me with your muscles?"
    putStrLn "Gym Guy: Yeah, the meal was great. Take me to the place."
    putStrLn "You and the Gym Guy went to the ventilation grid and broke it."
    return $ moveTo "hallway" $ addToInventory "great_meal" $ removeFromLocation "gym_guy" $ removeFromLocation "ventilation_grid" $ unlock "ventilation" world

talk "gym_guy" world
  | quest_done "all_quests" "gym_guy" = do
    putStrLn "You: Hi, I..."
    putStrLn "Gym Guy: Don't have time for you now, get lost."
    return world

talk "gym_guy" world = do
  putStrLn "There is no one named Gym Guy here."
  return world

talk "showering_prisoner" world
  | not (isWaitingFor "towel" world) && not (quest_done "towel_quest" "showering_prisoner") = do
    putStrLn "Prisoner: Hey what are you looking at?!"
    putStrLn "You: I was just.."
    putStrLn "Prisoner: Get out now!! Or actually, wait.. Bring me a towel!"
    putStrLn "You: Why would I?"
    putStrLn "Prisoner: You dare to ask?!"
    putStrLn "You: I'm not going to do this for free."
    putStrLn "Prisoner: Fine, if you decide to help me I'll give you something in return."
    return $ setWaitingFor "towel" world

talk "showering_prisoner" world
  | not (quest_done "all_quests" "showering_prisoner") && quest_done "towel_quest" "showering_prisoner" = do
    putStrLn "You: You received your towel. What about my reward?"
    putStrLn "Prisoner: Hmm... I don't have anything on me, but.."
    putStrLn "You: But what?!"
    putStrLn "Prisoner: Do you want some batteries?"
    putStrLn "You: Why would I need batteries?"
    putStrLn "Prisoner: I dunno, maybe to power up a flashlight or something.."
    putStrLn "You: Hmmm... Okay, give me those batteries!"
    putStrLn "Prisoner: You can find them under a pillow in cell 3."
    return $ addToLocation "pillow" "cell3" $ setQuestDone "all_quests" "showering_prisoner" world

talk "showering_prisoner" world
  | quest_done "all_quests" "showering_prisoner" = do
    putStrLn "You: Hi, I..."
    putStrLn "Prisoner: Don't have time for you now, get lost."
    return world

talk "showering_prisoner" world = do
  putStrLn "There is no one named Showering Prisoner here."
  return world

talk "chef" world
  | not (isWaitingFor "coffee" world) && not (quest_done "coffee_quest" "chef") = do
    putStrLn "You: Hi! I've heard that you're the best chef in here. Could you make me your signature meal?"
    putStrLn "Chef: Nice words won't be enough. I'am actually pretty tired, if you could bring me some coffee then I'll cook something."
    putStrLn "You: I should have some in my cell, I'll be in a moment."
    return $ addToLocation "coffee" "table" $ setWaitingFor "coffee" world

talk "chef" world
  | not (quest_done "all_quests" "chef") && quest_done "coffee_quest" "chef" = do
    putStrLn "You: Now you're quite caffenaited, aren't you?"
    putStrLn "Chef: Yeah, thanks. I'll cook something quickly."
    putStrLn "After few minutes chef hands you a hot meal."
    return $ addToInventory "great_meal" $ setQuestDone "all_quests" "chef" world

talk "chef" world
  | quest_done "all_quests" "chef" = do
    putStrLn "You: Hi, I..."
    putStrLn "Chef: Don't have time for you now, get lost."
    return world

talk "chef" world = do
  putStrLn "There is no one named Chef here."
  return world

talk person world = do
  putStrLn $ "There is no one named " ++ person ++ " here."
  return world

give :: Item -> Person -> World -> IO World
give Cigarette _ world = do
  putStrLn "To give someone cigarettes, type 'give Cigarettes Person.'"
  return world

give (Cigarettes count) OldMan world
  | atLocation "old_man" world && count >= 5 = do
    let newCount = count - 5
        updatedCigarettes = Cigarettes newCount
        quest = if not (questDone "quest1" "old_man") then "quest1" else "quest2"
        updatedWorld = removeItem (Cigarettes count) $ addItem updatedCigarettes $
          setQuestDone quest "old_man" world
    putStrLn "Old Man: Ah, you've brought the cigarettes. Good."
    putStrLn "You hand the cigarettes to the Old Man."
    return updatedWorld

give (Cigarettes _) OldMan world
  | atLocation "old_man" world = do
    putStrLn "Old Man: You don't have enough cigarettes."
    return world

give item OldMan world
  | atLocation "old_man" world && item `elem` getInventory world = do
    putStrLn "Old Man: I don't want that item."
    return world

give (GreatMeal) GymGuy world
  | atLocation "gym_guy" world && GreatMeal `elem` getInventory world = do
    let updatedWorld = removeItem GreatMeal $ setQuestDone "meal_quest" "gym_guy" world
    putStrLn "Gym Guy: Just on time, I'm hungry as hell."
    putStrLn "You hand the meal to the Gym Guy."
    return updatedWorld

give item GymGuy world
  | atLocation "gym_guy" world && item `elem` getInventory world = do
    putStrLn "Gym Guy: I don't want that item."
    return world

give _ GymGuy world
  | atLocation "gym_guy" world = do
    putStrLn "Gym Guy: You don't have the meal."
    return world

give (Coffee) Chef world
  | atLocation "chef" world && Coffee `elem` getInventory world = do
    let updatedWorld = removeItem Coffee $ setQuestDone "coffee_quest" "chef" world
    putStrLn "Chef: Oh, you have the coffee. I need a boost of energy."
    putStrLn "You hand the coffee to the Chef."
    return updatedWorld

give item Chef world
  | atLocation "chef" world && item `elem` getInventory world = do
    putStrLn "Chef: I don't want that item."
    return world

give _ Chef world
  | atLocation "chef" world = do
    putStrLn "Chef: You don't have the coffee."
    return world

give (Towel) ShoweringPrisoner world
  | atLocation "showering_prisoner" world && Towel `elem` getInventory world = do
    let updatedWorld = removeItem Towel $ setQuestDone "towel_quest" "showering_prisoner" world
    putStrLn "Prisoner: Oh, there you are. Gimmie the towel."
    putStrLn "You hand the towel to the Showering Prisoner."
    return updatedWorld

give item ShoweringPrisoner world
  | atLocation "showering_prisoner" world && item `elem` getInventory world = do
    putStrLn "Prisoner: I don't want that item."
    return world

give _ ShoweringPrisoner world
  | atLocation "showering_prisoner" world = do
    putStrLn "Prisoner: You don't have the towel."
    return world

give _ _ world = do
  putStrLn "This person doesn't want that item."
  return world

-- This rule describes how to display inventory's contents and how the number of picked up cigarettes increases
-- import Control.Monad (forM_)

inventory :: World -> IO ()
inventory world = do
  putStrLn "You have:"
  listItems world
  printCigarettes world

listItems :: World -> IO ()
listItems world = do
  let items = getInventory world
  forM_ items $ \item -> putStrLn $ "* " ++ show item

printCigarettes :: World -> IO ()
printCigarettes world = do
  let count = getCigaretteCount world
  putStrLn $ "* " ++ show count ++ " cigarettes"

increaseCigarettes :: Int -> World -> World
increaseCigarettes n world =
  let count = getCigaretteCount world
      newCount = count + n
  in setCigaretteCount newCount world


--These rules are responsible for finishing/restarting the game
-- import System.Exit (exitSuccess)

restart :: World -> IO World
restart _ = do
  putStrLn "Restarting the game..."
  makeWorld >>= start

gameOver :: IO ()
gameOver = do
  putStrLn "YOU GOT CAUGHT!"
  putStrLn "Type 'restart' to start over again"

finish :: IO ()
finish = do
  putStrLn "The game is over. Please enter the 'halt' command."
  exitSuccess

commands :: IO ()
commands = do
  putStrLn ""
  putStrLn "Enter commands using standard Haskell syntax."
  putStrLn "Available commands are:"
  putStrLn "start                 -- to start the game."
  putStrLn "go Destination        -- to go to the selected destination."
  putStrLn "unlock Destination    -- to unlock the passage to the destination."
  putStrLn "look                  -- to look around you again."
  putStrLn "investigate Object    -- to see if there is any item in the object."
  putStrLn "take Item Object      -- to pick up an item from the object (or person)."
  putStrLn "inventory             -- to list the items that you possess."
  putStrLn "give Item Person      -- to give a person the item they wanted."
  putStrLn "talk Person           -- to talk to a person."
  putStrLn "commands              -- to see this message again."
  putStrLn "restart               -- to restart the game."
  putStrLn "halt                  -- to end the game and quit."
  putStrLn ""