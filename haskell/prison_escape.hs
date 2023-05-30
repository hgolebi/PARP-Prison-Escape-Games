import Data.IORef
import Control.Monad (forM_)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Data.List (find)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe)


-- Dynamic facts
data DynamicFact = At Location | ThereIsOL Object Location | ThereIsIO Item Object | ThereIsPL Person Location
  | QuestDone Item Location | Cigarettes Item | Locked Location | Distracted Person | Done Quest Person
  | Waiting Person | Borders Location Location
 
-- type World = IORef [DynamicFact]
type World = IORef ([DynamicFact], S.Set String)  -- contains facts and set of items (inventory)

newWorld :: IO World
newWorld = newIORef []

-- Quests definition
data Quest = Quest1 | Quest2 | AllQuests | MealQuest | TowelQuest | CoffeeQuest

-- Map definition
data Location = Cell1 | Cell2 | Cell3 | Hallway | GuardRoom | Kitchen | ShowerRoom | Gym | Ventilation
  | Shed | PrisonYard | WayToFreedomLightsTurnedOff
  deriving (Eq, Show)

bordersInMap :: [(Location, Location)]
bordersInMap =
  [ (Cell1, Hallway)
  , (Cell2, Hallway)
  , (Cell3, Hallway)
  , (Hallway, Cell1)
  , (Hallway, Cell2)
  , (Hallway, Cell3)
  , (Hallway, GuardRoom)
  , (GuardRoom, Hallway)
  , (GuardRoom, Kitchen)
  , (GuardRoom, ShowerRoom)
  , (GuardRoom, Gym)
  , (Kitchen, GuardRoom)
  , (ShowerRoom, GuardRoom)
  , (Gym, GuardRoom)
  , (Ventilation, Hallway)
  , (Ventilation, Shed)
  , (Shed, PrisonYard)
  ]

initializeBorders :: World -> IO ()
initializeBorders world = do
  let facts = map (\(object, location) -> ThereIsOL object location) objectsInRooms
  modifyIORef world (\facts' -> facts ++ facts')

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
  | Pole11 | Pole12 | Pole13 | Pole14 | Pole15 | Pole16 | Pole17 | Pole18 | Pole19 | Pole20 | Pole21 | Pillow
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
  let facts = map (\(location1, location2) -> Borders location1 location2) bordersInMap
  modifyIORef world (\facts' -> facts ++ facts')


-- W przepisanej wersji użyłem typów danych i referencji IORef, aby odzwierciedlić dynamiczne fakty gry. 
-- Funkcja newWorld tworzy nowy stan świata gry, a initializeWorld inicjalizuje go początkowymi faktami.
-- Zdefiniowałem również typy danych Location dla lokacji w grze. Funkcja borders określa, czy dwie lokacje sąsiadują ze sobą.
-- Dodatkowo, utworzyłem listę initialFacts, która zawiera początkowe fakty gry. Funkcja initializeWorld zapisuje te fakty w stanie świata.
-- Należy pamiętać, że ta wersja w Haskellu nie uwzględnia jeszcze interakcji ani logiki gry. 
-- Przepisany kod stanowi jedynie podstawę dla dalszego rozwoju gry w Haskellu.


-- People definition
data Person = OldMan | GymGuy | ShoweringPrisoner | Chef | SleepingGuy | SleepingGuy2 | Guard
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
  let facts = map (\(person, location) -> ThereIsPL person location) peopleInRooms
  modifyIORef world (\facts' -> facts ++ facts')


-- Item definition
data Item = Poop | Coin | Cigarette | PlayboyMagazine | Flashlight | Cell1Key | Cell2Key | Towel | Batteries | GreatMeal | Coffee
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
  let facts = map (\(item, object) -> ThereIsIO item object) itemLocations
  modifyIORef world (\facts' -> facts ++ facts')

-- Starting in cell2
startLocation :: Location
startLocation = Cell2

-- Initialize the number of cigarettes
initializeCigarettes :: World -> IO ()
initializeCigarettes world = modifyIORef world (\facts -> (Cigarettes 0) : facts)

-- Rule: get location
getLocation :: [DynamicFact] -> Maybe Location
getLocation facts = listToMaybe [loc | At loc <- facts]


-- Rule: check if location is locked
isLocked :: Location -> [DynamicFact] -> Bool
isLocked location facts = case find isLockedFact facts of
  Just _ -> True
  Nothing -> False
  where
    isLockedFact (Locked loc) = loc == location
    isLockedFact _ = False

-- getting items from inventory
getInventory :: World -> [Item]
getInventory world =
  case readIORef world of
    (_, items) -> items

-- Rule: check if there is certain item in inventory
hasItem :: Item -> World -> Bool
hasItem item world = do
  (_, items) <- readIORef world
  return (item `elem` items)

-- adding item to the inventory
addItemToInventory :: Item -> World -> IO ()
addItemToInventory item world = modifyIORef world (\(facts, items) -> (facts, item : items))

-- removing item from inventory
removeItem :: Item -> World -> IO ()
removeItem item world = modifyIORef world (\(facts, items) -> (facts, filter (/= item) items))

-- adding object to location
addObjectToLocation :: Object -> Location -> World -> IO ()
addObjectToLocation object location world = do
  modifyIORef world (\(facts, items) -> (ThereIsOL object location : facts, items))

-- adding item to object
addItemToObject :: Item -> Object -> World -> IO ()
addItemToObject item object world = do
  modifyIORef world (\(facts, items) -> (ThereIsIO item object : facts, items))

-- Sprawdza, czy w danej lokalizacji znajduje się dana osoba
thereIsPersonInLocation :: Person -> Location -> World -> Bool
thereIsPersonInLocation person location world = do
  (facts, _) <- readIORef world
  (ThereIsPL person location) `elem` facts

-- Sprawdza, czy w danym obiekcie znajduje się dany przedmiot
thereIsItemInObject :: Object -> Item -> World -> Bool
thereIsItemInObject object item world = do
  (facts, _) <- readIORef world
  (ThereIsIO item object) `elem` facts

-- Sprawdza, czy dany obiekt znajduje się w danej lokalizacji
thereIsObjectInLocation :: Object -> Location -> World -> Bool
thereIsObjectInLocation object location world = do
  (facts, _) <- readIORef world
  (ThereIsOL object location) `elem` facts

-- checking if certain quest is done
-- isQuestDone :: Quest -> Person -> World -> Bool
-- isQuestDone quest person world =
--   case readIORef world of
--     (facts, _) -> any (\fact -> case fact of { Done q p -> q == quest && p == person; _ -> False }) facts

isQuestDone :: Quest -> Person -> World -> Bool
isQuestDone quest person world = do
  (facts, _) <- readIORef world
  (Done quest person) `elem` facts

-- adding fact Done Quest
setQuestDone :: Quest -> Person -> World -> IO ()
setQuestDone quest person world = do
  modifyIORef world (\(facts, items) -> (Done quest person : facts, items))

-- Rule: checking if certain person is waiting
isWaitingFor :: Person -> World -> Bool
isWaitingFor person world = do
  (facts, _) <- readIORef world
  Waiting person `elem` facts

-- adds a fact that someone is waiting for
setWaitingFor :: Person -> World -> IO ()
setWaitingFor person world = do
  modifyIORef world (\(facts, items) -> (Waiting person : facts, items))

-- adds a fact that some location borders with other
setBorder :: Location -> Location -> World -> IO ()
setBorder location1 location2 world = do
  modifyIORef world (\(facts, items) -> (Borders location1 location2 : facts, items))

-- checking if one location borders with other
doesBorder :: Location -> Location -> World -> Bool
doesBorder location1 location2 world = do
  (facts, _) <- readIORef world
  (Borders location1 location2) `elem` facts



-- Rule: Look
look :: World -> IO ()
look world = do
  (facts, items) <- readIORef world
  let currentLocation = getLocation facts
  putStrLn $ "You're currently at " ++ show currentLocation
  putStrLn "You can see:"
  listObjects currentLocation
  putStrLn "You can go to:"
  availableDestinations currentLocation


listObjects :: World -> Location -> IO ()
listObjects world place = do
  (facts, _) <- readIORef world
  let objects = filter (\fact -> case fact of { ThereIsOL _ loc -> loc == place; _ -> False }) facts
  putStrLn $ intercalate "\n" $ map (\obj -> "* " ++ show obj) objects


availableDestinations :: World -> Location -> IO ()
availableDestinations world place = do
  (facts, _) <- readIORef world
  let destinations = filter (\fact -> case fact of { At src -> src == place; _ -> False }) facts
  putStrLn $ intercalate "\n" $ map (\dest -> "-> " ++ show dest) destinations


-- Rules: Go to guard_room, Go to ventilation, Go to other destinations
go :: Location -> World -> IO ()
go location world = do
  (facts, items) <- readIORef world
  let currentLocation = getLocation facts
      guardDistracted = Distracted Guard `elem` facts
      ventilationLocked = isLocked Ventilation facts
      flashlightHeld = hasItem Flashlight world
      batteriesHeld = hasItem Batteries world
      newFacts = filter (\fact -> case fact of { At _ -> False; _ -> True }) facts ++ [At location]

  case (location, currentLocation) of
    (GuardRoom, GuardRoom) -> do
      if doesBorder location currentLocation world && not guardDistracted then do
        putStrLn "Oh no, there is a guard!"
        putStrLn "I probably should've distracted him first."
        gameOver
      else do
        putStrLn "You can't go there!"

    (Ventilation, Hallway) -> do
      if doesBorder location currentLocation world && not ventilationLocked then do
        if flashlightHeld && batteriesHeld then do
          writeIORef world (newFacts, items)
          look world
        else do
          writeIORef world (newFacts, items)
          putStrLn "It is too dark in here. You cannot see anything. Maybe with a working Flashlight you will be able to see more."
          look world
      else do
        putStrLn "You can't go there!"

    (_, _) -> do
      if doesBorder location currentLocation world && not (isLocked location) then do
        writeIORef world (newFacts, items)
        look world
      else if doesBorder location currentLocation world && isLocked location then
        putStrLn "This place is locked."
      else
        putStrLn "You can't go there!"


-- Debug: Teleport to a location
debugGo :: Location -> World -> IO ()
debugGo place world = do
  modifyIORef world (\(facts, items) -> (updateFactsLocation facts place, items))
  look world
  where
    updateFactsLocation :: [DynamicFact] -> Location -> [DynamicFact]
    updateFactsLocation facts newLocation = map updateFact facts
      where
        updateFact :: DynamicFact -> DynamicFact
        updateFact (At _) = At newLocation
        updateFact fact = fact


-- Unlocking locations
unlockLocation :: Location -> World -> IO ()
unlockLocation location world = do
  (facts, items) <- readIORef world
  let currentLocation = getLocation facts
      newFacts = filter (\fact -> case fact of { At _ -> False; _ -> True }) facts ++ [At location]
  writeIORef world (newFacts, items)
  putStrLn $ "Unlocked location: " ++ show location

unlock :: Location -> World -> IO ()
unlock location world = do
  (facts, items) <- readIORef world
  let currentLocation = getLocation facts
  case (currentLocation, location) of
    (place, _) | not (isLocked location world) -> putStrLn "To miejsce jest już odblokowane."
    (hallway, Cell1) | isLocked Cell1 world && hasItem Cell1Key world -> do
      modifyIORef world (\world' -> unlockLocation Cell1 world')
      putStrLn "Użyto klucza do odblokowania Cell1."
    (hallway, Cell1) | isLocked Cell1 world && not (hasItem Cell1Key world) -> putStrLn "Potrzebujesz klucza, aby odblokować tę celę."
    (cell2, Hallway) | isLocked Hallway world && hasItem Cell2Key world -> do
      modifyIORef world (\world' -> unlockLocation Hallway world')
      putStrLn "Użyto klucza do odblokowania Hallway."
    (cell2, Hallway) | isLocked Hallway world && not (hasItem Cell2Key world) -> putStrLn "Potrzebujesz klucza, aby odblokować te drzwi."
    (hallway, Ventilation) | isLocked Ventilation world -> putStrLn "Jestem za słaby, żeby to odblokować. Może ktoś silny mi pomoże."
    _ -> putStrLn ("Nie można odblokować " ++ show location ++ ".")
  putStrLn ""

-- Ucieczka z więzienia
escape :: World -> IO ()
escape world = do
  (facts, items) <- readIORef world
  let currentLocation = getLocation facts
  case currentLocation of
    PrisonYard | not (isLocked WayToFreedomLightsTurnedOff world) -> do
      putStrLn "Było całkowicie ciemno i udało ci się uciec z więzienia!"
      putStrLn "Gratulacje, wygrałeś grę!"
      finish world
    PrisonYard | isLocked WayToFreedomLightsTurnedOff world -> do
      putStrLn "Wszystcy strażnicy zobaczyli twoje ruchy, gdy światła zostały włączone."
      gameOver world
    _ -> putStrLn "Ha ha ha, nie tak szybko... ucieczka nie będzie taka łatwa."
  putStrLn ""

-- Wyłączanie bezpieczników
blowFuses :: World -> IO ()
blowFuses world = do
  (facts, items) <- readIORef world
  let currentLocation = getLocation facts
  if currentLocation == Shed
    then do
      modifyIORef world (\world' -> unlockLocation WayToFreedomLightsTurnedOff world')
      putStrLn "Wyłączyłeś zasilanie w więzieniu."
    else putStrLn "Nie możesz tu wyłączyć bezpieczników."
  putStrLn ""


-- Badanie obiektów
investigate :: Object -> World -> IO ()
investigate old_man world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
  if currentLocation == Cell2
    then putStrLn "Stary człowiek: Ty brudny szczurze, trzymaj ręce przy sobie!!"
    else putStrLn "Nic do odkrycia tutaj."
  putStrLn ""

investigate pole16 world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
  if currentLocation == PrisonYard
    then putStrLn "Obok słupa 16 jest dziura w murze. Wpisz 'escape.' aby uciec z więzienia."
    else putStrLn "Nic do odkrycia tutaj."
  putStrLn ""

investigate fuse_box world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
  if currentLocation == Shed
    then putStrLn "Wewnątrz skrzynki z bezpiecznikami znajdują się przełączniki do wyłączania światła. Wpisz 'blow_fuses.' aby odciąć zasilanie."
    else putStrLn "Nic do odkrycia tutaj."
  putStrLn ""

investigate object world = do
  let objectItems = getObjectItems object world
  case objectItems of
    [] -> putStrLn "Nic do odkrycia tutaj."
    _ -> do
      putStrLn $ "W " ++ object ++ " znajdujesz:"
      mapM_ (\item -> putStrLn $ "* " ++ item) objectItems
  putStrLn ""


-- Zwraca listę przedmiotów znajdujących się w danym obiekcie
getObjectItems :: Object -> World -> [Item]
getObjectItems object world = do
  (facts, _) <- readIORef world
  let items = filter (\fact -> case fact of { ThereIsIO item obj -> obj == object; _ -> False }) facts
  [item | ThereIsIO item _ <- items]


-- PEWNIE ZLE NAPISANE, ALE NIEPOTRZEBNE WIEC WALIC
-- Sprawdza, czy obiekt jest pusty
-- isEmpty :: Object -> World -> Bool
-- isEmpty object world = not $ any (thereIs object) (getObjectLocations world)

-- Sprawdza, czy istnieje przedmiot w danym obiekcie
-- thereIs :: Object -> Location -> World -> Bool
-- thereIs object location world = object `elem` getObjectItems location world

-- Zwraca listę obiektów zawierających przedmioty
-- getObjectsWithItems :: World -> [Object]
-- getObjectsWithItems world = nub $ concatMap (getObjectItemsInLocation world) (getObjectLocations world)

-- Zwraca listę miejsc, w których znajduje się obiekt
-- getObjectLocations :: World -> [Location]
-- getObjectLocations world = M.keys $ objects world

-- Zwraca listę przedmiotów w danym miejscu
-- getObjectItemsInLocation :: World -> Location -> [Object]
-- getObjectItemsInLocation world location = M.keys $ M.filter (\items -> location `elem` items) (items world)


-- Podniesienie obiektu
take :: Item -> Object -> World -> IO World
take Flashlight object world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
  if thereIsObjectInLocation object currentLocation world && thereIsItemInObject Flashlight object world
    then do
      let updatedWorld = removeItemFromObject Flashlight object world
      putStrLn "Podniosłeś latarkę. Wygląda na to, że potrzebuje baterii, aby działać."
      return $ addItemToInventory Flashlight updatedWorld
    else do
      putStrLn "Nie widzę tego tutaj."
      return world

take PlayboyMagazine object world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
  if thereIsObjectInLocation object currentLocation world && thereIsItemInObject PlayboyMagazine object world
    then do
      let updatedWorld = removeItemFromObject PlayboyMagazine object world
      putStrLn "Ty: Fajny magazyn, może będę mógł go użyć, żeby rozproszyć strażnika."
      putStrLn "Wpisz 'leave(playboy_magazine, Miejsce).' aby zostawić magazyn w jakimś miejscu."
      putStrLn "Musisz być w sąsiednim pomieszczeniu."
      return $ addItemToInventory PlayboyMagazine updatedWorld
    else do
      putStrLn "Nie widzę tego tutaj."
      return world

take Towel object world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
  if currentLocation == Gym && thereIsObjectInLocation object currentLocation world && thereIsItemInObject Towel object world
    then do
      let updatedWorld = removeItemFromObject Towel object world
      putStrLn "Facet na siłowni: Hej, co ty tam robisz?! To moje ręcznik!"
      putStrLn "Ty: Czy mogę go pożyczyć?"
      putStrLn "Facet na siłowni: Zapomnij. Potrzebuję go do treningu."
      return updatedWorld
    else if currentLocation == Gym && thereIsObjectInLocation object currentLocation world && not (thereIsPersonInLocation GymGuy Gym world)
      then do
        let updatedWorld = removeItemFromObject Towel object world
        putStrLn "OK."
        return $ addItemToInventory Towel updatedWorld
      else do
        putStrLn "Nie widzę tego tutaj."
        return world

take item object world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
  if thereIsObjectInLocation object currentLocation world && thereIsItemInObject item object world
    then do
      let updatedWorld = removeItemFromObject item object world
      putStrLn "OK."
      return $ addItemToInventory item updatedWorld
    else do
      putStrLn "Nie widzę tego tutaj."
      return world

-- Usuwa przedmiot z obiektu
removeItemFromObject :: Item -> Object -> World -> World
removeItemFromObject item object world = do
  modifyIORef world (\(facts, items) -> (filter (not . isTargetFact) facts, items))
  where
    isTargetFact (ThereIsIO i o) = i == item && isTargetObject o
    isTargetFact _ = False
    isTargetObject Desk = object == Desk
    isTargetObject _ = False

-- Odkładanie obiektu
leave :: Item -> Location -> World -> IO World
leave PlayboyMagazine "guard_room" world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
      magazineHeld = hasItem PlayboyMagazine world
  if magazineHeld && currentLocation == Hallway
    then do
      let updatedWorld = removeItem PlayboyMagazine world
      putStrLn "Zostawiłeś magazyn w pokoju strażnika. Wygląda na to, że strażnik jest rozproszony ;)"
      return $ addDistractedGuard updatedWorld
    else do
      putStrLn "Prawdopodobnie nie powinienem zostawiać tego tam."
      return world

leave PlayboyMagazine destination world = do
  (facts, _) <- readIORef world
  let currentLocation = getLocation facts
  if currentLocation /= destination || not (doesBorder destination currentLocation world)
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
-- addDistractedGuard :: World -> World
-- addDistractedGuard world = world { attributes = S.insert "distracted" (attributes world) }
addDistractedGuard :: World -> World
addDistractedGuard world = do
  modifyIORef world (\(facts, attrs) -> (facts, S.insert "distracted" attrs))
  world

-- removing person from location
removePersonFromLocation :: Person -> Location -> World -> IO ()
removePersonFromLocation person location world = do
  modifyIORef world (\(facts, items) -> (filter (\fact -> case fact of { ThereIsPL p l -> p /= person || l /= location; _ -> True }) facts, items))

-- removing object from location
removeObjectFromLocation :: Object -> Location -> World -> IO ()
removeObjectFromLocation object location world = do
  modifyIORef world (\(facts, items) -> (filter (\fact -> case fact of { ThereIsOL o l -> o /= object || l /= location; _ -> True }) facts, items))


-- Rozmowa z postacią
talk :: Person -> World -> IO World
talk SleepingGuy world = do
  putStrLn "Sleeping guy: Zzzzz..."
  return world

talk SleepingGuy2 world = do
  putStrLn "Sleeping guy: Zzzzzzzz..."
  return world

talk Guard world = do
  putStrLn "Guard: Wait, what are you doing here?!"
  gameOver
  return world

talk OldMan world
  | not (isQuestDone Quest1 OldMan world) && not (hasItem Cell2Key world) = do
    putStrLn "You: Psst... I was thinking about escape. Are you in?"
    putStrLn "Old Man: Escape, huh? It won't be easy. I've been here for years and I'm too old for this."
    putStrLn "You: Damn... But you probably know this prison quite well. Do you have any advice?"
    putStrLn "Old Man: Yes, but it will cost. Please bring me 5 cigarettes and we will talk."
    putStrLn "You: I don't have that much.."
    putStrLn "Old Man: Here, take that key. Maybe you will find some outside."
    putStrLn "You: Wait, you had a key to our cell all this time?!"
    putStrLn "Old Man: Maybe I had, maybe I didn't. That's not important now. Just take the key and find me some cigarettes."
    putStrLn "You received cell2_key."
    return $ addItemToInventory Cell2Key world

talk OldMan world
  | not (isWaitingFor Cigarettes world) && isQuestDone Quest1 OldMan world && not (isQuestDone Quest2 OldMan world) = do
    putStrLn "You: Okey, could you give me some advice now?"
    putStrLn "Old Man: Alright. There is a hole in the wall by the 16th pole on a prison yard."
    putStrLn "You: But wait, the lights are on, everything will be visible."
    putStrLn "Old Man: I don't give free information. Bring 5 more cigarettes."
    return $ setWaitingFor OldMan world

talk OldMan world
  | not (isQuestDone AllQuests OldMan world) && isQuestDone Quest2 OldMan world = do
    putStrLn "You: What about the light?"
    putStrLn "Old Man: You can break the ventilation hole in the hallway and get into the room with fuses, where you turn off the light."
    putStrLn "You: Holy Chicken Trolley, that's my opportunity!!"
    return $ setBorder Hallway Ventilation world

talk OldMan world
  | isQuestDone AllQuests OldMan world = do
    putStrLn "You: Hi, I..."
    putStrLn "Old Man: Don't have time for you now, get lost."
    return world

talk OldMan world = do
  putStrLn "There is no one named Old Man here."
  return world

talk GymGuy world
  | not (isWaitingFor GreatMeal world) && isQuestDone Quest2 OldMan world && not (isQuestDone MealQuest GymGuy world) = do
    putStrLn "You see a strong guy that is exhausted after his training."
    putStrLn "You: Hey! I have a case. Could I do something for you in return for a small favor?"
    putStrLn "Gym Guy: You little man, what would you need help for?"
    putStrLn "You: To break the ventilation hole."
    putStrLn "Gym Guy: It's a piece of cake for me. Bring me a great meal cause I need to refill my carbs. Then I'll do the job."
    return $ setWaitingFor GymGuy world

talk GymGuy world
  | not (isQuestDone AllQuests GymGuy world) && isQuestDone MealQuest GymGuy world = do
    putStrLn "You: So, will you help me with your muscles?"
    putStrLn "Gym Guy: Yeah, the meal was great. Take me to the place."
    putStrLn "You and the Gym Guy went to the ventilation grid and broke it."
    return $ debugGo Hallway world $ addItemToInventory GreatMeal $ removePersonFromLocation GymGuy Gym world $ removeObjectFromLocation VentilationGrid Hallway world $ unlock Ventilation world

talk GymGuy world
  | isQuestDone AllQuests GymGuy = do
    putStrLn "You: Hi, I..."
    putStrLn "Gym Guy: Don't have time for you now, get lost."
    return world

talk GymGuy world = do
  putStrLn "There is no one named Gym Guy here."
  return world

talk ShoweringPrisoner world
  | not (isWaitingFor Towel world) && not (isQuestDone TowelQuest ShoweringPrisoner world) = do
    putStrLn "Prisoner: Hey what are you looking at?!"
    putStrLn "You: I was just.."
    putStrLn "Prisoner: Get out now!! Or actually, wait.. Bring me a towel!"
    putStrLn "You: Why would I?"
    putStrLn "Prisoner: You dare to ask?!"
    putStrLn "You: I'm not going to do this for free."
    putStrLn "Prisoner: Fine, if you decide to help me I'll give you something in return."
    return $ setWaitingFor ShoweringPrisoner world

talk ShoweringPrisoner world
  | not (isQuestDone AllQuests ShoweringPrisoner world) && isQuestDone TowelQuest ShoweringPrisoner world = do
    putStrLn "You: You received your towel. What about my reward?"
    putStrLn "Prisoner: Hmm... I don't have anything on me, but.."
    putStrLn "You: But what?!"
    putStrLn "Prisoner: Do you want some batteries?"
    putStrLn "You: Why would I need batteries?"
    putStrLn "Prisoner: I dunno, maybe to power up a Flashlight or something.."
    putStrLn "You: Hmmm... Okay, give me those batteries!"
    putStrLn "Prisoner: You can find them under a pillow in cell 3."
    return $ addObjectToLocation Pillow Cell3 world $ setQuestDone AllQuests ShoweringPrisoner world

talk ShoweringPrisoner world
  | isQuestDone AllQuests ShoweringPrisoner = do
    putStrLn "You: Hi, I..."
    putStrLn "Prisoner: Don't have time for you now, get lost."
    return world

talk ShoweringPrisoner world = do
  putStrLn "There is no one named Showering Prisoner here."
  return world

talk Chef world
  | not (isWaitingFor Coffee world) && not (isQuestDone CoffeeQuest Chef world) = do
    putStrLn "You: Hi! I've heard that you're the best chef in here. Could you make me your signature meal?"
    putStrLn "Chef: Nice words won't be enough. I'am actually pretty tired, if you could bring me some coffee then I'll cook something."
    putStrLn "You: I should have some in my cell, I'll be in a moment."
    return $ addItemToObject Coffee Table world $ setWaitingFor Chef world

talk Chef world
  | not (isQuestDone AllQuests Chef world) && isQuestDone CoffeeQuest Chef world = do
    putStrLn "You: Now you're quite caffenaited, aren't you?"
    putStrLn "Chef: Yeah, thanks. I'll cook something quickly."
    putStrLn "After few minutes chef hands you a hot meal."
    return $ addItemToInventory GreatMeal $ setQuestDone AllQuests Chef world

talk Chef world
  | isQuestDone AllQuests Chef = do
    putStrLn "You: Hi, I..."
    putStrLn "Chef: Don't have time for you now, get lost."
    return world

talk Chef world = do
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
  | thereIsPersonInLocation OldMan getLocation world && count >= 5 = do
    let newCount = count - 5
        updatedCigarettes = Cigarettes newCount
        quest = if not (isQuestDone Quest1 OldMan world) then Quest1 else Quest2
    putStrLn "Old Man: Ah, you've brought the cigarettes. Good."
    putStrLn "You hand the cigarettes to the Old Man."
    return $ removeItem (Cigarettes count) $ addItemToInventory updatedCigarettes $
          setQuestDone quest OldMan world

give (Cigarettes _) OldMan world
  | thereIsPersonInLocation OldMan getLocation world = do
    putStrLn "Old Man: You don't have enough cigarettes."
    return world

give item OldMan world
  | thereIsPersonInLocation OldMan getLocation world && item `elem` getInventory world = do
    putStrLn "Old Man: I don't want that item."
    return world

give (GreatMeal) GymGuy world
  | thereIsPersonInLocation GymGuy getLocation world && GreatMeal `elem` getInventory world = do
    putStrLn "Gym Guy: Just on time, I'm hungry as hell."
    putStrLn "You hand the meal to the Gym Guy."
    return $ removeItem GreatMeal $ setQuestDone MealQuest GymGuy world

give item GymGuy world
  | thereIsPersonInLocation GymGuy getLocation world && item `elem` getInventory world = do
    putStrLn "Gym Guy: I don't want that item."
    return world

give _ GymGuy world
  | thereIsPersonInLocation GymGuy getLocation world = do
    putStrLn "Gym Guy: You don't have the meal."
    return world

give (Coffee) Chef world
  | thereIsPersonInLocation Chef getLocation world && Coffee `elem` getInventory world = do
    putStrLn "Chef: Oh, you have the coffee. I need a boost of energy."
    putStrLn "You hand the coffee to the Chef."
    return $ removeItem Coffee $ setQuestDone CoffeeQuest Chef world

give item Chef world
  | thereIsPersonInLocation Chef getLocation world && item `elem` getInventory world = do
    putStrLn "Chef: I don't want that item."
    return world

give _ Chef world
  | thereIsPersonInLocation Chef getLocation world = do
    putStrLn "Chef: You don't have the coffee."
    return world

give (Towel) ShoweringPrisoner world
  | thereIsPersonInLocation ShoweringPrisoner getLocation world && Towel `elem` getInventory world = do
    putStrLn "Prisoner: Oh, there you are. Gimmie the towel."
    putStrLn "You hand the towel to the Showering Prisoner."
    return $ removeItem Towel $ setQuestDone TowelQuest ShoweringPrisoner world

give item ShoweringPrisoner world
  | thereIsPersonInLocation ShoweringPrisoner getLocation world && item `elem` getInventory world = do
    putStrLn "Prisoner: I don't want that item."
    return world

give _ ShoweringPrisoner world
  | thereIsPersonInLocation ShoweringPrisoner getLocation world = do
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