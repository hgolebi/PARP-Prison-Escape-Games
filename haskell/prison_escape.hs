import Data.IORef
import Control.Monad (forM_)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (listToMaybe)


-- Dynamic facts
data DynamicFact = At Location | ThereIsOL Object Location | ThereIsIO Item Object | ThereIsPL Person Location
  | QuestDone Item Location | Cigarettes Item | Locked Location | Distracted Person | Done Quest Person
  | Waiting Person | Borders Location Location
 
-- type World = IORef [DynamicFact]
type World = IORef ([DynamicFact], M.Map Item Int)  -- contains facts and map of items and their quantity (inventory)

newWorld :: IO World
newWorld = newIORef ([], M.empty)

-- Quests definition
data Quest = Quest1 | Quest2 | AllQuests | MealQuest | TowelQuest | CoffeeQuest
  deriving (Eq, Show)

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
  let facts = map (\(location1, location2) -> Borders location1 location2) bordersInMap
  modifyIORef world (\(facts', items) -> (facts' ++ facts, items))

initialLockedLocations :: [Location]
initialLockedLocations =
  [ Cell1
  , Hallway
  , Ventilation
  , WayToFreedomLightsTurnedOff
  ]

initializeLockedLocations :: World -> IO ()
initializeLockedLocations world = do
  let facts = map (\fact -> Locked fact) initialLockedLocations
  modifyIORef world (\(facts', items) -> (facts ++ facts', items))


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
  let facts = map (\(object, location) -> ThereIsOL object location) objectsInRooms
  modifyIORef world (\(facts', items) -> (facts ++ facts', items))


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
  modifyIORef world (\(facts', items) -> (facts ++ facts', items))


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
  , (Cigarette, OccupiedBed)
  , (Cigarette, OccupiedBed2)
  , (Cigarette, Coat)
  , (Cigarette, Desk)
  , (Cigarette, Fridge)
  , (Cigarette, Treadmill)
  , (Cigarette, Cabinet)
  , (Cigarette, Bench)
  , (PlayboyMagazine, Shelf)
  , (Flashlight, Desk)
  , (Cell1Key, Coat)
  , (Towel, Bench)
  , (Batteries, Pillow)
  ]

initializeItemLocations :: World -> IO ()
initializeItemLocations world = do
  let facts = map (\(item, object) -> ThereIsIO item object) itemLocations
  modifyIORef world (\(facts', items) -> (facts ++ facts', items))

-- Starting in cell2
initializeStartingLocation :: World -> IO ()
initializeStartingLocation world = do
  (facts, items) <- readIORef world
  let newFacts = (At Cell2) : facts
  writeIORef world (newFacts, items)


instance Eq DynamicFact where
  (At loc1) == (At loc2) = loc1 == loc2
  (ThereIsOL obj1 loc1) == (ThereIsOL obj2 loc2) = obj1 == obj2 && loc1 == loc2
  (ThereIsIO item1 obj1) == (ThereIsIO item2 obj2) = item1 == item2 && obj1 == obj2
  (ThereIsPL person1 loc1) == (ThereIsPL person2 loc2) = person1 == person2 && loc1 == loc2
  (QuestDone item1 loc1) == (QuestDone item2 loc2) = item1 == item2 && loc1 == loc2
  (Cigarettes item1) == (Cigarettes item2) = item1 == item2
  (Locked loc1) == (Locked loc2) = loc1 == loc2
  (Distracted person1) == (Distracted person2) = person1 == person2
  (Done quest1 person1) == (Done quest2 person2) = quest1 == quest2 && person1 == person2
  (Waiting person1) == (Waiting person2) = person1 == person2
  (Borders loc1 loc2) == (Borders loc3 loc4) = (loc1 == loc3 && loc2 == loc4) || (loc1 == loc4 && loc2 == loc3)
  _ == _ = False

instance Show DynamicFact where
  show (At location) = "At " ++ show location

instance Ord Item where
  compare item1 item2 = compare (show item1) (show item2)


-- Rule: get location
getLocation :: World -> IO Location
getLocation world = do
  (facts, _) <- readIORef world
  case find isAtFact facts of
    Just (At location) -> return location
    _ -> error "Location not found"

isAtFact :: DynamicFact -> Bool
isAtFact fact = case fact of
  At _ -> True
  _ -> False

-- Rule: check if location is locked
isLocked :: Location -> World -> IO Bool
isLocked location world = do
  (facts, _) <- readIORef world
  return $ (Locked location) `elem` facts

-- getting items from inventory
getInventory :: World -> IO [Item]
getInventory world = do
  (_, items) <- readIORef world
  return (M.keys items)


-- getting items and their number from inventory
getInventoryItemCounts :: World -> IO [(Item, Int)]
getInventoryItemCounts world = do
  (_, items) <- readIORef world
  return (M.toList items)

-- Rule: check if there is a certain item in inventory
hasItem :: Item -> World -> IO Bool
hasItem item world = do
  items <- getInventory world
  return (item `elem` items)

-- adding item to the inventory
-- addItemToInventory :: Item -> World -> IO ()
-- addItemToInventory item world = modifyIORef world (\(facts, items) -> (facts, item : items))

-- Dodawanie przedmiotu do mapy z licznikiem
addItemToInventory :: Item -> World -> IO ()
addItemToInventory item world = modifyIORef world (\(facts, items) -> (facts, incrementItem item items))

-- Zwiększanie liczby przedmiotów w mapie
incrementItem :: Item -> M.Map Item Int -> M.Map Item Int
incrementItem item items = M.insertWith (+) item 1 items


-- removing item from inventory
-- removeItem :: Item -> World -> IO ()
-- removeItem item world = modifyIORef world (\(facts, items) -> (facts, filter (/= item) items))

removeItemFromInventory :: Item -> Int -> World -> IO ()
removeItemFromInventory item amount world =
  modifyIORef world (\(facts, items) -> (facts, M.update (subtractCount amount) item items))
  where
    subtractCount :: Int -> Int -> Maybe Int
    subtractCount amount count
      | count <= amount = Nothing  -- Usunięcie przedmiotu z mapy, jeśli ilość jest mniejsza lub równa żądanej liczbie
      | otherwise = Just (count - amount)  -- Zmniejszenie liczby przedmiotów o żądaną wartość


-- checking whether there is certain number of specified item in inventory
hasItemCountInInventory :: Item -> Int -> World -> IO Bool
hasItemCountInInventory item count world = do
  (_, items) <- readIORef world
  case M.lookup item items of
    Just itemCount -> return (itemCount >= count)
    Nothing -> return False

-- getting item count from inventory
--getItemCount :: Item -> World -> IO (Maybe Int)
--getItemCount item world = do
--  (_, items) <- readIORef world
--  return (M.lookup item items)

getItemCount :: Item -> World -> IO Int
getItemCount item world = do
  (_, items) <- readIORef world
  return $ case M.findWithDefault 0 item items of
    count -> count



-- adding object to location
addObjectToLocation :: Object -> Location -> World -> IO ()
addObjectToLocation object location world = do
  modifyIORef world (\(facts, items) -> (ThereIsOL object location : facts, items))

-- adding item to object
addItemToObject :: Item -> Object -> World -> IO ()
addItemToObject item object world = do
  modifyIORef world (\(facts, items) -> (ThereIsIO item object : facts, items))

-- Sprawdza, czy w danej lokalizacji znajduje się dana osoba
thereIsPersonInLocation :: Person -> Location -> World -> IO Bool
thereIsPersonInLocation person location world = do
  (facts, _) <- readIORef world
  return $ (ThereIsPL person location) `elem` facts

-- Sprawdza, czy w danym obiekcie znajduje się dany przedmiot
thereIsItemInObject :: Item -> Object -> World -> IO Bool
thereIsItemInObject item object world = do
  (facts, _) <- readIORef world
  return $ (ThereIsIO item object) `elem` facts

-- Sprawdza, czy dany obiekt znajduje się w danej lokalizacji
thereIsObjectInLocation :: Object -> Location -> World -> IO Bool
thereIsObjectInLocation object location world = do
  (facts, _) <- readIORef world
  return $ (ThereIsOL object location) `elem` facts

-- checking if certain quest is done
-- isQuestDone :: Quest -> Person -> World -> Bool
-- isQuestDone quest person world =
--   case readIORef world of
--     (facts, _) -> any (\fact -> case fact of { Done q p -> q == quest && p == person; _ -> False }) facts

isQuestDone :: Quest -> Person -> World -> IO Bool
isQuestDone quest person world = do
  (facts, _) <- readIORef world
  return $ (Done quest person) `elem` facts

-- adding fact Done Quest
setQuestDone :: Quest -> Person -> World -> IO ()
setQuestDone quest person world = do
  modifyIORef world (\(facts, items) -> (Done quest person : facts, items))

-- checking if certain person is waiting
isWaitingFor :: Person -> World -> IO Bool
isWaitingFor person world = do
  (facts, _) <- readIORef world
  return $ Waiting person `elem` facts

-- adds a fact that someone is waiting for
setWaitingFor :: Person -> World -> IO ()
setWaitingFor person world = do
  modifyIORef world (\(facts, items) -> (Waiting person : facts, items))

-- adds a fact that some location borders with other
setBorder :: Location -> Location -> World -> IO ()
setBorder location1 location2 world = do
  modifyIORef world (\(facts, items) -> (Borders location1 location2 : facts, items))

-- checking if one location borders with other
doesBorder :: Location -> Location -> World -> IO Bool
doesBorder location1 location2 world = do
  (facts, _) <- readIORef world
  return $ (Borders location1 location2) `elem` facts


-- Rule: Look
look :: World -> IO ()
look world = do
  currentLocation <- getLocation world
  putStrLn $ "You're currently at " ++ show currentLocation
  putStrLn "You can see:"
  listObjects currentLocation world
  putStrLn "You can go to:"
  availableDestinations currentLocation world


listObjects :: Location -> World -> IO ()
listObjects place world = do
  (facts, _) <- readIORef world
  let objects = filter (\fact -> case fact of { ThereIsOL _ loc -> loc == place; _ -> False }) facts
  putStrLn $ intercalate "\n" $ map (\obj -> "* " ++ show obj) objects


availableDestinations :: Location -> World -> IO ()
availableDestinations place world = do
  (facts, _) <- readIORef world
  let destinations = filter (\fact -> case fact of { At src -> src == place; _ -> False }) facts
  putStrLn $ intercalate "\n" $ map (\dest -> "-> " ++ show dest) destinations


-- Rules: Go to guard_room, Go to ventilation, Go to other destinations
go :: Location -> World -> IO ()
go location world = do
  (facts, items) <- readIORef world
  currentLocation <- getLocation world
  flashlightHeld <- hasItem Flashlight world
  batteriesHeld <- hasItem Batteries world
  borderExists <- doesBorder location currentLocation world
  locationLocked <- isLocked location world
  let guardDistracted = Distracted Guard `elem` facts
      newFacts = filter (\fact -> case fact of { At _ -> False; _ -> True }) facts ++ [At location]

  case (location, currentLocation) of
    (Hallway, GuardRoom) -> do
      if borderExists && not guardDistracted then do
        putStrLn "Oh no, there is a guard!"
        putStrLn "I probably should've distracted him first."
        gameOver
      else do
        putStrLn "You can't go there!"

    (Ventilation, Hallway) -> do
      if borderExists && not locationLocked then do
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
      if borderExists && not locationLocked then do
        writeIORef world (newFacts, items)
        look world
      else if borderExists && locationLocked then
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
  let updatedFacts = filter (\fact -> case fact of { Locked l -> l /= location; _ -> True }) facts
  writeIORef world (updatedFacts, items)

unlock :: Location -> World -> IO ()
unlock location world = do
  locationLocked <- isLocked location world
  hasCell1Key <- hasItem Cell1Key world
  hasCell2Key <- hasItem Cell2Key world
  currentLocation <- getLocation world
  case (currentLocation, location) of
    (place, _) | not locationLocked -> putStrLn "To miejsce jest już odblokowane."
    (hallway, Cell1) | locationLocked && hasCell1Key -> do
      unlockLocation Cell1 world
      putStrLn "Użyto klucza do odblokowania Cell1."
    (hallway, Cell1) | locationLocked && not hasCell1Key -> putStrLn "Potrzebujesz klucza, aby odblokować tę celę."
    (cell2, Hallway) | locationLocked && hasCell2Key -> do
      unlockLocation Hallway world
      putStrLn "Użyto klucza do odblokowania Hallway."
    (cell2, Hallway) | locationLocked && not hasCell2Key -> putStrLn "Potrzebujesz klucza, aby odblokować te drzwi."
    (hallway, Ventilation) | locationLocked -> putStrLn "Jestem za słaby, żeby to odblokować. Może ktoś silny mi pomoże."
    _ -> putStrLn ("Nie można odblokować " ++ show location ++ ".")
  putStrLn ""

-- Ucieczka z więzienia
escape :: World -> IO ()
escape world = do
  escapeLocked <- isLocked WayToFreedomLightsTurnedOff world
  currentLocation <- getLocation world
  case currentLocation of
    PrisonYard | not escapeLocked -> do
      putStrLn "Było całkowicie ciemno i udało ci się uciec z więzienia!"
      putStrLn "Gratulacje, wygrałeś grę!"
      finish
    PrisonYard | escapeLocked -> do
      putStrLn "Wszyscy strażnicy zobaczyli twoje ruchy, gdy światła zostały włączone."
      gameOver
    _ -> putStrLn "Ha ha ha, nie tak szybko... ucieczka nie będzie taka łatwa."
  putStrLn ""

-- Wyłączanie bezpieczników
blowFuses :: World -> IO ()
blowFuses world = do
  currentLocation <- getLocation world
  if currentLocation == Shed
    then do
      unlockLocation WayToFreedomLightsTurnedOff world
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
  objectItems <- getObjectItems object world
  case objectItems of
    [] -> putStrLn "Nic do odkrycia tutaj."
    _ -> do
      putStrLn $ "W " ++ show object ++ " znajdujesz:"
      mapM_ (\item -> putStrLn $ "* " ++ show item) objectItems
  putStrLn ""


-- Zwraca listę przedmiotów znajdujących się w danym obiekcie
getObjectItems :: Object -> World -> IO [Item]
getObjectItems object world = do
  (facts, _) <- readIORef world
  let objectItems = [item | ThereIsIO item obj <- facts, obj == object]
  return objectItems


-- Podniesienie obiektu
take :: Item -> Object -> World -> IO()
take Flashlight object world = do
  currentLocation <- getLocation world
  objectExistsInLocation <- thereIsObjectInLocation object currentLocation world
  flashlightExistsInObject <- thereIsItemInObject Flashlight object world
  if objectExistsInLocation && flashlightExistsInObject
    then do
      removeItemFromObject Flashlight object world
      putStrLn "Podniosłeś latarkę. Wygląda na to, że potrzebuje baterii, aby działać."
      addItemToInventory Flashlight world
    else do
      putStrLn "Nie widzę tego tutaj."


take PlayboyMagazine object world = do
  currentLocation <- getLocation world
  objectExistsInLocation <- thereIsObjectInLocation object currentLocation world
  playboyMagazineExistsInObject <- thereIsItemInObject PlayboyMagazine object world
  if objectExistsInLocation && playboyMagazineExistsInObject
    then do
      removeItemFromObject PlayboyMagazine object world
      putStrLn "Ty: Fajny magazyn, może będę mógł go użyć, żeby rozproszyć strażnika."
      putStrLn "Wpisz 'leave(playboy_magazine, Miejsce).' aby zostawić magazyn w jakimś miejscu."
      putStrLn "Musisz być w sąsiednim pomieszczeniu."
      addItemToInventory PlayboyMagazine world
    else do
      putStrLn "Nie widzę tego tutaj."

take Towel object world = do
  currentLocation <- getLocation world
  objectExistsInLocation <- thereIsObjectInLocation object currentLocation world
  towelExistsInObject <- thereIsItemInObject Towel object world
  personExistsInLocation <- thereIsPersonInLocation GymGuy Gym world
  if currentLocation == Gym && objectExistsInLocation && towelExistsInObject
    then do
      removeItemFromObject Towel object world
      putStrLn "Facet na siłowni: Hej, co ty tam robisz?! To moje ręcznik!"
      putStrLn "Ty: Czy mogę go pożyczyć?"
      putStrLn "Facet na siłowni: Zapomnij. Potrzebuję go do treningu."
    else if currentLocation == Gym && objectExistsInLocation && not personExistsInLocation
      then do
        removeItemFromObject Towel object world
        putStrLn "OK."
        addItemToInventory Towel world
      else do
        putStrLn "Nie widzę tego tutaj."


take item object world = do
  currentLocation <- getLocation world
  objectExistsInLocation <- thereIsObjectInLocation object currentLocation world
  itemExistsInObject <- thereIsItemInObject item object world
  if objectExistsInLocation && itemExistsInObject
    then do
      removeItemFromObject item object world
      putStrLn "OK."
      addItemToInventory item world
    else do
      putStrLn "Nie widzę tego tutaj."


-- Usuwa przedmiot z obiektu


removeItemFromObject :: Item -> Object -> World -> IO ()
removeItemFromObject item object world = do
  modifyIORef world (\(facts, items) -> (removeItemFact item object facts, removeFromItems item items))

removeFromItems :: Item -> M.Map Item Int -> M.Map Item Int
removeFromItems item items = M.filterWithKey (\k _ -> k /= item) items

removeItemFact :: Item -> Object -> [DynamicFact] -> [DynamicFact]
removeItemFact item object facts =
  filter (\fact -> case fact of { ThereIsIO i o -> i /= item || o /= object; _ -> True }) facts



-- Odkładanie obiektu
leave :: Item -> Location -> World -> IO ()
leave PlayboyMagazine GuardRoom world = do
  magazineHeld <- hasItem PlayboyMagazine world
  currentLocation <- getLocation world
  if magazineHeld && currentLocation == Hallway
    then do
      removeItemFromInventory PlayboyMagazine 1 world
      putStrLn "Zostawiłeś magazyn w pokoju strażnika. Wygląda na to, że strażnik jest rozproszony ;)"
      setDistractedPerson Guard world
    else do
      putStrLn "Prawdopodobnie nie powinienem zostawiać tego tam."

leave PlayboyMagazine destination world = do
  currentLocation <- getLocation world
  doesLocationsBorder <- doesBorder destination currentLocation world
  if currentLocation /= destination || not doesLocationsBorder
    then putStrLn "Jesteś zbyt daleko."
    else putStrLn "Nie masz tego przedmiotu."

leave _ _ _ = putStrLn "Nie masz tego przedmiotu."

-- Dodaje atrybut "distracted" do osoby
setDistractedPerson :: Person -> World -> IO ()
setDistractedPerson person world = do
  modifyIORef world (\(facts, items) -> (Distracted person : facts, items))

-- removing person from location
removePersonFromLocation :: Person -> Location -> World -> IO ()
removePersonFromLocation person location world = do
  modifyIORef world (\(facts, items) -> (filter (\fact -> case fact of { ThereIsPL p l -> p /= person || l /= location; _ -> True }) facts, items))

-- removing object from location
removeObjectFromLocation :: Object -> Location -> World -> IO ()
removeObjectFromLocation object location world = do
  modifyIORef world (\(facts, items) -> (filter (\fact -> case fact of { ThereIsOL o l -> o /= object || l /= location; _ -> True }) facts, items))


-- Rozmowa z postacią
talk :: Person -> World -> IO ()
talk SleepingGuy world = do
  putStrLn "Sleeping guy: Zzzzz..."
  return ()

talk SleepingGuy2 world = do
  putStrLn "Sleeping guy: Zzzzzzzz..."
  return ()

talk Guard world = do
  putStrLn "Guard: Wait, what are you doing here?!"
  gameOver
  return ()

talk OldMan world = do
  isQuest1Done <- isQuestDone Quest1 OldMan world
  isQuest2Done <- isQuestDone Quest2 OldMan world
  areAllQuestsDone <- isQuestDone AllQuests OldMan world
  hasCell2Key <- hasItem Cell2Key world
  isWaitingForCigarettes <- isWaitingFor OldMan world
  if not isWaitingForCigarettes && isQuest1Done && not isQuest2Done
    then do
      putStrLn "You: Okey, could you give me some advice now?"
      putStrLn "Old Man: Alright. There is a hole in the wall by the 16th pole on a prison yard."
      putStrLn "You: But wait, the lights are on, everything will be visible."
      putStrLn "Old Man: I don't give free information. Bring 5 more cigarettes."
      setWaitingFor OldMan world
    else if not areAllQuestsDone && isQuest2Done
      then do
        putStrLn "You: What about the light?"
        putStrLn "Old Man: You can break the ventilation hole in the hallway and get into the room with fuses, where you turn off the light."
        putStrLn "You: Holy Chicken Trolley, that's my opportunity!!"
        setBorder Hallway Ventilation world
      else if areAllQuestsDone
        then do
          putStrLn "You: Hi, I..."
          putStrLn "Old Man: Don't have time for you now, get lost."
        else do
          putStrLn "There is no one named Old Man here."

talk GymGuy world = do
  isWaitingForMeal <- isWaitingFor GymGuy world
  isQuest2Done <- isQuestDone Quest2 OldMan world
  isMealQuestDone <- isQuestDone MealQuest GymGuy world
  areAllQuestsDone <- isQuestDone AllQuests GymGuy world
  if not isWaitingForMeal && isQuest2Done && not isMealQuestDone
    then do
      putStrLn "You see a strong guy that is exhausted after his training."
      putStrLn "You: Hey! I have a case. Could I do something for you in return for a small favor?"
      putStrLn "Gym Guy: You little man, what would you need help for?"
      putStrLn "You: To break the ventilation hole."
      putStrLn "Gym Guy: It's a piece of cake for me. Bring me a great meal cause I need to refill my carbs. Then I'll do the job."
      setWaitingFor GymGuy world
    else if not areAllQuestsDone && isMealQuestDone
      then do
        putStrLn "You: So, will you help me with your muscles?"
        putStrLn "Gym Guy: Yeah, the meal was great. Take me to the place."
        putStrLn "You and the Gym Guy went to the ventilation grid and broke it."
        debugGo Hallway world
        addItemToInventory GreatMeal world
        removePersonFromLocation GymGuy Gym world
        removeObjectFromLocation VentilationGrid Hallway world
        unlock Ventilation world
      else if areAllQuestsDone
        then do
          putStrLn "You: Hi, I..."
          putStrLn "Gym Guy: Don't have time for you now, get lost."
        else do
          putStrLn "There is no one named Gym Guy here."

talk ShoweringPrisoner world = do
  isWaitingForTowel <- isWaitingFor ShoweringPrisoner world
  isTowelQuestDone <- isQuestDone TowelQuest ShoweringPrisoner world
  areAllQuestsDone <- isQuestDone AllQuests ShoweringPrisoner world
  if not isWaitingForTowel && not isTowelQuestDone
    then do
      putStrLn "Prisoner: Hey what are you looking at?!"
      putStrLn "You: I was just.."
      putStrLn "Prisoner: Get out now!! Or actually, wait.. Bring me a towel!"
      putStrLn "You: Why would I?"
      putStrLn "Prisoner: You dare to ask?!"
      putStrLn "You: I'm not going to do this for free."
      putStrLn "Prisoner: Fine, if you decide to help me I'll give you something in return."
      setWaitingFor ShoweringPrisoner world
    else if not areAllQuestsDone && isTowelQuestDone
      then do
        putStrLn "You: You received your towel. What about my reward?"
        putStrLn "Prisoner: Hmm... I don't have anything on me, but.."
        putStrLn "You: But what?!"
        putStrLn "Prisoner: Do you want some batteries?"
        putStrLn "You: Why would I need batteries?"
        putStrLn "Prisoner: I dunno, maybe to power up a Flashlight or something.."
        putStrLn "You: Hmmm... Okay, give me those batteries!"
        putStrLn "Prisoner: You can find them under a pillow in cell 3."
        addObjectToLocation Pillow Cell3 world
        setQuestDone AllQuests ShoweringPrisoner world
      else if areAllQuestsDone
        then do
          putStrLn "You: Hi, I..."
          putStrLn "Prisoner: Don't have time for you now, get lost."
        else do
          putStrLn "There is no one named Showering Prisoner here."

talk Chef world = do
  isWaitingForCoffee <- isWaitingFor Chef world
  isCoffeeQuestDone <- isQuestDone CoffeeQuest Chef world
  areAllQuestsDone <- isQuestDone AllQuests Chef world
  if not isWaitingForCoffee && not isCoffeeQuestDone
    then do
      putStrLn "You: Hi! I've heard that you're the best chef in here. Could you make me your signature meal?"
      putStrLn "Chef: Nice words won't be enough. I'am actually pretty tired, if you could bring me some coffee then I'll cook something."
      putStrLn "You: I should have some in my cell, I'll be in a moment."
      addItemToObject Coffee Table world
      setWaitingFor Chef world
    else if not areAllQuestsDone && isCoffeeQuestDone
      then do
        putStrLn "You: Now you're quite caffenaited, aren't you?"
        putStrLn "Chef: Yeah, thanks. I'll cook something quickly."
        putStrLn "After few minutes chef hands you a hot meal."
        addItemToInventory GreatMeal world
        setQuestDone AllQuests Chef world
      else if areAllQuestsDone
        then do
          putStrLn "You: Hi, I..."
          putStrLn "Chef: Don't have time for you now, get lost."
        else do
          putStrLn "There is no one named Chef here."

talk person world = do
  putStrLn $ "There is no one named " ++ show person ++ " here."

-- giving items to people
-- giving items to people
give :: Item -> Person -> World -> IO ()
give Cigarette _ world = do
  putStrLn "To give someone cigarettes, type 'give Cigarettes Person.'"

give Cigarette OldMan world = do
  currentLocation <- getLocation world
  isOldManHere <- thereIsPersonInLocation OldMan currentLocation world
  hasEnoughCigarettes <- hasItemCountInInventory Cigarette 5 world
  isQuest1Done <- isQuestDone Quest1 OldMan world
  if isOldManHere && hasEnoughCigarettes
    then do
      removeItemFromInventory Cigarette 5 world
      let quest = if not isQuest1Done then Quest1 else Quest2
      putStrLn "Old Man: Ah, you've brought the cigarettes. Good."
      putStrLn "You hand the cigarettes to the Old Man."
      setQuestDone quest OldMan world
    else do
      putStrLn "Old Man: You don't have enough cigarettes."

give item OldMan world = do
  currentLocation <- getLocation world
  isOldManHere <- thereIsPersonInLocation OldMan currentLocation world
  hasItemInInventory <- hasItem item world
  if isOldManHere && hasItemInInventory
    then putStrLn "Old Man: I don't want that item."
    else pure ()

give GreatMeal GymGuy world = do
  currentLocation <- getLocation world
  isGymGuyHere <- thereIsPersonInLocation GymGuy currentLocation world
  hasGreatMeal <- hasItem GreatMeal world
  if isGymGuyHere && hasGreatMeal
    then do
      putStrLn "Gym Guy: Just on time, I'm hungry as hell."
      putStrLn "You hand the meal to the Gym Guy."
      removeItemFromInventory GreatMeal 1 world
      setQuestDone MealQuest GymGuy world
    else pure ()

give item GymGuy world = do
  currentLocation <- getLocation world
  isGymGuyHere <- thereIsPersonInLocation GymGuy currentLocation world
  hasItemInInventory <- hasItem item world
  if isGymGuyHere && hasItemInInventory
    then putStrLn "Gym Guy: I don't want that item."
    else pure ()

give _ GymGuy world = do
  currentLocation <- getLocation world
  isGymGuyHere <- thereIsPersonInLocation GymGuy currentLocation world
  if isGymGuyHere
    then putStrLn "Gym Guy: You don't have the meal."
    else pure ()

give Coffee Chef world = do
  currentLocation <- getLocation world
  isChefHere <- thereIsPersonInLocation Chef currentLocation world
  hasCoffee <- hasItem Coffee world
  if isChefHere && hasCoffee
    then do
      putStrLn "Chef: Oh, you have the coffee. I need a boost of energy."
      putStrLn "You hand the coffee to the Chef."
      removeItemFromInventory Coffee 1 world
      setQuestDone CoffeeQuest Chef world
    else pure ()

give item Chef world = do
  currentLocation <- getLocation world
  isChefHere <- thereIsPersonInLocation Chef currentLocation world
  hasItemInInventory <- hasItem item world
  if isChefHere && hasItemInInventory
    then putStrLn "Chef: I don't want that item."
    else pure ()

give _ Chef world = do
  currentLocation <- getLocation world
  isChefHere <- thereIsPersonInLocation Chef currentLocation world
  if isChefHere
    then putStrLn "Chef: You don't have the coffee."
    else pure ()

give Towel ShoweringPrisoner world = do
  currentLocation <- getLocation world
  isPrisonerHere <- thereIsPersonInLocation ShoweringPrisoner currentLocation world
  hasTowel <- hasItem Towel world
  if isPrisonerHere && hasTowel
    then do
      putStrLn "Prisoner: Oh, there you are. Gimmie the towel."
      putStrLn "You hand the towel to the Showering Prisoner."
      removeItemFromInventory Towel 1 world
      setQuestDone TowelQuest ShoweringPrisoner world
    else pure ()

give item ShoweringPrisoner world = do
  currentLocation <- getLocation world
  isPrisonerHere <- thereIsPersonInLocation ShoweringPrisoner currentLocation world
  hasItemInInventory <- hasItem item world
  if isPrisonerHere && hasItemInInventory
    then putStrLn "Prisoner: I don't want that item."
    else pure ()

give _ ShoweringPrisoner world = do
  currentLocation <- getLocation world
  isPrisonerHere <- thereIsPersonInLocation ShoweringPrisoner currentLocation world
  if isPrisonerHere
    then putStrLn "Prisoner: You don't have the towel."
    else pure ()

give _ _ world =
  putStrLn "This person doesn't want that item."


-- This rule describes how to display inventory's contents and how the number of picked up cigarettes increases
-- import Control.Monad (forM_)

listItems :: World -> IO ()
listItems world = do
  items <- getInventory world
  forM_ items $ \item -> putStrLn $ "* " ++ show item


--These rules are responsible for finishing the game

gameOver :: IO ()
gameOver = do
  putStrLn "YOU GOT CAUGHT!"
  putStrLn "Maybe try your luck again from the beginning."

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
  putStrLn "halt                  -- to end the game and quit."
  putStrLn ""


main :: IO ()
main = do
  worldRef <- newWorld  -- Inicjalizacja stanu gry
  putStrLn "Welcome to the Prison Escape Game!"
  putStrLn "You find yourself in a dark prison cell."
--  gameLoop worldRef

--gameLoop :: World -> IO ()
--gameLoop worldRef = do
--  putStrLn "\nWhat do you want to do?"
--  command <- getLine
--  executeCommand command worldRef
--  gameLoop worldRef

--executeCommand :: String -> World -> IO ()
--executeCommand command worldRef
--  | "look" `isPrefixOf` command = do
--      location <- getLocation worldRef
--      describeLocation location
--  | "go" `isPrefixOf` command = do
--      let destination = extractDestination command
--      movePlayerTo destination worldRef
--  | "take" `isPrefixOf` command = do
--      let item = extractItem command
--      takeItem item worldRef
--  | "use" `isPrefixOf` command = do
--      let item = extractItem command
--      useItem item worldRef
--  | "give" `isPrefixOf` command = do
--     let item = extractItem command
--          person = extractPerson command