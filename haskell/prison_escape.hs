import Data.IORef
import Control.Monad (forM_)
import System.Exit (exitSuccess)
import Data.List (intercalate)
import Data.List (find)
import Data.List (isPrefixOf)
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
  | Shed | PrisonYard | WayToFreedom
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
  , Ventilation
  , WayToFreedom
  , Hallway
  , Shed
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
  show (ThereIsOL obj loc) = "ThereIsOL " ++ show obj ++ " " ++ show loc
  --show (ThereIsOL obj loc) = show obj
  show (ThereIsIO item obj) = "ThereIsIO " ++ show item ++ " " ++ show obj
  show (ThereIsPL person loc) = "ThereIsPL " ++ show person ++ " " ++ show loc
  show (QuestDone item loc) = "QuestDone " ++ show item ++ " " ++ show loc
  show (Cigarettes item) = "Cigarettes " ++ show item
  show (Locked loc) = "Locked " ++ show loc
  show (Distracted person) = "Distracted " ++ show person
  show (Done quest person) = "Done " ++ show quest ++ " " ++ show person
  show (Waiting person) = "Waiting " ++ show person
  show (Borders loc1 loc2) = "Borders " ++ show loc1 ++ " " ++ show loc2

instance Ord Item where
  compare item1 item2 = compare (show item1) (show item2)


-- function: get location
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

-- function: check if location is locked
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

-- function: check if there is a certain item in inventory
hasItem :: Item -> World -> IO Bool
hasItem item world = do
  items <- getInventory world
  return (item `elem` items)

-- adding item to the inventory
addItemToInventory :: Item -> World -> IO ()
addItemToInventory item world = do
  (facts, items) <- readIORef world
  case M.lookup item items of
    Just count -> do
      let updatedCount = count + 1
      putStrLn $ "Existing item count: " ++ show count
      modifyIORef world (\(facts, items) -> (facts, M.insert item updatedCount items))
    Nothing -> do
      putStrLn "New item added."
      modifyIORef world (\(facts, items) -> (facts, M.insert item 1 items))




-- removing item from inventory
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

-- sprawdza, czy w danej lokalizacji znajduje się dana osoba
thereIsPersonInLocation :: Person -> Location -> World -> IO Bool
thereIsPersonInLocation person location world = do
  (facts, _) <- readIORef world
  return $ (ThereIsPL person location) `elem` facts

-- sprawdza, czy w danym obiekcie znajduje się dany przedmiot
thereIsItemInObject :: Item -> Object -> World -> IO Bool
thereIsItemInObject item object world = do
  (facts, _) <- readIORef world
  return $ (ThereIsIO item object) `elem` facts

-- sprawdza, czy dany obiekt znajduje się w danej lokalizacji
thereIsObjectInLocation :: Object -> Location -> World -> IO Bool
thereIsObjectInLocation object location world = do
  (facts, _) <- readIORef world
  return $ (ThereIsOL object location) `elem` facts

-- checks if a quest is done
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


-- function: Look
look :: World -> IO ()
look world = do
  currentLocation <- getLocation world
  putStrLn $ "You're currently at " ++ show currentLocation
  putStrLn "Objects you can see:"
  listObjects currentLocation world
  putStrLn "You can go to:"
  availableDestinations currentLocation world
  putStrLn "People you can see:"
  listPeople currentLocation world


listObjects :: Location -> World -> IO ()
listObjects place world = do
  (facts, _) <- readIORef world
  let objects = [obj | ThereIsOL obj loc <- facts, loc == place]
  putStrLn $ intercalate "\n" $ map (\obj -> "* " ++ show obj) objects


listPeople :: Location -> World -> IO ()
listPeople place world = do
  (facts, _) <- readIORef world
  let people = [person | ThereIsPL person loc <- facts, loc == place]
  putStrLn $ intercalate "\n" $ map (\person -> "* " ++ show person) people


availableDestinations :: Location -> World -> IO ()
availableDestinations place world = do
  (facts, _) <- readIORef world
  --let destinations = filter (\fact -> case fact of { At src -> src == place; _ -> False }) facts
  let destinations = [dest | Borders loc dest <- facts, loc == place]
  putStrLn $ intercalate "\n" $ map (\dest -> "-> " ++ show dest) destinations


-- functions: Go to guard_room, Go to ventilation, Go to other destinations
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

  case (currentLocation, location) of
    (Hallway, GuardRoom) -> do
      if borderExists && not guardDistracted then do
        putStrLn "You: Oh no, there is a guard!"
        putStrLn "You: I probably should've distracted him first."
        gameOver
      else if borderExists && guardDistracted then do
        writeIORef world (newFacts, items)
        look world
      else do
        putStrLn "You can't go there!"

    (Hallway, Ventilation) -> do
      if borderExists && not locationLocked then do
        if flashlightHeld && batteriesHeld then do
          writeIORef world (newFacts, items)
          unlockLocation Shed world
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
    (place, _) | not locationLocked -> putStrLn "This location is already unlocked."
    (hallway, Cell1) | locationLocked && hasCell1Key -> do
      unlockLocation Cell1 world
      putStrLn "Użyto klucza do odblokowania Cell1."
    (hallway, Cell1) | locationLocked && not hasCell1Key -> putStrLn "You need a key to unlock this cell."
    (cell2, Hallway) | locationLocked && hasCell2Key -> do
      unlockLocation Hallway world
      putStrLn "You used a key to unlock Hallway."
    (cell2, Hallway) | locationLocked && not hasCell2Key -> putStrLn "You need a key to unlock this door."
    (hallway, Ventilation) | locationLocked -> putStrLn "I'm too weak to break it. Maybe someone strong will help me."
    _ -> putStrLn ("Nie można odblokować " ++ show location ++ ".")
  putStrLn ""

-- escape from prison
escape :: World -> IO ()
escape world = do
  escapeLocked <- isLocked WayToFreedom world
  currentLocation <- getLocation world
  case currentLocation of
    PrisonYard | not escapeLocked -> do
      putStrLn "It was completely dark and you managed to escape from prison!"
      putStrLn "Congratulations, YOU WON THE GAME!"
      finish
    PrisonYard | escapeLocked -> do
      putStrLn "The lights were turned on so all of the guards saw your moves..."
      putStrLn "You have lost the game..."
      gameOver
    _ -> putStrLn "Ha ha ha, not so quick... escape won't be that easy."
  putStrLn ""

-- blowing the prison fuses
blowFuses :: World -> IO ()
blowFuses world = do
  currentLocation <- getLocation world
  if currentLocation == Shed
    then do
      unlockLocation WayToFreedom world
      putStrLn "You turned off the power in prison.."
    else putStrLn "You cannot turn off the power."
  putStrLn ""


-- objects investigation to find out what item do they contain
investigate :: Object -> World -> IO ()
investigate Pole16 world = do
  currentLocation <- getLocation world
  if currentLocation == PrisonYard
    then putStrLn "There is a hole in the wall just next to pole16. You can try to escape (type 'escape')."
    else putStrLn "Nothing to be found here."
  putStrLn ""

investigate FuseBox world = do
  currentLocation <- getLocation world
  if currentLocation == Shed
    then putStrLn "Inside the fuse box are switchers to cut off the light. You can try doing this (type 'blow fuses')."
    else putStrLn "Nothing to be found here."
  putStrLn ""

investigate object world = do
  objectItems <- getObjectItems object world
  if null objectItems
    then putStrLn "Nothing to be found here."
    else do
      putStrLn $ "In " ++ show object ++ " you can see:"
      mapM_ (\item -> putStrLn $ "* " ++ show item) objectItems


-- return list of items that are in the object
getObjectItems :: Object -> World -> IO [Item]
getObjectItems object world = do
  (facts, _) <- readIORef world
  let objectItems = [item | ThereIsIO item obj <- facts, obj == object]
  return objectItems


-- picking up an item
takeItem :: Item -> Object -> World -> IO()
takeItem Flashlight object world = do
  currentLocation <- getLocation world
  objectExistsInLocation <- thereIsObjectInLocation object currentLocation world
  flashlightExistsInObject <- thereIsItemInObject Flashlight object world
  if objectExistsInLocation && flashlightExistsInObject
    then do
      removeItemFromObject Flashlight object world
      putStrLn "You've picked up a flashlight. It seems like it needs batteries to work properly."
      addItemToInventory Flashlight world
    else do
      putStrLn "I dont't see it here."


takeItem PlayboyMagazine object world = do
  currentLocation <- getLocation world
  objectExistsInLocation <- thereIsObjectInLocation object currentLocation world
  playboyMagazineExistsInObject <- thereIsItemInObject PlayboyMagazine object world
  if objectExistsInLocation && playboyMagazineExistsInObject
    then do
      removeItemFromObject PlayboyMagazine object world
      putStrLn "You: Cool magazine, maybe I can use it to distract the guard."
      putStrLn "type 'leave magazine' to leave the magazine"
      putStrLn "You have to be in adjacent room."
      addItemToInventory PlayboyMagazine world
    else do
      putStrLn "I dont't see it here."

takeItem Towel object world = do
  currentLocation <- getLocation world
  isGymGuyHere <- thereIsPersonInLocation GymGuy currentLocation world
  objectExistsInLocation <- thereIsObjectInLocation object currentLocation world
  towelExistsInObject <- thereIsItemInObject Towel object world
  if currentLocation == Gym && objectExistsInLocation && towelExistsInObject && isGymGuyHere
    then do
      removeItemFromObject Towel object world
      putStrLn "Gym Guy: Hey what do you think you're doing?! That's my towel!"
      putStrLn "You: Can I borrow it?"
      putStrLn "Gym Guy: Forget it. I need it for my workout."
    else if currentLocation == Gym && objectExistsInLocation
      then do
        removeItemFromObject Towel object world
        putStrLn "OK."
        addItemToInventory Towel world
      else do
        putStrLn "I dont't see it here."


takeItem item object world = do
  currentLocation <- getLocation world
  objectExistsInLocation <- thereIsObjectInLocation object currentLocation world
  itemExistsInObject <- thereIsItemInObject item object world
  if objectExistsInLocation && itemExistsInObject
    then do
      removeItemFromObject item object world
      putStrLn "OK."
      addItemToInventory item world
    else do
      putStrLn "I dont't see it here."


-- removes item from object
removeItemFromObject :: Item -> Object -> World -> IO ()
removeItemFromObject item object world = do
  modifyIORef world (\(facts, items) -> (removeItemFact item object facts, removeFromItems item items))

removeFromItems :: Item -> M.Map Item Int -> M.Map Item Int
removeFromItems item items = M.filterWithKey (\k _ -> k /= item) items

removeItemFact :: Item -> Object -> [DynamicFact] -> [DynamicFact]
removeItemFact item object facts =
  filter (\fact -> case fact of { ThereIsIO i o -> i /= item || o /= object; _ -> True }) facts



-- putting down an item
leave :: Item -> Location -> World -> IO ()
leave PlayboyMagazine GuardRoom world = do
  magazineHeld <- hasItem PlayboyMagazine world
  currentLocation <- getLocation world
  if magazineHeld && currentLocation == Hallway
    then do
      removeItemFromInventory PlayboyMagazine 1 world
      putStrLn "You left a magazine in guard room. Looks like the guard is distracted."
      setDistractedPerson Guard world
    else if not magazineHeld
      then do
        putStrLn "You don't have this item"
    else do
      putStrLn "You: Probably I shouldn't leave it there."

leave PlayboyMagazine destination world = do
  currentLocation <- getLocation world
  doesLocationsBorder <- doesBorder destination currentLocation world
  if currentLocation /= destination || not doesLocationsBorder
    then putStrLn "You're too far."
    else putStrLn "You don't have this item"

leave _ _ _ = putStrLn "You don't have this item"

-- sets that a person is distracted
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


-- dialogue with people
talk :: Person -> World -> IO ()
talk SleepingGuy world = do
  putStrLn "Sleeping guy: Zzzzz..."
  return ()

talk SleepingGuy2 world = do
  putStrLn "Sleeping guy: Zzzzzzzz..."
  return ()

talk Guard world = do
  currentLocation <- getLocation world
  isGuardHere <- thereIsPersonInLocation Guard currentLocation world
  if isGuardHere then do
    putStrLn "Guard: Wait, what are you doing here?!"
    gameOver
    return ()
  else do
    putStrLn "There is no one named Guard here."

talk OldMan world = do
  currentLocation <- getLocation world
  isOldManHere <- thereIsPersonInLocation OldMan currentLocation world
  isQuest1Done <- isQuestDone Quest1 OldMan world
  isQuest2Done <- isQuestDone Quest2 OldMan world
  areAllQuestsDone <- isQuestDone AllQuests OldMan world
  hasCell2Key <- hasItem Cell2Key world
  isWaitingForCigarettes <- isWaitingFor OldMan world
  if not isQuest1Done && not hasCell2Key && isOldManHere
    then do
      putStrLn "You: Psst... I was thinking about escape. Are you in?"
      putStrLn "Old Man: Escape, huh? It won't be easy. I've been here for years and I'm too old for this."
      putStrLn "You: Damn... But you probably know this prison quite well. Do you have any advice?"
      putStrLn "Old Man: Yes, but it will cost. Please bring me 1 cigarettes and we will talk."
      putStrLn "You: I don't have that much.."
      putStrLn "Old Man: Here, take that key. Maybe you will find some outside."
      putStrLn "You: Wait, you had a key to our cell all this time?!"
      putStrLn "Old Man: Maybe I had, maybe I didn't. That's not important now. Just take the key and find me some ciggaretes."
      putStrLn "You received cell2_key."
      addItemToInventory Cell2Key world
    else if not isWaitingForCigarettes && isQuest1Done && not isQuest2Done && isOldManHere
      then do
        putStrLn "You: Okey, could you give me some advice now?"
        putStrLn "Old Man: Alright. There is a hole in the wall by the 16th pole on a prison yard."
        putStrLn "You: But wait, the lights are on, everything will be visible."
        putStrLn "Old Man: I don't give free information. Bring 1 more cigarettes."
        setWaitingFor OldMan world
      else if not areAllQuestsDone && isQuest2Done && isOldManHere
        then do
          putStrLn "You: What about the light?"
          putStrLn "Old Man: You can break the ventilation hole in the hallway and get into the room with fuses, where you turn off the light."
          putStrLn "You: Holy Chicken Trolley, that's my opportunity!!"
          setBorder Hallway Ventilation world
          setQuestDone AllQuests OldMan world
        else if areAllQuestsDone && isOldManHere
          then do
            putStrLn "You: Hi, I..."
            putStrLn "Old Man: Don't have time for you now, get lost."
          else if (not isQuest1Done || not isQuest2Done) && isOldManHere
            then do
              putStrLn "You: Hi, I..."
              putStrLn "Old Man: Bring me my cigarettes..."
            else do
              putStrLn "There is no one named Old Man here."

talk GymGuy world = do
  currentLocation <- getLocation world
  isGymGuyHere <- thereIsPersonInLocation GymGuy currentLocation world
  isWaitingForMeal <- isWaitingFor GymGuy world
  isQuest2Done <- isQuestDone Quest2 OldMan world
  isMealQuestDone <- isQuestDone MealQuest GymGuy world
  areAllQuestsDone <- isQuestDone AllQuests GymGuy world
  if not isWaitingForMeal && isQuest2Done && not isMealQuestDone && isGymGuyHere
    then do
      putStrLn "You see a strong guy that is exhausted after his training."
      putStrLn "You: Hey! I have a case. Could I do something for you in return for a small favor?"
      putStrLn "Gym Guy: You little man, what would you need help for?"
      putStrLn "You: To break the ventilation hole."
      putStrLn "Gym Guy: It's a piece of cake for me. Bring me a great meal cause I need to refill my carbs. Then I'll do the job."
      setWaitingFor GymGuy world
    else if not areAllQuestsDone && isMealQuestDone && isGymGuyHere
      then do
        putStrLn "You: So, will you help me with your muscles?"
        putStrLn "Gym Guy: Yeah, the meal was great. Take me to the place."
        putStrLn "You and the Gym Guy went to the ventilation grid and broke it."
        removePersonFromLocation GymGuy Gym world
        removeObjectFromLocation VentilationGrid Hallway world
        unlockLocation Ventilation world
        setQuestDone AllQuests GymGuy world
        debugGo Hallway world
      else if areAllQuestsDone && isGymGuyHere
        then do
          putStrLn "You: Hi, I..."
          putStrLn "Gym Guy: Don't have time for you now, get lost."
        else if not isMealQuestDone && isGymGuyHere
            then do
              putStrLn "You: Hi, I..."
              putStrLn "Old Man: Bring me my meal..."
          else do
            putStrLn "There is no one named Gym Guy here."

talk ShoweringPrisoner world = do
  currentLocation <- getLocation world
  isPrisonerHere <- thereIsPersonInLocation ShoweringPrisoner currentLocation world
  isWaitingForTowel <- isWaitingFor ShoweringPrisoner world
  isTowelQuestDone <- isQuestDone TowelQuest ShoweringPrisoner world
  areAllQuestsDone <- isQuestDone AllQuests ShoweringPrisoner world
  if not isWaitingForTowel && not isTowelQuestDone && isPrisonerHere
    then do
      putStrLn "Prisoner: Hey what are you looking at?!"
      putStrLn "You: I was just.."
      putStrLn "Prisoner: Get out now!! Or actually, wait.. Bring me a towel!"
      putStrLn "You: Why would I?"
      putStrLn "Prisoner: You dare to ask?!"
      putStrLn "You: I'm not going to do this for free."
      putStrLn "Prisoner: Fine, if you decide to help me I'll give you something in return."
      setWaitingFor ShoweringPrisoner world
    else if not areAllQuestsDone && isTowelQuestDone && isPrisonerHere
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
      else if areAllQuestsDone && isPrisonerHere
        then do
          putStrLn "You: Hi, I..."
          putStrLn "Prisoner: Don't have time for you now, get lost."
        else if not isTowelQuestDone && isPrisonerHere
            then do
              putStrLn "You: Hi, I..."
              putStrLn "Old Man: Bring me my towel..."
          else do
            putStrLn "There is no one named Showering Prisoner here."

talk Chef world = do
  currentLocation <- getLocation world
  isChefHere <- thereIsPersonInLocation Chef currentLocation world
  isWaitingForCoffee <- isWaitingFor Chef world
  isCoffeeQuestDone <- isQuestDone CoffeeQuest Chef world
  areAllQuestsDone <- isQuestDone AllQuests Chef world
  if not isWaitingForCoffee && not isCoffeeQuestDone && isChefHere
    then do
      putStrLn "You: Hi! I've heard that you're the best chef in here. Could you make me your signature meal?"
      putStrLn "Chef: Nice words won't be enough. I'am actually pretty tired, if you could bring me some coffee then I'll cook something."
      putStrLn "You: I should have some in my cell, I'll be in a moment."
      addItemToObject Coffee Table world
      setWaitingFor Chef world
    else if not areAllQuestsDone && isCoffeeQuestDone && isChefHere
      then do
        putStrLn "You: Now you're quite caffenaited, aren't you?"
        putStrLn "Chef: Yeah, thanks. I'll cook something quickly."
        putStrLn "After few minutes chef hands you a hot meal."
        addItemToInventory GreatMeal world
        setQuestDone AllQuests Chef world
      else if areAllQuestsDone && isChefHere
        then do
          putStrLn "You: Hi, I..."
          putStrLn "Chef: Don't have time for you now, get lost."
        else if not isCoffeeQuestDone && isChefHere
            then do
              putStrLn "You: Hi, I..."
              putStrLn "Old Man: Bring me my coffee..."
          else do
            putStrLn "There is no one named Chef here."

-- giving items to people
give :: Item -> Person -> World -> IO ()
give Cigarette OldMan world = do
  currentLocation <- getLocation world
  isOldManHere <- thereIsPersonInLocation OldMan currentLocation world
  hasEnoughCigarettes <- hasItemCountInInventory Cigarette 1 world
  isQuest1Done <- isQuestDone Quest1 OldMan world
  if isOldManHere && hasEnoughCigarettes
    then do
      removeItemFromInventory Cigarette 1 world
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
    else putStrLn "You don't have that item."

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
    else putStrLn "Gym Guy: You don't have the meal."

give Coffee Chef world = do
  currentLocation <- getLocation world
  isChefHere <- thereIsPersonInLocation Chef currentLocation world
  hasCoffee <- hasItem Coffee world
  isCoffeeQuestDone <- isQuestDone CoffeeQuest Chef world
  if isChefHere && hasCoffee && not isCoffeeQuestDone
    then do
      putStrLn "Chef: Oh, you have the coffee. I need a boost of energy."
      putStrLn "You hand the coffee to the Chef."
      removeItemFromInventory Coffee 1 world
      setQuestDone CoffeeQuest Chef world
  else if not hasCoffee && not isCoffeeQuestDone
    then do
      putStrLn "Chef: You don't have the coffee."
  else do
    putStrLn "Chef: I don't want that item."

give Towel ShoweringPrisoner world = do
  currentLocation <- getLocation world
  isPrisonerHere <- thereIsPersonInLocation ShoweringPrisoner currentLocation world
  hasTowel <- hasItem Towel world
  isTowelQuestDone <- isQuestDone TowelQuest ShoweringPrisoner world
  if isPrisonerHere && hasTowel &&  not isTowelQuestDone
    then do
      putStrLn "Prisoner: Oh, there you are. Gimmie the towel."
      putStrLn "You hand the towel to the Showering Prisoner."
      removeItemFromInventory Towel 1 world
      setQuestDone TowelQuest ShoweringPrisoner world
  else if not hasTowel && not isTowelQuestDone
    then do
      putStrLn "Prisoner: You don't have the towel."
  else do
    putStrLn "Prisoner: I don't want that item."

give _ _ world =
  putStrLn "This person doesn't want that item."


-- This function describes how to display inventory's contents and how the number of picked up cigarettes increases
listItems :: World -> IO ()
listItems world = do
  items <- getInventory world
  forM_ items $ \item -> putStrLn $ "* " ++ show item


--These functions are responsible for finishing the game
gameOver :: IO ()
gameOver = do
  putStrLn "YOU GOT CAUGHT!"
  putStrLn "Maybe try your luck again from the beginning."
  exitSuccess

finish :: IO ()
finish = do
  putStrLn "The game is over. Hope you had a great time!"
  exitSuccess

commands :: IO ()
commands = do
  putStrLn ""
  putStrLn "Available commands are:"
  putStrLn "go Destination            -- to go to the selected destination."
  putStrLn "unlock Destination        -- to unlock the passage to the destination."
  putStrLn "look                      -- to look around you again."
  putStrLn "investigate Object        -- to see if there is any item in the object."
  putStrLn "take Item from Object     -- to pick up an item from the object."
  putStrLn "inventory                 -- to list the items that you possess."
  putStrLn "give Item to Person       -- to give a person the item they wanted."
  putStrLn "talk Person               -- to talk to a person."
  putStrLn "commands                  -- to see this message again."
  putStrLn ""

-- dictionaries for parsing strings to data objects
stringsToItems :: [(String, Item)]
stringsToItems = [("poop", Poop), ("coin", Coin), ("cigarette", Cigarette), ("playboymagazine", PlayboyMagazine), ("flashlight", Flashlight),
         ("cell1key", Cell1Key), ("cell2key", Cell2Key), ("towel", Towel), ("batteries", Batteries), ("greatmeal", GreatMeal), ("coffee", Coffee)]

stringsToObjects :: [(String, Object)]
stringsToObjects = [("occupiedbed", OccupiedBed), ("smalltoilet", SmallToilet), ("teapot", Teapot), ("occupiedbed2", OccupiedBed2),
           ("table", Table), ("yourbed", YourBed), ("oldmansbed", OldMansBed), ("toilet", Toilet), ("bunkbed", BunkBed),
           ("bedcabinet", BedCabinet), ("shelf", Shelf), ("ventilationgrid", VentilationGrid), ("desk", Desk), ("tv", TV),
           ("coat", Coat), ("chair", Chair), ("oven", Oven), ("corner", Corner), ("fridge", Fridge), ("sink", Sink),
           ("shower", Shower), ("shower2", Shower2), ("shower3", Shower3), ("shower4", Shower4), ("cabinet", Cabinet),
           ("treadmill", Treadmill), ("treadmill2", Treadmill2), ("bench", Bench), ("fusebox", FuseBox), ("pole1", Pole1),
           ("pole2", Pole2), ("pole3", Pole3), ("pole4", Pole4), ("pole5", Pole5), ("pole6", Pole6), ("pole7", Pole7),
           ("pole8", Pole8), ("pole9", Pole9), ("pole10", Pole10), ("pole11", Pole11), ("pole12", Pole12), ("pole13", Pole13),
           ("pole14", Pole14), ("pole15", Pole15), ("pole16", Pole16), ("pole17", Pole17), ("pole18", Pole18), ("pole19", Pole19),
           ("pole20", Pole20), ("pole21", Pole21), ("pillow", Pillow)]

stringsToLocations :: [(String, Location)]
stringsToLocations = [("cell1", Cell1), ("cell2", Cell2), ("cell3", Cell3), ("hallway", Hallway), ("guardroom", GuardRoom),
             ("kitchen", Kitchen), ("showerroom", ShowerRoom), ("gym", Gym), ("ventilation", Ventilation),
             ("shed", Shed), ("prisonyard", PrisonYard), ("waytofreedom", WayToFreedom)]

stringsToPeople :: [(String, Person)]
stringsToPeople = [("oldman", OldMan), ("gymguy", GymGuy), ("showeringprisoner", ShoweringPrisoner), ("chef", Chef),
          ("sleepingguy", SleepingGuy), ("sleepingguy2", SleepingGuy2), ("guard", Guard)]


-- functions for extracting data objects from users commands
extractDestination :: String -> IO (Maybe Location)
extractDestination command = case words command of
  ["go", destination] -> return (lookup destination stringsToLocations)
  _ -> return Nothing

extractLocationToUnlock :: String -> IO (Maybe Location)
extractLocationToUnlock command = case words command of
  ["unlock", location] -> return (lookup location stringsToLocations)
  _ -> return Nothing

extractObject :: String -> IO (Maybe Object)
extractObject command = case words command of
  ["investigate", object] -> return (lookup object stringsToObjects)
  _ -> return Nothing

extractItemAndObject :: String -> IO (Maybe (Item, Object))
extractItemAndObject command = case words command of
  ["take", itemName, "from", objectName] -> do
    case (lookup itemName stringsToItems, lookup objectName stringsToObjects) of
      (Just item, Just object) -> return (Just (item, object))
      _ -> return Nothing
  _ -> return Nothing

extractPersonToTalk :: String -> IO (Maybe Person)
extractPersonToTalk command = case words command of
  ["talk", person] -> return (lookup person stringsToPeople)
  _ -> return Nothing

extractItemAndPerson :: String -> IO (Maybe (Item, Person))
extractItemAndPerson command = case words command of
  ["give", itemName, "to", personName] -> do
    case (lookup itemName stringsToItems, lookup personName stringsToPeople) of
      (Just item, Just person) -> return (Just (item, person))
      _ -> return Nothing
  _ -> return Nothing

-- prints all facts that are curenntly in world's facts (for debugging)
printFacts :: World -> IO ()
printFacts worldRef = do
  (facts, _) <- readIORef worldRef
  putStrLn "Facts:"
  mapM_ print facts


main :: IO ()
main = do
  putStrLn ""
  putStrLn "Welcome to the Prison Escape Game!"
  putStrLn "You find yourself in a dark prison cell."
  putStrLn "Type 'commands' to see available commands"
  putStrLn "Every words/names write without spaces and with small letters"
  putStrLn ""
  worldRef <- newWorld  -- game state initialization
  initializeBorders worldRef
  initializeItemLocations worldRef
  initializeLockedLocations worldRef
  initializeObjects worldRef
  initializePeople worldRef
  initializeStartingLocation worldRef
  --printFacts worldRef
  gameLoop worldRef

gameLoop :: World -> IO ()
gameLoop worldRef = do
  putStrLn "\nWhat do you want to do?"
  command <- getLine
  executeCommand command worldRef
  gameLoop worldRef

executeCommand :: String -> World -> IO ()
executeCommand command worldRef
  | "commands" `isPrefixOf` command = do
    commands
  | "look" `isPrefixOf` command = do
      look worldRef
  | "go" `isPrefixOf` command = do
      destination <- extractDestination command
      case destination of
        Just location -> go location worldRef
        Nothing -> putStrLn "Invalid destination"
      --go destination worldRef
  | "take" `isPrefixOf` command = do
      itemObject <- extractItemAndObject command
      case itemObject of
        Just (item, object) -> takeItem item object worldRef
        Nothing -> putStrLn "Invalid item or object"
  | "investigate" `isPrefixOf` command = do
    investigatedObject <- extractObject command
    case investigatedObject of
      Just object -> investigate object worldRef
      Nothing -> putStrLn "Invalid object"
  | "talk" `isPrefixOf` command = do
    chitChatPerson <- extractPersonToTalk command
    case chitChatPerson of
      Just person -> talk person worldRef
      Nothing -> putStrLn "Invalid person"
  | "inventory" `isPrefixOf` command = do
      inventory <- getInventoryItemCounts worldRef
      putStrLn "Inventory:"
      mapM_ putStrLn (map show inventory)
  | "unlock" `isPrefixOf` command = do
    unlockedLocation <- extractLocationToUnlock command
    case unlockedLocation of
      Just location -> unlock location worldRef
      Nothing -> putStrLn "Invalid location"
  | "give" `isPrefixOf` command = do
      personToGive <- extractItemAndPerson command
      case personToGive of
        Just (item, person) -> give item person worldRef
        Nothing -> putStrLn "Invalid item or person"
  | "escape" `isPrefixOf` command = do
    escape worldRef
  | "blow fuses" `isPrefixOf` command = do
    blowFuses worldRef
  | "leave magazine" `isPrefixOf` command = do
    leave PlayboyMagazine GuardRoom worldRef
  | otherwise = putStrLn "Invalid command"