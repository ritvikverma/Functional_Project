module KodableUtils where

-- This file is the utility file for useful functions 

import Data.Maybe
import Data.List
import Prelude hiding (Left, Right)
import Parser
import KodableData

import Data.Char

import System.IO
import Data.Function

loadMap :: String -> (Maybe Map, String) -- Loads the map, returns Nothing if could not load, with reason
loadMap content 
    | not $ all (== head wordsListLengths) (tail wordsListLengths) = (Nothing, "The map is not symmetrical") -- Lengths are not equal
    | length wordsListLengths == head wordsListLengths = (Nothing, "The map is not rectangular") -- Square map
    | [InvalidElement] `elem` attemptedMap = (Nothing, "Invalid element found in map") -- Invalid element in map
    | isNothing $ getElementLocationInMap attemptedMap Ball = (Nothing, "No ball in map") -- No ball in map
    | isNothing $ getElementLocationInMap attemptedMap Target = (Nothing, "No target in map") -- No ball in map
    | otherwise = (Just attemptedMap, "")
    where
        attemptedMap = [if InvalidElement `elem` map charToElement (words line) then [InvalidElement] else map charToElement (words line) | line <- lines content]
        wordsListLengths = [length $ words line | line <- lines content]

isColour :: MapElement -> Bool -- Is this map element one of the conditionals? 
isColour ele = ele `elem` [Yellow, Pink, Orange]

getElementLocationInMap :: Map -> MapElement -> Maybe Location -- Which location is this element at in this map? 
getElementLocationInMap map ele = if null res then Nothing else Just (snd(head res), fromJust (fst $ head res))
    where 
        res = filter (\(x, _) -> isJust x) [(elemIndex ele xs, xi) | (xs, xi) <- zip map [0..]]

getElementAtLocation :: Map -> Location -> MapElement -- Which element is at this location?
getElementAtLocation inpMap location = inpMap !! fst location !! snd location 
 
inBoard :: Map -> Location -> Bool -- Is this location in this board? 
inBoard inpMap (a,b) = a >= 0 && a < length inpMap && b >= 0 && b < length(head inpMap)

isSolvable :: Map -> Bool -- Is this map solvable? 
isSolvable inpMap = not $ null allPaths
    where
        allPaths = concat [getPathsToTargetWithBonuses inpMap bonusNumber (fromJust $ getElementLocationInMap inpMap Ball) [] [] 0 | bonusNumber <- [0..getBonusOnMap inpMap]]

move :: Map -> Direction -> (Map, Int, Bool) -- Makes a move in this direction, and returns the new map, bonuses collected, and whether we have won
move inpMap direction
    | direction == Up && inBoard inpMap (ballRow - 1, ballColumn) && getElementAtLocation inpMap (ballRow - 1, ballColumn) `elem` movableElements = makeMove (ballRow - 1, ballColumn)
    | direction == Down && inBoard inpMap (ballRow + 1, ballColumn) && getElementAtLocation inpMap (ballRow + 1, ballColumn) `elem` movableElements = makeMove (ballRow + 1, ballColumn)
    | direction == Right && inBoard inpMap (ballRow, ballColumn + 1) && getElementAtLocation inpMap (ballRow, ballColumn + 1) `elem` movableElements = makeMove (ballRow, ballColumn + 1)
    | direction == Left && inBoard inpMap (ballRow, ballColumn - 1) && getElementAtLocation inpMap (ballRow, ballColumn - 1) `elem` movableElements = makeMove (ballRow, ballColumn - 1)
    | isConditional direction = move inpMap $ snd (fromConditional direction)
    | otherwise = (inpMap, 0, False)
    where
        (ballRow, ballColumn) = fromJust (getElementLocationInMap inpMap Ball)
        makeMove targetPosition
            | getElementAtLocation inpMap targetPosition == Bonus = (swapPositions (ballRow, ballColumn) targetPosition, 1, False)
            | getElementAtLocation inpMap targetPosition == Target = (swapPositions (ballRow, ballColumn) targetPosition, 0, True)
            | otherwise =  (swapPositions (ballRow, ballColumn) targetPosition, 0, False)
        swapPositions ballPosition targetPosition
            | getElementAtLocation inpMap targetPosition `elem` [Bonus, Target] = putElemAtLocation (putElemAtLocation inpMap targetPosition Ball) ballPosition PathBlock
            | otherwise = putElemAtLocation (putElemAtLocation inpMap targetPosition Ball) ballPosition (getElementAtLocation inpMap targetPosition)
        movableElements = [PathBlock, Pink, Orange, Yellow, Bonus, Target]

getNextElementInDirection :: Map -> Direction -> Maybe MapElement -- Returns the next element in this direction
getNextElementInDirection inpMap direction
    | direction == Up && inBoard inpMap (ballRow - 1, ballColumn) = Just $ getElementAtLocation inpMap (ballRow - 1, ballColumn)
    | direction == Down && inBoard inpMap (ballRow + 1, ballColumn) = Just $ getElementAtLocation inpMap (ballRow + 1, ballColumn)
    | direction == Right && inBoard inpMap (ballRow, ballColumn + 1) = Just $ getElementAtLocation inpMap (ballRow, ballColumn + 1)
    | direction == Left && inBoard inpMap (ballRow, ballColumn - 1) = Just $ getElementAtLocation inpMap (ballRow, ballColumn - 1)
    | otherwise = Nothing
    where
        (ballRow, ballColumn) = fromJust (getElementLocationInMap inpMap Ball)

putElemAtLocation :: Map -> Location -> MapElement -> Map -- Returns a map after putting a MapElement at a user specified location
putElemAtLocation inpMap (a,b) toPut = take a inpMap ++ [addPathBlock (inpMap !! a) b] ++ drop (a + 1) inpMap
    where
        addPathBlock row col = take col row ++ [toPut] ++ drop (col+1) row

getPathsToTargetWithBonuses :: Map -> Int -> Location -> Path -> [(Location, Int)] -> Int -> [Path] -- Core function for shortest path with X number of bonuses
getPathsToTargetWithBonuses inpMap requiredBonuses currNode currPath alreadyVisitedWithBonusCount bonusesCollected
    | getElementAtLocation inpMap currNode == Target && bonusesCollected==requiredBonuses = [currPath++[currNode]] -- Base case where we have found the target and have visited the max bonus(es)
    | getElementAtLocation inpMap currNode == Target = [] -- We cannot pass by the target and come back again
    | getElementAtLocation inpMap currNode == Bonus = concat [getPathsToTargetWithBonuses (putElemAtLocation inpMap currNode PathBlock) requiredBonuses newNeighbour (currPath++[currNode]) (alreadyVisitedWithBonusCount ++ [(currNode, bonusesCollected)]) (bonusesCollected+1) | newNeighbour <- newNeighbours] -- Increase bonus
    | (newNeighboursWithBonusCounts `intersect` alreadyVisitedWithBonusCount) == newNeighboursWithBonusCounts = [] -- Base case where we have already traversed the neighbours, with the same bonus count. No incentive to re-traverse
    | otherwise = concat [getPathsToTargetWithBonuses inpMap requiredBonuses newNeighbour (currPath++[currNode]) (alreadyVisitedWithBonusCount ++ [(currNode, bonusesCollected)]) bonusesCollected | newNeighbour <- newNeighbours] 
    where
        neighbours inpMap (a, b) = [(x,y) | (x,y) <- existingNeighbours, booledMap !! x !! y]
            where
            booledMap = [map (/= Grass) row | row <- inpMap]
            existingNeighbours = filter (\(a,b) -> inBoard inpMap (a,b)) [(a+1, b), (a-1, b), (a, b+1), (a, b-1)]
        newNeighbours
            | null alreadyVisitedWithBonusCount = neighbours inpMap currNode
            | otherwise = neighboursInDirection inpMap (fst $ last alreadyVisitedWithBonusCount) currNode
        newNeighboursWithBonusCounts = [(newNeighbour, bonusesCollected) | newNeighbour <- newNeighbours]
        neighboursInDirection inpMap (parentX, parentY) (currX, currY)
            | currentIsCondition || hasReachedEnd = neighbours inpMap (currX, currY)
            | otherwise = [nextNode]
            where
                nextNode = getNextInDirection (parentX, parentY) (currX, currY)
                currentIsCondition = isColour (getElementAtLocation inpMap (currX, currY))
                hasReachedEnd = not (inBoard inpMap nextNode) || getElementAtLocation inpMap (getNextInDirection (parentX, parentY) (currX, currY)) == Grass
                getNextInDirection (parentRow, parentColumn) (currRow, currColumn) -- Returns the locations of the neighbours that are in the ball's direction
                    | parentRow - currRow == 1 = (currRow - 1 , currColumn) -- Going up
                    | parentRow - currRow == -1 = (currRow + 1 , currColumn) -- Going down
                    | parentColumn - currColumn == 1 = (currRow, currColumn - 1) -- Going left
                    | parentColumn - currColumn == -1 = (currRow, currColumn + 1) -- Going right

getBonusOnMap :: Map -> Int -- Simply returns all the bonuses on this map
getBonusOnMap inpMap = sum [length $ filter (==Bonus) row | row <- inpMap]

decodeDirections :: [Direction] -> [Direction] -- Simplifies the user inputted directions to sequential directions
decodeDirections = concatMap decodeDirection
    where
        decodeDirection (Function(direction1, direction2, direction3)) = [direction1, direction2, direction3]
        decodeDirection (Loop(n, direction1, direction2)) = concat $ replicate n [direction1, direction2]
        decodeDirection direction = [direction]

getIndividualDirection :: Location -> Location -> Direction
getIndividualDirection (parentRow, parentColumn) (currRow, currColumn) -- Gets the direction of where the ball seems to be going
    | parentRow - currRow == 1 = Up -- Going up
    | parentRow - currRow == -1 = Down -- Going down
    | parentColumn - currColumn == 1 = Left -- Going left
    | parentColumn - currColumn == -1 = Right -- Going right

createLoops :: [Direction] -> [Direction] -- Generates loops on set of directions
createLoops stack
  | length stack < 4 = stack
  | otherwise = if action1 == (stack !! 2) && action2 == (stack !! 3) then result else head stack : createLoops (tail stack)
  where
    action1 = head stack
    action2 = stack !! 1
    (loopCount, remList) = appendLoops (action1, action2) (drop 4 stack) 2
    result = Loop (loopCount, action1, action2) : createLoops remList
    appendLoops (dir1, dir2) currDirList count
        | length currDirList < 2 || count > 4 = (count, currDirList)
        | otherwise = if head currDirList == dir1 && currDirList !! 1 == dir2 then appendLoops (dir1, dir2) (drop 2 currDirList) (count + 1) else (count, currDirList)

highestOccurringFunction :: [Direction] -> Direction -- Returns the highest occurring functon in this list of directions
highestOccurringFunction list = fst $ maximumBy (compare `on` snd) elemCounts
  where
    elemCounts = nub [(element, count) | element <- list, let count = length (filter (== element) list)]

putFunc :: [Direction] -> Direction -> [Direction] -- Puts this function in this list of directions
putFunc directions fn
  | length directions < 3 = directions
  | otherwise = if head directions == a && directions !! 1 == b && directions !! 2 == c then fn : putFunc (drop 3 directions) fn else head directions : putFunc (tail directions) fn
  where
    Function (a, b, c) = fn

getCompressedPath :: [Direction] -> [Direction] -- Nested function that returns compressed path (after adding loops and functions) if we send a list of directions
getCompressedPath optimalPath = finalPathWithFunctions
  where
    parsedWithLoops = createLoops optimalPath
    finalPathWithFunctions = appendFunctions parsedWithLoops
    appendFunctions directions = if null functions then directions else putFunc directions (highestOccurringFunction functions)
        where
            functions = getFunctions $ createFunctions directions
            createFunctions [a, b] = [a, b]
            createFunctions [a] = [a]
            createFunctions [] = []
            createFunctions (Loop (n, a1, a2) : b : c : remList) = Loop (n, a1, a2) : createFunctions (b : c : remList)
            createFunctions (b : Loop (n, a1, a2) : c : remList) = [b, Loop (n, a1, a2)] ++ createFunctions (c : remList)
            createFunctions (b : c : Loop (n, a1, a2) : remList) = [b, c, Loop (n, a1, a2)] ++ createFunctions remList
            createFunctions (a : b : c : remList) = Function (a, b, c) : createFunctions (b : c : remList)  
            getFunctions [] = []
            getFunctions (Function (a, b, c) : remList) = Function (a, b, c) : getFunctions remList
            getFunctions (x : remList) = getFunctions remList

stringifyPath :: [Direction] -> String -- Stringifies a path. Useful for optimal paths
stringifyPath path = if isNothing retrievedFunction then unwords formedStringArray else unwords (formedStringArray++[show $ fromJust retrievedFunction])
    where
        formedStringArray = [if isFunction direction then "Function" else show direction | direction <- path]
        retrievedFunction = getFunction path
        getFunction [] = Nothing
        getFunction (Function f : xs) = Just (Function f)
        getFunction (_:xs) = getFunction xs

hintyPath :: [Direction] -> String -- During a hint, we can't show a function, so the hint path is different
hintyPath inpPath = unwords [show direction | direction <- hintyDirections inpPath]
    where
        hintyDirections [] = []
        hintyDirections (Function (a, b, c): xs) = a:b:c:hintyDirections xs
        hintyDirections (x:xs) = x:hintyDirections xs

shortestPath :: Map -> [Path] -> [Direction] -- Returns the shortest path
shortestPath inpMap pathList = shortestCompressedPath
    where
        compressedPath path = getCompressedPath (map (snd . addDirectionStrings) $ reverse(getReducedArray(reverse (getRawString inpMap path))))
        shortestCompressedPath = head $ sortOn length $ [compressedPath uncompressedPath | uncompressedPath <- pathList]
        getRawString inpMap [x,y] = [(getElementAtLocation inpMap x, getIndividualDirection x y)]
        getRawString inpMap (x:y:zs) = (getElementAtLocation inpMap x, getIndividualDirection x y): getRawString inpMap (y:zs) 
        getReducedArray [x] = [x]
        getReducedArray [x,y] = if snd x/= snd y then x:[y] else [x]
        getReducedArray (x:y:zs) = if snd x/= snd y then x:getReducedArray (y:zs) else getReducedArray (y:zs)
        addDirectionStrings (mapElement, direction) = if isColour mapElement then (mapElement, Conditional (mapElement, direction)) else (mapElement, direction)