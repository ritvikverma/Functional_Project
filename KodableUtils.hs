module KodableUtils where

-- This file is the utility file for useful functions 

import Data.List
    ( (++),
      filter,
      zip,
      map,
      length,
      elem,
      null,
      sum,
      concat,
      concatMap,
      elemIndex,
      intersect,
      unwords,
      (!!),
      drop,
      head,
      last,
      replicate,
      reverse,
      take )
import Data.Maybe
import Prelude
import Parser
import KodableData

import Data.Char

isConditional :: MapElement -> Bool -- Is this map element one of the conditionals? 
isConditional ele = ele `elem` [Yellow, Pink, Orange]

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
    | direction == KodableData.Right && inBoard inpMap (ballRow, ballColumn + 1) && getElementAtLocation inpMap (ballRow, ballColumn + 1) `elem` movableElements = makeMove (ballRow, ballColumn + 1)
    | direction == KodableData.Left && inBoard inpMap (ballRow, ballColumn - 1) && getElementAtLocation inpMap (ballRow, ballColumn - 1) `elem` movableElements = makeMove (ballRow, ballColumn - 1)
    | isConditionalElem direction = move inpMap $ snd (fromConditionalElem direction)
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
    | direction == KodableData.Right && inBoard inpMap (ballRow, ballColumn + 1) = Just $ getElementAtLocation inpMap (ballRow, ballColumn + 1)
    | direction == KodableData.Left && inBoard inpMap (ballRow, ballColumn - 1) = Just $ getElementAtLocation inpMap (ballRow, ballColumn - 1)
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
    | getElementAtLocation inpMap currNode == Bonus = concat [getPathsToTargetWithBonuses (putElemAtLocation inpMap currNode PathBlock) requiredBonuses newNeighbour (currPath++[currNode]) (alreadyVisitedWithBonusCount ++ [(currNode, bonusesCollected+1)]) (bonusesCollected+1) | newNeighbour <- newNeighbours] -- Increase bonus
    | newNeighboursWithBonusCounts `intersect` alreadyVisitedWithBonusCount == newNeighboursWithBonusCounts = [] -- Base case where we have already traversed the neighbours, with the same bonus count. No incentive to re-traverse
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
                currentIsCondition = isConditional (getElementAtLocation inpMap (currX, currY))
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
        decodeDirection (FunctionElem (Function(direction1, direction2, direction3))) = [direction1, direction2, direction3]
        decodeDirection (LoopElem (Loop(n, direction1, direction2))) = concat $ replicate n [direction1, direction2]
        decodeDirection direction = [direction]

stringifyPath :: Map -> Path -> String -- Takes a path and makes it into a string after compressing the path
stringifyPath inpMap path = appendDirections (map addDirectionStrings $ reverse (getReducedArray (reverse (getRawString inpMap path))))
    where
        getReducedArray [x] = [x]
        getReducedArray [x,y] = if snd x/= snd y then x:[y] else [x]
        getReducedArray (x:y:zs) = if snd x/= snd y then x:getReducedArray (y:zs) else getReducedArray (y:zs)
        getRawString inpMap [x,y] = [(getElementAtLocation inpMap x, getIndividualDirection x y)]
        getRawString inpMap (x:y:zs) = (getElementAtLocation inpMap x, getIndividualDirection x y): getRawString inpMap (y:zs) 
        addDirectionStrings (mapElement, direction) = if isConditional mapElement then (mapElement, show (Conditional (mapElement, direction))) else (mapElement, show direction)
        appendDirections directions = unwords $ map snd directions
        getIndividualDirection (parentRow, parentColumn) (currRow, currColumn) -- Gets the direction of where the ball seems to be going
            | parentRow - currRow == 1 = Up -- Going up
            | parentRow - currRow == -1 = Down -- Going down
            | parentColumn - currColumn == 1 = KodableData.Left -- Going left
            | parentColumn - currColumn == -1 =  KodableData.Right -- Going right