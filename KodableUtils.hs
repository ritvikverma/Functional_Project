module KodableUtils where

import Data.List
import Data.Maybe

data MapElement = PathBlock | Grass | Bonus | Target | Pink | Orange | Yellow | Ball | Invalid deriving (Eq)
data Direction = Down | Up | Right | Left deriving (Eq, Show)
type Map = [[MapElement]]
type Conditional = (MapElement, Direction)
type Location = (Int, Int)
type Path = [Location]

charToElement :: String -> MapElement
charToElement "*" = Grass
charToElement "-" = PathBlock
charToElement "b" = Bonus
charToElement "@" = Ball
charToElement "p" = Pink
charToElement "o" = Orange
charToElement "y" = Yellow
charToElement "t" = Target
charToElement  _  = Invalid

instance Show MapElement where
    show Grass = "*"
    show PathBlock = "-"
    show Bonus = "b"
    show Ball = "@"
    show Pink = "p"
    show Orange = "o"
    show Yellow = "y"
    show Target = "t"

conditionalToString :: Conditional -> String
conditionalToString (mapElement, direction) = "Cond{" ++ show mapElement ++ "}{" ++ show direction ++ "}"

printMap :: Map -> String
printMap [x] = unwords (map show x)
printMap (x:xs) = unwords (map show x) ++ "\n" ++ printMap xs

isConditional :: MapElement -> Bool
isConditional ele = ele `elem` [Yellow, Pink, Orange]

getElementLocationInMap :: Map -> MapElement -> Maybe Location
getElementLocationInMap map ele = if null res then Nothing else Just (snd(head res), fromJust (fst $ head res))
    where 
        res = filter (\(x, _) -> isJust x) [(elemIndex ele xs, xi) | (xs, xi) <- zip map [0..]]

getElementAtLocation :: Map -> Location -> MapElement
getElementAtLocation inpMap location = inpMap !! fst location !! snd location 

neighbours :: Map -> Location -> [Location]
neighbours inpMap (a, b) = [(x,y) | (x,y) <- existingNeighbours, booledMap !! x !! y]
        where
            booledMap = [map (/= Grass) row | row <- inpMap]
            existingNeighbours = filter (\(a,b) -> inBoard inpMap (a,b)) [(a+1, b), (a-1, b), (a, b+1), (a, b-1)]
 
inBoard :: Map -> Location -> Bool
inBoard inpMap (a,b) = a >= 0 && a < length inpMap && b >= 0 && b < length(head inpMap)

getNextInDirection :: Location -> Location -> Location
getNextInDirection (parentRow, parentColumn) (currRow, currColumn)
    | parentRow - currRow == 1 = (currRow - 1 , currColumn) -- Going up
    | parentRow - currRow == -1 = (currRow + 1 , currColumn) -- Going down
    | parentColumn - currColumn == 1 = (currRow, currColumn - 1) -- Going left
    | parentColumn - currColumn == -1 = (currRow, currColumn + 1) -- Going right

neighboursInDirection :: Map -> Location -> Location -> [Location]
neighboursInDirection inpMap (parentX, parentY) (currX, currY)
    | currentIsCondition || hasReachedEnd = neighbours inpMap (currX, currY)
    | otherwise = [nextNode]
    where
        nextNode = getNextInDirection (parentX, parentY) (currX, currY)
        currentIsCondition = isConditional (getElementAtLocation inpMap (currX, currY))
        hasReachedEnd = not (inBoard inpMap nextNode) || getElementAtLocation inpMap (getNextInDirection (parentX, parentY) (currX, currY)) == Grass

putElemAtLocation :: Map -> Location -> MapElement -> Map
putElemAtLocation inpMap (a,b) toPut = take a inpMap ++ [addPathBlock (inpMap !! a) b] ++ drop (a + 1) inpMap
    where
        addPathBlock row col = take col row ++ [toPut] ++ drop (col+1) row

getPathsToTargetWithBonuses :: Map -> Int -> Location -> Path -> [(Location, Int)] -> Int -> [Path]
getPathsToTargetWithBonuses inpMap requiredBonuses currNode currPath alreadyVisitedWithBonusCount bonusesCollected
    | getElementAtLocation inpMap currNode == Target && bonusesCollected==requiredBonuses = [currPath++[currNode]] -- Base case where we have found the target and have visited the max bonuses
    | getElementAtLocation inpMap currNode == Target = [] -- We cannot pass by the target and come back again
    | getElementAtLocation inpMap currNode == Bonus = concat [getPathsToTargetWithBonuses (putElemAtLocation inpMap currNode PathBlock) requiredBonuses newNeighbour (currPath++[currNode]) (alreadyVisitedWithBonusCount ++ [(currNode, bonusesCollected+1)]) (bonusesCollected+1) | newNeighbour <- newNeighbours] -- Increase bonus
    | newNeighboursWithBonusCounts `intersect` alreadyVisitedWithBonusCount == newNeighboursWithBonusCounts = [] -- Base case where we have already traversed the neighbours, with the same bonus count. No incentive to re-traverse
    | otherwise = concat [getPathsToTargetWithBonuses inpMap requiredBonuses newNeighbour (currPath++[currNode]) (alreadyVisitedWithBonusCount ++ [(currNode, bonusesCollected)]) bonusesCollected | newNeighbour <- newNeighbours] 
    where
        newNeighbours
            | null alreadyVisitedWithBonusCount = neighbours inpMap currNode
            | otherwise = neighboursInDirection inpMap (fst $ last alreadyVisitedWithBonusCount) currNode
        newNeighboursWithBonusCounts = [(newNeighbour, bonusesCollected) | newNeighbour <- newNeighbours]

getBonusOnMap :: Map -> Int
getBonusOnMap inpMap = sum [length $ filter (==Bonus) row | row <- inpMap]

getIndividualDirection :: Location -> Location -> Direction
getIndividualDirection (parentRow, parentColumn) (currRow, currColumn)
    | parentRow - currRow == 1 = Up -- Going up
    | parentRow - currRow == -1 = Down -- Going down
    | parentColumn - currColumn == 1 = KodableUtils.Left -- Going left
    | parentColumn - currColumn == -1 =  KodableUtils.Right -- Going right

stringifyPath :: Map -> Path -> String
stringifyPath inpMap path = appendDirections (map addDirectionStrings $ reverse (getReducedArray (reverse (getRawString inpMap path))))
    where
        getReducedArray [x] = [x]
        getReducedArray [x,y] = if snd x/= snd y then x:[y] else [x]
        getReducedArray (x:y:zs) = if snd x/= snd y then x:getReducedArray (y:zs) else getReducedArray (y:zs)
        getRawString inpMap [x,y] = [(getElementAtLocation inpMap x, getIndividualDirection x y)]
        getRawString inpMap (x:y:zs) = (getElementAtLocation inpMap x, getIndividualDirection x y): getRawString inpMap (y:zs) 
        addDirectionStrings (mapElement, direction) = if isConditional mapElement then (mapElement, conditionalToString (mapElement, direction)) else (mapElement, show direction)
        appendDirections directions = unwords $ map snd directions
