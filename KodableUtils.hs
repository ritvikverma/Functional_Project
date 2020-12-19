module KodableUtils where

import Data.List
import Data.Maybe
import Prelude
import Control.Monad 
import Parser

import Control.Applicative hiding (optional)

import Control.Arrow ()

import Data.Char
import Data.Functor
import Data.Monoid

data MapElement = PathBlock | Grass | Bonus | Target | Pink | Orange | Yellow | Ball | InvalidElement deriving (Eq)
data Direction = Down | Up | Right | Left | ConditionalElem Conditional | LoopElem Loop | FunctionElem Function | InvalidDirection deriving (Eq)
newtype Conditional = Conditional (MapElement, Direction) deriving (Eq)
newtype Function = Function (Direction, Direction, Direction) deriving (Eq)
newtype Loop = Loop (Int, Direction, Direction) deriving (Eq)
type Map = [[MapElement]]
type Location = (Int, Int)
type Path = [Location]

instance Show Conditional where
    show (Conditional (mapElement, direction)) = "Cond{" ++ show mapElement ++ "}{" ++ show direction ++ "}"

instance Show MapElement where
    show Grass = "*"
    show PathBlock = "-"
    show Bonus = "b"
    show Ball = "@"
    show Pink = "p"
    show Orange = "o"
    show Yellow = "y"
    show Target = "t"

instance Show Direction where
    show Down = "Down"
    show Up = "Up"
    show KodableUtils.Right = "Right"
    show KodableUtils.Left = "Left"
    show (ConditionalElem c) = show c
    show (LoopElem l) = show l
    show (FunctionElem f) = show f
    show InvalidDirection = "Invalid"

instance Show Function where
    show (Function (direction1, direction2, direction3)) = "with " ++ show direction1 ++ " " ++ show direction2 ++ " " ++ show direction3 

instance Show Loop where
    show (Loop (n, direction1, direction2)) = "Loop{" ++ show n ++ "}{" ++ show direction1 ++ "," ++ show direction2 ++ "}"

isConditionalElem :: Direction -> Bool
isConditionalElem (ConditionalElem _) = True
isConditionalElem _ = False

isLoopElem :: Direction -> Bool
isLoopElem (LoopElem _) = True
isLoopElem _ = False

fromConditionalElem :: Direction -> (MapElement, Direction)
fromConditionalElem (ConditionalElem (Conditional (mapElement, direction))) = (mapElement, direction)
fromConditionalElem _ = (InvalidElement, InvalidDirection)

stringToDirection :: String -> Direction
stringToDirection inpString
    | inpString == "Down" = Down
    | inpString == "Up" = Up
    | inpString == "Right" = KodableUtils.Right
    | inpString == "Left" = KodableUtils.Left
    | runParser conditionalParser inpString /=[] = ConditionalElem (fst(head(runParser conditionalParser inpString)))
    | runParser loopParser inpString /=[] = LoopElem (fst(head(runParser loopParser inpString)))
    | otherwise = InvalidDirection

simpleDirectionParser :: Parser String
simpleDirectionParser = string "Down" +++ string "Up" +++ string "Left" +++ string "Right"

conditionalParser :: Parser Conditional
conditionalParser = do
    string "Cond{"
    colour <- string "p" +++ string "y" +++ string "o"
    string "}{"
    direction <- simpleDirectionParser
    string "}"
    return $ Conditional (charToElement colour, stringToDirection direction)
 
loopParser :: Parser Loop
loopParser = do
    string "Loop{"
    number <- string "0" +++ string "1" +++ string "2" +++ string "3" +++ string "4" +++ string "5"
    string "}{"
    direction1 <- simpleDirectionParser +++ conditionalContainedParser
    string ","
    direction2 <- simpleDirectionParser +++ conditionalContainedParser
    string "}"
    return $ Loop (read number :: Int, stringToDirection direction1, stringToDirection direction2)
    where
        conditionalContainedParser = parser(\s -> case runParser conditionalParser s of
                                            [] -> []
                                            [(x, xs)] -> [(show x, xs)]
                                            )

charToElement :: String -> MapElement
charToElement "*" = Grass
charToElement "-" = PathBlock
charToElement "b" = Bonus
charToElement "@" = Ball
charToElement "p" = Pink
charToElement "o" = Orange
charToElement "y" = Yellow
charToElement "t" = Target
charToElement  _  = InvalidElement

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

isSolvable :: Map -> Bool
isSolvable inpMap = not $ null allPaths
    where
        allPaths = concat [getPathsToTargetWithBonuses inpMap bonusNumber (fromJust $ getElementLocationInMap inpMap Ball) [] [] 0 | bonusNumber <- [0..getBonusOnMap inpMap]]

move :: Map -> Direction -> (Map, Int, Bool)
move inpMap direction
    | direction == Up && inBoard inpMap (ballRow - 1, ballColumn) && getElementAtLocation inpMap (ballRow - 1, ballColumn) `elem` movableElements = makeMove (ballRow - 1, ballColumn)
    | direction == Down && inBoard inpMap (ballRow + 1, ballColumn) && getElementAtLocation inpMap (ballRow + 1, ballColumn) `elem` movableElements = makeMove (ballRow + 1, ballColumn)
    | direction == KodableUtils.Right && inBoard inpMap (ballRow, ballColumn + 1) && getElementAtLocation inpMap (ballRow, ballColumn + 1) `elem` movableElements = makeMove (ballRow, ballColumn + 1)
    | direction == KodableUtils.Left && inBoard inpMap (ballRow, ballColumn - 1) && getElementAtLocation inpMap (ballRow, ballColumn - 1) `elem` movableElements = makeMove (ballRow, ballColumn - 1)
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


getNextElementInDirection :: Map -> Direction -> Maybe MapElement
getNextElementInDirection inpMap direction
    | direction == Up && inBoard inpMap (ballRow - 1, ballColumn) = Just $ getElementAtLocation inpMap (ballRow - 1, ballColumn)
    | direction == Down && inBoard inpMap (ballRow + 1, ballColumn) = Just $ getElementAtLocation inpMap (ballRow + 1, ballColumn)
    | direction == KodableUtils.Right && inBoard inpMap (ballRow, ballColumn + 1) = Just $ getElementAtLocation inpMap (ballRow, ballColumn + 1)
    | direction == KodableUtils.Left && inBoard inpMap (ballRow, ballColumn - 1) = Just $ getElementAtLocation inpMap (ballRow, ballColumn - 1)
    | otherwise = Nothing
    where
        (ballRow, ballColumn) = fromJust (getElementLocationInMap inpMap Ball)

neighboursInDirection :: Map -> Location -> Location -> [Location]
neighboursInDirection inpMap (parentX, parentY) (currX, currY)
    | currentIsCondition || hasReachedEnd = neighbours inpMap (currX, currY)
    | otherwise = [nextNode]
    where
        nextNode = getNextInDirection (parentX, parentY) (currX, currY)
        currentIsCondition = isConditional (getElementAtLocation inpMap (currX, currY))
        hasReachedEnd = not (inBoard inpMap nextNode) || getElementAtLocation inpMap (getNextInDirection (parentX, parentY) (currX, currY)) == Grass
        getNextInDirection (parentRow, parentColumn) (currRow, currColumn)
            | parentRow - currRow == 1 = (currRow - 1 , currColumn) -- Going up
            | parentRow - currRow == -1 = (currRow + 1 , currColumn) -- Going down
            | parentColumn - currColumn == 1 = (currRow, currColumn - 1) -- Going left
            | parentColumn - currColumn == -1 = (currRow, currColumn + 1) -- Going right

putElemAtLocation :: Map -> Location -> MapElement -> Map
putElemAtLocation inpMap (a,b) toPut = take a inpMap ++ [addPathBlock (inpMap !! a) b] ++ drop (a + 1) inpMap
    where
        addPathBlock row col = take col row ++ [toPut] ++ drop (col+1) row

getPathsToTargetWithBonuses :: Map -> Int -> Location -> Path -> [(Location, Int)] -> Int -> [Path]
getPathsToTargetWithBonuses inpMap requiredBonuses currNode currPath alreadyVisitedWithBonusCount bonusesCollected
    | getElementAtLocation inpMap currNode == Target && bonusesCollected==requiredBonuses = [currPath++[currNode]] -- Base case where we have found the target and have visited the max bonus(es)
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

decodeDirections :: [Direction] -> [Direction]
decodeDirections = concatMap decodeDirection
    where
        decodeDirection (FunctionElem (Function(direction1, direction2, direction3))) = [direction1, direction2, direction3]
        decodeDirection (LoopElem (Loop(n, direction1, direction2))) = concat $ replicate n [direction1, direction2]
        decodeDirection direction = [direction]

stringifyPath :: Map -> Path -> String
stringifyPath inpMap path = appendDirections (map addDirectionStrings $ reverse (getReducedArray (reverse (getRawString inpMap path))))
    where
        getReducedArray [x] = [x]
        getReducedArray [x,y] = if snd x/= snd y then x:[y] else [x]
        getReducedArray (x:y:zs) = if snd x/= snd y then x:getReducedArray (y:zs) else getReducedArray (y:zs)
        getRawString inpMap [x,y] = [(getElementAtLocation inpMap x, getIndividualDirection x y)]
        getRawString inpMap (x:y:zs) = (getElementAtLocation inpMap x, getIndividualDirection x y): getRawString inpMap (y:zs) 
        addDirectionStrings (mapElement, direction) = if isConditional mapElement then (mapElement, show (Conditional (mapElement, direction))) else (mapElement, show direction)
        appendDirections directions = unwords $ map snd directions
