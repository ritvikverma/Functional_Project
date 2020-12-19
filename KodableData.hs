module KodableData where

-- This file provides the core implementation of Kodable, defining all the data structures and their corresponding overrides

import Parser
import Prelude

data MapElement = PathBlock | Grass | Bonus | Target | Pink | Orange | Yellow | Ball | InvalidElement deriving (Eq)
data Direction = Down | Up | Right | Left | ConditionalElem Conditional | LoopElem Loop | FunctionElem Function | InvalidDirection deriving (Eq)
newtype Conditional = Conditional (MapElement, Direction) deriving (Eq)
newtype Function = Function (Direction, Direction, Direction) deriving (Eq)
newtype Loop = Loop (Int, Direction, Direction) deriving (Eq)
type Map = [[MapElement]]
type Location = (Int, Int)
type Path = [Location]

instance Show Conditional where -- Show override for Conditionals
    show (Conditional (mapElement, direction)) = "Cond{" ++ show mapElement ++ "}{" ++ show direction ++ "}"

instance Show MapElement where -- Show override for MapElements
    show Grass = "*"
    show PathBlock = "-"
    show Bonus = "b"
    show Ball = "@"
    show Pink = "p"
    show Orange = "o"
    show Yellow = "y"
    show Target = "t"

instance Show Direction where -- Show override for Directions
    show Down = "Down"
    show Up = "Up"
    show KodableData.Right = "Right"
    show KodableData.Left = "Left"
    show (ConditionalElem c) = show c
    show (LoopElem l) = show l
    show (FunctionElem f) = show f
    show InvalidDirection = "Invalid"

instance Show Function where -- Show override for Functions
    show (Function (direction1, direction2, direction3)) = "with " ++ show direction1 ++ " " ++ show direction2 ++ " " ++ show direction3 

instance Show Loop where -- Show override for Loops
    show (Loop (n, direction1, direction2)) = "Loop{" ++ show n ++ "}{" ++ show direction1 ++ "," ++ show direction2 ++ "}"


printMap :: Map -> String -- Takes a map, and prints it
printMap [x] = unwords (map show x)
printMap (x:xs) = unwords (map show x) ++ "\n" ++ printMap xs

isConditionalElem :: Direction -> Bool -- Tells whether this direction is a ConditionalElem
isConditionalElem (ConditionalElem _) = True
isConditionalElem _ = False

isLoopElem :: Direction -> Bool -- Tells whether this direction is a LoopElem
isLoopElem (LoopElem _) = True
isLoopElem _ = False

fromConditionalElem :: Direction -> (MapElement, Direction) -- Retreives the tuple contained within the ConditionalElem
fromConditionalElem (ConditionalElem (Conditional (mapElement, direction))) = (mapElement, direction)
fromConditionalElem _ = (InvalidElement, InvalidDirection)

stringToDirection :: String -> Direction -- Takes a string, and returns the corresponding direction after running parsers if needed
stringToDirection inpString
    | inpString == "Down" = Down
    | inpString == "Up" = Up
    | inpString == "Right" = KodableData.Right
    | inpString == "Left" = KodableData.Left
    | runParser conditionalParser inpString /=[] = ConditionalElem (fst(head(runParser conditionalParser inpString)))
    | runParser loopParser inpString /=[] = LoopElem (fst(head(runParser loopParser inpString)))
    | otherwise = InvalidDirection

simpleDirectionParser :: Parser String -- Runs a series of string parsers to check if this direction is valid
simpleDirectionParser = string "Down" +++ string "Up" +++ string "Left" +++ string "Right"

conditionalParser :: Parser Conditional -- Runs a series of string parsers to check if this is a valid condition
conditionalParser = do
    string "Cond{"
    colour <- string "p" +++ string "y" +++ string "o"
    string "}{"
    direction <- simpleDirectionParser
    string "}"
    return $ Conditional (charToElement colour, stringToDirection direction)
 
loopParser :: Parser Loop -- Runs a series of string parsers to check if this a valid loop
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
        conditionalContainedParser = parser(\s -> case runParser conditionalParser s of -- Parser that runs the conditional parser and outputs a string result
                                            [] -> []
                                            [(x, xs)] -> [(show x, xs)]
                                            )

charToElement :: String -> MapElement -- Takes a character, and returns the corresponding MapElement
charToElement "*" = Grass
charToElement "-" = PathBlock
charToElement "b" = Bonus
charToElement "@" = Ball
charToElement "p" = Pink
charToElement "o" = Orange
charToElement "y" = Yellow
charToElement "t" = Target
charToElement  _  = InvalidElement