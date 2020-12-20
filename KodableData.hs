module KodableData where

-- This file provides the core implementation of Kodable, defining all the data structures and their corresponding overrides

import Parser
import Prelude hiding (Left, Right)

data MapElement = PathBlock | Grass | Bonus | Target | Pink | Orange | Yellow | Ball | InvalidElement deriving (Eq)
data Direction = Down | Up | Right | Left | Conditional (MapElement, Direction) | Loop (Int, Direction, Direction) | Function (Direction, Direction, Direction) | InvalidDirection deriving (Eq)
type Map = [[MapElement]]
type Location = (Int, Int)
type Path = [Location]

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
    show Right = "Right"
    show Left = "Left"
    show (Conditional (mapElement, direction)) = "Cond{" ++ show mapElement ++ "}{" ++ show direction ++ "}"
    show (Loop (n, direction1, direction2)) = "Loop{" ++ show n ++ "}{" ++ show direction1 ++ "," ++ show direction2 ++ "}"
    show (Function (direction1, direction2, direction3)) = "Function with " ++ show direction1 ++ " " ++ show direction2 ++ " " ++ show direction3 
    show InvalidDirection = "Invalid"

printMap :: Map -> String -- Takes a map, and prints it
printMap [x] = unwords (map show x)
printMap (x:xs) = unwords (map show x) ++ "\n" ++ printMap xs

isConditional :: Direction -> Bool -- Tells whether this direction is a ConditionalElem
isConditional (Conditional _) = True
isConditional _ = False

isLoop :: Direction -> Bool -- Tells whether this direction is a LoopElem
isLoop (Loop _) = True
isLoop _ = False

fromConditional :: Direction -> (MapElement, Direction) -- Retreives the tuple contained within the ConditionalElem
fromConditional ((Conditional (mapElement, direction))) = (mapElement, direction)
fromConditional _ = (InvalidElement, InvalidDirection)

stringToDirection :: String -> Direction -- Takes a string, and returns the corresponding direction after running parsers if needed
stringToDirection inpString
    | inpString == "Down" = Down
    | inpString == "Up" = Up
    | inpString == "Right" = Right
    | inpString == "Left" = Left
    | runParser conditionalParser inpString /=[] = fst(head(runParser conditionalParser inpString))
    | runParser loopParser inpString /=[] = fst(head(runParser loopParser inpString))
    | otherwise = InvalidDirection

simpleDirectionParser :: Parser String -- Runs a series of string parsers to check if this direction is valid
simpleDirectionParser = string "Down" +++ string "Up" +++ string "Left" +++ string "Right"

conditionalParser :: Parser Direction -- Runs a series of string parsers to check if this is a valid condition
conditionalParser = do
    string "Cond{"
    colour <- string "p" +++ string "y" +++ string "o"
    string "}{"
    direction <- simpleDirectionParser
    string "}"
    return $ Conditional (charToElement colour, stringToDirection direction)
 
loopParser :: Parser Direction -- Runs a series of string parsers to check if this a valid loop
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