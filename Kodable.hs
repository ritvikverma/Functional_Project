module Kodable where

-- This file is the menu and the user interface for hints, play, quit, solve, and check

import Data.List
import KodableData
import Data.Maybe
import System.Directory
import Data.Char
import System.IO
import KodableUtils
import Control.Monad
import Control.Concurrent

import System.Console.ANSI

import Prelude hiding (Left, Right)

printMapElement :: MapElement -> IO()
printMapElement mapElement = do
    if mapElement == Grass then do 
        setSGR [SetColor Foreground Vivid Green]
    else if mapElement == PathBlock then do
        setSGR [SetColor Foreground Vivid Blue]
    else if mapElement == Bonus then do
        setSGR [SetColor Foreground Vivid Red]
    else if mapElement == Ball then do
        setSGR [SetColor Foreground Vivid White]
    else if mapElement == Pink then do
        setSGR [SetColor Foreground Vivid Magenta]
    else if mapElement == Orange then do
        setSGR [SetColor Foreground Dull System.Console.ANSI.Yellow]
    else if mapElement == KodableData.Yellow then do
        setSGR [SetColor Foreground Vivid System.Console.ANSI.Yellow]
    else if mapElement == Target then do
        setSGR [SetColor Foreground Vivid Cyan]
    else do
        setSGR [Reset]        
    putStr $ show mapElement ++ " "

printRow :: [MapElement] -> IO()
printRow [x] = printMapElement x
printRow (x:xs) = do
    printMapElement x
    printRow xs

printMap :: Map -> IO()
printMap [x] = do
    printRow x
    putStrLn " "
    setSGR [Reset]
printMap (x:xs) = do
    printRow x
    putStrLn " "
    printMap xs

kodable :: IO () -- Introductory function, takes you to the main menu
kodable = kodableMenu Nothing True

load :: Maybe Map -> String -> IO() -- Menu function for loading assistance
load inpMap playerInp =
    if length (words playerInp) /= 2 then
        do
            putStrLn "Invalid number of arguments for load"    
            kodableMenu inpMap False       
    else
        do
            let fileName = filter (/='"') $ words playerInp !! 1
            exists <- doesFileExist fileName
            if exists then
                do
                    handle <- openFile fileName ReadMode
                    contents <- hGetContents handle
                    case fst $ loadMap contents of
                        Nothing -> do
                            putStrLn $ snd $ loadMap contents
                            kodableMenu inpMap False
                        playableMap -> do
                            putStrLn "Read map successfully!"
                            putStrLn "Initial: "
                            printMap (fromJust playableMap)
                            kodableMenu playableMap False
                    hClose handle                            
            else                        
                do
                    putStrLn $ "File " ++ fileName ++ " does not exist!"
                    kodableMenu inpMap False

solve :: Maybe Map -> String -> IO() -- Menu function for solving assistance
solve inpMap playerInp = 
    do
        if null (words playerInp) || length (words playerInp) > 2 then 
            do
                putStrLn "Invalid number of arguments for solve"    
        else
            do
                if isJust inpMap then
                    do
                        let unJustMap = fromJust inpMap
                        let bonusesOnMap = getBonusOnMap unJustMap
                        let allPaths = concat [getPathsToTargetWithBonuses unJustMap bonusNumber (fromJust $ getElementLocationInMap unJustMap Ball) [] [] 0 | bonusNumber <- [0..bonusesOnMap]]
                        let pathsWithMaximumBonuses = getPathsToTargetWithBonuses unJustMap bonusesOnMap (fromJust $ getElementLocationInMap unJustMap Ball) [] [] 0
                        if not $ null allPaths then
                            do
                                if length (words playerInp) == 2 then
                                    do
                                        let customBonus = words playerInp !! 1
                                        if not (all isDigit customBonus) then do
                                            putStrLn "Please input a number as second argument for custom bonus traversal"
                                        else if (read customBonus :: Int) < 0 then do
                                            putStrLn "Number of bonus(es) collected must be positive"
                                        else if (read customBonus :: Int) > bonusesOnMap then do
                                            putStrLn $ "There is/are only " ++ show bonusesOnMap ++ " bonus(es) on this map"
                                        else do
                                            let pathsWithCustomBonus = getPathsToTargetWithBonuses unJustMap (read customBonus :: Int) (fromJust $ getElementLocationInMap unJustMap Ball) [] [] 0
                                            if null pathsWithCustomBonus then do
                                                putStrLn $ "There are no paths with " ++ show (read customBonus :: Int) ++ " bonus(es) on this map"
                                            else
                                                putStrLn $ "The shortest path with " ++ show (read customBonus :: Int) ++ " bonus(es) is: " ++ stringifyPath(shortestPath unJustMap pathsWithCustomBonus)

                                else do
                                    putStrLn $ "The shortest path if we do not care about the bonus(es) is: " ++ stringifyPath (shortestPath unJustMap allPaths)

                                if not $ null pathsWithMaximumBonuses then
                                    do
                                        putStrLn $ "The shortest path with all the bonus(es) is: " ++ stringifyPath (shortestPath unJustMap pathsWithMaximumBonuses)
                                        putStrLn $ "The bonus(es) on this path are: " ++ show bonusesOnMap
                                else
                                    do
                                        putStrLn $ "There is no path where we can collect all " ++ show bonusesOnMap ++ " bonus(es) on this map"
                        else
                            do
                                putStrLn "This map is not solvable"
                else
                    do
                        putStrLn "No map loaded for solve"
        kodableMenu inpMap False    

applyDirection :: Map -> Direction -> [Direction] -> IO(Map, Bool) -- Applies the direction and returns the updated map and if we have won
applyDirection inpMap direction nextDirection = do
    clearScreen
    printMap inpMap
    putStrLn " "
    threadDelay 100000
    when (bonusesCollectedOnThisMove > 0) $ putStrLn (show bonusesCollectedOnThisMove ++ " bonu(es) collected! ")
    if hasWon then return (mapAfterNextMove, True)
    else if mapAfterNextMove == inpMap then return (mapAfterNextMove, hasWon)
    else if not (null nextDirection) && isConditional (head nextDirection) then do
        let (mapElementOfConditional, directionOfConditional) = fromConditional (head nextDirection)
        let nextElementInDirection = getNextElementInDirection inpMap direction
        if nextElementInDirection == Just mapElementOfConditional then do
            applyDirection mapAfterNextMove directionOfConditional nextDirection
        else do
            applyDirection mapAfterNextMove direction nextDirection
    else 
        applyDirection mapAfterNextMove direction nextDirection
    where
        (mapAfterNextMove, bonusesCollectedOnThisMove, hasWon) = move inpMap direction

playGame :: Map -> Bool -> [Direction] -> [Direction] -> IO() -- Plays the game if we enter play in the menu
playGame inpMap throughHint [] predefinedFunction = do
    playInput inpMap False throughHint [] predefinedFunction 
playGame inpMap throughHint (x:xs) predefinedFunction = do
    (mapAfterDirection, hasWon) <- if null xs then applyDirection inpMap x [] else applyDirection inpMap x [head xs]
    if hasWon then do
        printMap mapAfterDirection
        putStrLn "Congratulations! You win the game!"
        kodableMenu Nothing True
    else if mapAfterDirection == inpMap then do
        putStrLn "Sorry! You have inputted an invalid move. "
        putStrLn "Your current board: "
        printMap inpMap
        playInput inpMap False False [] predefinedFunction
    else do
        playGame mapAfterDirection throughHint xs predefinedFunction

playInput :: Map -> Bool -> Bool -> [Direction] -> [Direction] -> IO() -- Input function for play 
playInput inpMap firstTime throughHint directions predefinedFunction = do
    let bonusesOnMap = getBonusOnMap inpMap
    let allPaths = concat [getPathsToTargetWithBonuses inpMap bonusNumber (fromJust $ getElementLocationInMap inpMap Ball) [] [] 0 | bonusNumber <- [0..bonusesOnMap]]
    let pathsWithMaximumBonus = getPathsToTargetWithBonuses inpMap bonusesOnMap (fromJust $ getElementLocationInMap inpMap Ball) [] [] 0
    let overAllShortestPath = shortestPath inpMap allPaths
    let pathWithMaxBonus = shortestPath inpMap pathsWithMaximumBonus
    if throughHint then do
        if null pathsWithMaximumBonus then do
            putStrLn "We cannot collect all bonuses at this point in this map"
        else do
            putStrLn $ "If you want to collect the most bonuses, how about: " ++ head (words $ hintyPath pathWithMaxBonus)
        if null allPaths then do
            putStrLn "Error! Target is unreachable. "
            kodableMenu Nothing True
        else do
            putStrLn $ "To reach the target fastest, how about: " ++ head (words $ hintyPath overAllShortestPath)
        
        playInput inpMap firstTime False directions predefinedFunction
    else do
        when (firstTime && null directions) $ putStr "First direction: "
        when (firstTime && not (null directions)) $ putStrLn "First direction: Function"
        when (firstTime && not (null directions) || not firstTime) $ putStr "Next direction: "
        playerInp <- getLine
        if null playerInp then do
            playGame inpMap False (decodeDirections directions) predefinedFunction
        else if head (words playerInp) `elem` ["hint", "Hint"] then do
            playGame inpMap True directions predefinedFunction
        else if head (words playerInp) == "Function" then do
            if null predefinedFunction then do
                putStrLn "There is no predefined function."
                playInput inpMap firstTime False directions predefinedFunction
            else
                playInput inpMap False False (directions++predefinedFunction) predefinedFunction
        else if length (words playerInp) > 1 || stringToDirection (head $ words playerInp)  == InvalidDirection then do
            putStrLn "Invalid arguments for direction. Make sure you are complying with the rules of the game."
            playInput inpMap firstTime False directions predefinedFunction
        else do
            let inputtedValidDirection = stringToDirection (head $ words playerInp) 
            playInput inpMap False False (directions++[inputtedValidDirection]) predefinedFunction

play :: Maybe Map -> String -> IO() -- Argument handling function for play
play inpMap playerInp = 
    do
        if isNothing inpMap then do
            putStrLn "No map loaded for play"
            kodableMenu inpMap False
        else if not $ isSolvable $ fromJust inpMap then do
            putStrLn "This map is unsolvable!"
            kodableMenu inpMap False
        else if length (words playerInp) `notElem` [1,4] then do
            putStrLn "Invalid number of arguments for play"
            kodableMenu inpMap False          
        else if length (words playerInp) == 4 then do
            if InvalidDirection `elem` map stringToDirection (tail (words playerInp)) then do
                putStrLn "Invalid direction for function call"
                kodableMenu inpMap False
            else if any (isLoop . stringToDirection) (tail (words playerInp))  then do
                putStrLn "A function cannot contain a loop inside"
                kodableMenu inpMap False
            else do
                playInput (fromJust inpMap) True False (map stringToDirection (tail (words playerInp))) (map stringToDirection (tail (words playerInp)))
        else
            playInput (fromJust inpMap) True False [] []

kodableMenu :: Maybe Map -> Bool -> IO() -- Main menu of our Kodable game
kodableMenu inpMap playingFirstTime = do
    putStrLn " "
    when playingFirstTime $ putStrLn "Welcome to Kodable!"
    putStrLn "Game Menu: "
    putStrLn "1) Load File: Input 'load <filename>'"
    putStrLn "2) Check: Input 'check' if a map is already loaded"
    putStrLn "3) Solve: Input 'solve' if a map is already loaded"
    putStrLn "4) Quit: Input 'quit' to quit the game"
    putStrLn "5) Play: Input 'play' to play Kodable"
    putStrLn " "
    playerInp <- getLine
    if null $ words playerInp then
        do
            putStrLn "Please enter a command"
            kodableMenu inpMap False

    else if head (words playerInp) `notElem` ["load", "check", "solve", "quit", "play"] then
        do
            putStrLn "Invalid command"
            kodableMenu inpMap False

    else if head (words playerInp) == "load" then 
        do load inpMap playerInp

    else if head (words playerInp) == "solve" then
        do solve inpMap playerInp
    
    else if head (words playerInp) == "play" then
        do play inpMap playerInp

    else if length (words playerInp) /= 1 then
        do
            putStrLn $ "Invalid number of arguments for " ++ head (words playerInp)
            kodableMenu inpMap False  

    else if head (words playerInp) == "check" then
        do
            if isJust inpMap then
                do
                    if isSolvable $ fromJust inpMap then putStrLn "This map is solvable" else putStrLn "This map is not solvable"
            else
                do
                    putStrLn $ "No map loaded for " ++ head (words playerInp)
            kodableMenu inpMap False     

    else if head (words playerInp) == "quit" then
        do
            putStrLn "Quitting Kodable. Thanks for playing!\n"
    else
        do
            kodableMenu inpMap False