module Kodable where

import Data.List
import Data.Maybe
import System.Directory
import Data.Char
import System.IO
import KodableUtils
import Control.Monad

kodable :: IO ()
kodable = kodableMenu Nothing True

loadMap :: String -> (Maybe Map, String)
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


load :: Maybe Map -> String -> IO()
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
                            putStrLn $ printMap (fromJust playableMap)
                            kodableMenu playableMap False
                    hClose handle                            
            else                        
                do
                    putStrLn $ "File " ++ fileName ++ " does not exist!"
                    kodableMenu inpMap False

solve :: Maybe Map -> String -> IO()
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
                                                putStrLn $ "The shortest path with " ++ show (read customBonus :: Int) ++ " bonus(es) is: " ++ unwords (head $ sortOn length $ map (words . stringifyPath unJustMap) pathsWithCustomBonus)

                                else do
                                    putStrLn $ "The shortest path if we do not care about the bonus(es) is: " ++ unwords (head $ sortOn length $ map (words . stringifyPath unJustMap) allPaths)

                                if not $ null pathsWithMaximumBonuses then
                                    do
                                        putStrLn $ "The shortest path with all the bonus(es) is: " ++ unwords (head $ sortOn length $ map (words . stringifyPath unJustMap) pathsWithMaximumBonuses)
                                        putStrLn $ "The bonus(es) on this path are: " ++ show (getBonusOnMap unJustMap)
                                else
                                    do
                                        putStrLn $ "There is no path where we can collect all " ++ show (getBonusOnMap unJustMap) ++ " bonus(es) on this map"
                        else
                            do
                                putStrLn "This map is not solvable"
                else
                    do
                        putStrLn "No map loaded for solve"
        kodableMenu inpMap False    


applyDirection :: Map -> Direction -> IO(Map, Bool)
applyDirection inpMap direction = do
    when (bonusesCollectedOnThisMove > 0) $ putStrLn (show bonusesCollectedOnThisMove ++ " bonu(es) collected! ")
    if hasWon then return (mapAfterNextMove, True)
    else if mapAfterNextMove == inpMap then return (mapAfterNextMove, hasWon)
    else applyDirection mapAfterNextMove direction
    where
        (mapAfterNextMove, bonusesCollectedOnThisMove, hasWon) = move inpMap direction


playGame :: Map -> [Direction] -> IO()
playGame inpMap [] = do
    putStrLn $ printMap inpMap
    playInput inpMap False []    
playGame inpMap (x:xs) = do
    (mapAfterDirection, hasWon) <- applyDirection inpMap x
    if hasWon then do
        putStrLn "Congratulations! You win the game!"
        putStrLn $ printMap mapAfterDirection
        kodableMenu Nothing True
    else if mapAfterDirection == inpMap then do
        putStrLn "Sorry! You have inputted an invalid move. "
        putStrLn "Your current board: "
        putStrLn $ printMap inpMap
        playInput inpMap False []
    else do
        playGame mapAfterDirection xs



playInput :: Map -> Bool -> [Direction] -> IO()
playInput inpMap firstTime directions = do
    if firstTime && null directions then do
        putStr "First Direction: "
        playerInp <- getLine
        if length (words playerInp) /= 1 || stringToDirection (head $ words playerInp) == InvalidDirection then do
            putStrLn "Invalid arguments for first direction. Make sure you are complying with the rules of the game."
            playInput inpMap True directions
        else do
            let inputtedValidDirection = stringToDirection (head $ words playerInp) 
            playInput inpMap False (directions++[inputtedValidDirection])
    else do
        when (firstTime && not (null directions)) $ putStrLn "First Direction: Function"
        putStr "Next direction: "
        playerInp <- getLine
        if null $ words playerInp then do
            playGame inpMap (decodeDirections directions) 
        else if length (words playerInp) > 1 || stringToDirection (head $ words playerInp)  == InvalidDirection then do
            putStrLn "Invalid arguments for direction. Make sure you are complying with the rules of the game."
            playInput inpMap False directions
        else do
            let inputtedValidDirection = stringToDirection (head $ words playerInp) 
            playInput inpMap False (directions++[inputtedValidDirection])  


play :: Maybe Map -> String -> IO()
play inpMap playerInp = 
    do
        if isNothing inpMap then do
            putStrLn "No map loaded for play"
            kodableMenu inpMap False
        else if length (words playerInp) `notElem` [1,4] then do
            putStrLn "Invalid number of arguments for play"
            kodableMenu inpMap False          
        else if length (words playerInp) == 4 && InvalidDirection `elem` map stringToDirection (tail (words playerInp)) then do
            putStrLn "Invalid arguments for initial function call"
            kodableMenu inpMap False
        else if length (words playerInp) == 4 then do
            playInput (fromJust inpMap) True (map stringToDirection (tail (words playerInp)))
        else
            playInput (fromJust inpMap) True []
                

kodableMenu :: Maybe Map -> Bool -> IO()
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
                    let allPaths = concat [getPathsToTargetWithBonuses (fromJust inpMap) bonusNumber (fromJust $ getElementLocationInMap (fromJust inpMap) Ball) [] [] 0 | bonusNumber <- [0..getBonusOnMap (fromJust inpMap)]]
                    if not $ null allPaths then putStrLn "This map is solvable" else putStrLn "This map is not solvable"
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