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
    | [Invalid] `elem` attemptedMap = (Nothing, "Invalid element found in map") -- Invalid element in map
    | isNothing $ getElementLocationInMap attemptedMap Ball = (Nothing, "No ball in map") -- No ball in map
    | isNothing $ getElementLocationInMap attemptedMap Target = (Nothing, "No target in map") -- No ball in map
    | otherwise = (Just attemptedMap, "")
    where
        attemptedMap = [if Invalid `elem` map charToElement (words line) then [Invalid] else map charToElement (words line) | line <- lines content]
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
                                        putStrLn $ "There is no path where we can collect all " ++ show (getBonusOnMap unJustMap) ++ " bonuses on this map"
                        else
                            do
                                putStrLn "This map is not solvable"
                else
                    do
                        putStrLn $ "No map loaded for " ++ head (words playerInp)
        kodableMenu inpMap False    

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