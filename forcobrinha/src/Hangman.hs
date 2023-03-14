{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Hangman where

import Graphics.Gloss
import Util
import System.Random
import Data.Char
import Data.List
import Data.Maybe (isJust, isNothing)
import Control.Monad (forever)



data Hangman = Hangman
    { level :: Int
    , wordLists :: [[String]]
    , currentWordList :: [String]
    , discoveredWords :: [String]
    , challenge :: Challenge
    , currentLetter :: Char
    } deriving (Eq, Show)


data Challenge = Challenge
    { gen :: StdGen
    , challengeTip :: String
    , secretWord :: String
    , discoveredLetters :: [Maybe Char]
    , kickedLetters :: [Char]
    } deriving (Eq, Show)

initialStateHangman :: StdGen -> [String] -> Hangman
initialStateHangman gen allWords = Hangman { level = firstLevel , wordLists = wordListB
                                            , currentWordList = firstList, discoveredWords = []
                                            , challenge = newChallenge
                                            , currentLetter = '#'
                                            }
    where
        firstLevel = 0
        wordListA = filterAllWords allWords
        wordListB = processWordLists wordListA ([] :: [[String]])
        firstList = wordListB !! firstLevel
        newChallenge = makeNewChallenge gen firstList []


initialStateHangman' :: Hangman -> Hangman
initialStateHangman' hangman = Hangman { level = firstLevel, wordLists = wordList
                                            , currentWordList = firstList, discoveredWords = []
                                            , challenge = newChallenge
                                            , currentLetter = '#'
                                            }
    where
        firstLevel = 0
        wordList = (wordLists hangman)
        firstList = wordList !! firstLevel
        newChallenge = makeNewChallenge (gen (challenge hangman)) firstList []


filterAllWords :: [String] -> [String]
filterAllWords allWords 
    | null allWords = []
    | last (head allWords) `notElem` ['a'..'z'] = init (head allWords) : filterAllWords (tail allWords)
    | otherwise = head allWords : filterAllWords (tail allWords)
    

processWordLists :: [String] -> [[String]] -> [[String]]
processWordLists [] wordLists = wordLists
processWordLists allWords wordLists = processWordLists newAllWords newWordLists
    where
        wordList = processOneWordList (tail allWords)
        newAllWords = tail allWords \\ wordList
        newWordLists = reverse (wordList : reverse wordLists)


processOneWordList :: [String] -> [String]
processOneWordList  allWords
    | null allWords = []
    | head allWords == "#" = []
    | otherwise = head allWords : processOneWordList (tail allWords)


renderHangman :: Hangman -> Picture
renderHangman hangman = pictures [challenge, kickedLetters, level]
    where
        texts = buildTextChallenge hangman
        challenge = translate 0 (30) $ drawText 0.1 0.1 $ head texts
        kickedLetters = drawText 0.1 0.1 $ texts !! 1
        level = translate (-140) (30) $ drawText 0.2 0.2 $ texts !! 2


buildTextChallenge :: Hangman -> [String]
buildTextChallenge (Hangman level _ _ _ challenge _) =
    ["Challenge tip: " ++ challengeTip challenge ++ "  Secret Word: \n" ++
    (intersperse ' ' $ fmap renderDiscoveredLetter (discoveredLetters challenge))] ++
    ["Kicked letters: " ++ kickedLetters challenge] ++ ["Level: " ++ show (level + 1)]


renderDiscoveredLetter :: Maybe Char -> Char
renderDiscoveredLetter Nothing  = '_'
renderDiscoveredLetter (Just letter) = letter


freshChallenge :: StdGen -> String -> String -> Challenge
freshChallenge newGen word tip = Challenge
        { gen = newGen
        , challengeTip = fmap toLower tip
        , secretWord = fmap toLower word
        , discoveredLetters = map (const Nothing) word
        , kickedLetters = []
        }


drawText :: Float -> Float -> String -> Picture
drawText scaleA scaleB msg = scale scaleA scaleB $ color black $ text msg


getWords :: IO [String]
getWords = do
    contents <- readFile filePath
    return $ lines contents
    where filePath = "src/words.txt"


getRandomWord :: StdGen -> [String] -> String
getRandomWord gen allWords = allWords !! index
    where
        index = getRandomNumber gen (length allWords)


charInWord :: String -> Char -> Bool
charInWord word char = char `elem` word


updateStateChallenge :: Hangman -> Hangman
updateStateChallenge (Hangman level wordLists currentWordList discoveredWords challenge currentLetter) =
    case (charInWord (secretWord challenge) currentLetter, charInWord (kickedLetters challenge) currentLetter) of
    (_ ,True) -> Hangman level wordLists currentWordList discoveredWords challenge '#'
    (True, _) -> Hangman level wordLists currentWordList discoveredWords updatedChallenge '#'
    (False, _) -> Hangman level wordLists currentWordList discoveredWords updatedChallenge '#'
    where
        updatedChallenge = addCharInChallenge challenge currentLetter


checkChangeChallenge :: Hangman -> Hangman
checkChangeChallenge (Hangman level wordLists currentWordList discoveredWords challenge currentLetter) =
    Hangman level wordLists currentWordList newDiscoveredWords newChallenge currentLetter
    where
        newDiscoveredWords = verifyNewDiscoveredWords discoveredWords currentWordList challenge
        newChallenge = verifyNewChallenge discoveredWords challenge currentWordList newDiscoveredWords


verifyNewDiscoveredWords :: [String] -> [String] -> Challenge -> [String]
verifyNewDiscoveredWords discoveredWords currentWordList challenge =
    if discoveredTheWord (discoveredLetters challenge) && (length (tail currentWordList) > length discoveredWords)
        then secretWord challenge : discoveredWords
        else discoveredWords


verifyNewChallenge :: [String] -> Challenge -> [String] -> [String] -> Challenge
verifyNewChallenge discoveredWords challenge currentWordList newDiscoveredWords =
    if discoveredTheWord (discoveredLetters challenge)
    then
        if length (tail currentWordList) == length newDiscoveredWords
        then challenge
        else makeNewChallenge (gen challenge) currentWordList newDiscoveredWords
    else challenge


discoveredTheWord :: [Maybe Char] -> Bool
discoveredTheWord = all isJust


makeNewChallenge :: StdGen -> [String] -> [String] -> Challenge
makeNewChallenge gen currentWordList discoveredWords = freshChallenge newGen scretWord challengeTip
    where
        scretWord = getRandomWord gen (tail currentWordList \\ discoveredWords)
        newGen = makeNewSeed gen 100.0
        challengeTip = head currentWordList


hangmanHasInput :: Hangman -> Bool
hangmanHasInput hangman = currentLetter hangman /= '#'


addCharInChallenge :: Challenge -> Char -> Challenge
addCharInChallenge (Challenge gen challengeTip secretWord discoveredLetters kickedLetters) letterKicked =
    Challenge gen challengeTip secretWord newDiscoveredLetters newKickedLetters
    where
        newKickedLetters = addCharInkickedLetters letterKicked kickedLetters
        zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
        newDiscoveredLetters = zipWith (zipper letterKicked) secretWord discoveredLetters


addCharInkickedLetters :: Char -> String -> String
addCharInkickedLetters char kickedLetters
    | isAlpha char = char : kickedLetters
    | otherwise = kickedLetters


checkChangeLevel :: Hangman -> Hangman
checkChangeLevel (Hangman level wordLists currentWordList discoveredWords challenge currentLetter)
    | (length (tail currentWordList) == length discoveredWords) && (level == length wordLists -1) =
        Hangman level wordLists currentWordList discoveredWords challenge currentLetter
    | (length (tail currentWordList) == length discoveredWords) && (level < length wordLists -1) =
        makeNewLevel (Hangman level wordLists currentWordList discoveredWords challenge currentLetter)
    | otherwise = Hangman level wordLists currentWordList discoveredWords challenge currentLetter


makeNewLevel :: Hangman -> Hangman
makeNewLevel (Hangman level wordLists currentWordList discoveredWords challenge currentLetter) =
    Hangman newLevel wordLists newCurrentList [] newChallenge '#'
    where
        newLevel = level + 1
        newCurrentList = wordLists !! newLevel
        newChallenge = makeNewChallenge (gen challenge) newCurrentList []


checkGameWin :: Hangman -> Bool
checkGameWin (Hangman level wordLists currentWordList discoveredWords challenge currentLetter) =
    level == length wordLists -1 && (length currentWordList - 1) == length discoveredWords


discoveredLettersLength :: [Maybe Char] -> Int
discoveredLettersLength [] = 0
discoveredLettersLength (x:xs) = if isJust x
                                 then 1 + discoveredLettersLength xs
                                 else 0 + discoveredLettersLength xs


hangmanInput :: Char -> Hangman -> Hangman
hangmanInput letterKicked (Hangman level wordLists currentWordList discoveredWords challenge currentLetter) =
    Hangman level wordLists currentWordList discoveredWords challenge letterKicked


updateHangman :: Hangman -> Hangman
updateHangman hangman = do
    if discoveredTheWord (discoveredLetters (challenge hangman)) && not (checkGameWin hangman)
    then checkChangeLevel $ checkChangeChallenge hangman
    else checkChangeChallenge $ updateStateChallenge hangman