module ForCobrinha where

import Graphics.Gloss
import Draw
import Snake
import State
import Window
import Controllers
import System.Random
import Data.Maybe
import Hangman
import Food
import Menu
import Util


-- --------------------------------------------------- -- 

-- Game Step and Functions

-- Game Step
gameStep :: Float -> State -> State
gameStep _ state
  | getPaused  state = goBackToMenu state
  | (screenDecision state) == RECORD || (screenDecision state) == CREATORS = goBackToMenu state
  | menuScreen state = runMenu state
  | itsNotValid (getSnake state) = setGameOver True state
  | checkGameWin (getHangman state) = setGameWin True state
  | otherwise = newState
  where
      newState = decisionState (control state) state

goBackToMenu :: State -> State
goBackToMenu state
    | getPaused state && (getDecision state == BACK) = runMenu $ setRunMenu True state
    | (getDecision state) == BACKMENU = resetState state
    | otherwise = state

runMenu :: State -> State
runMenu state
    | opSelectec (menu state) (getDecision state) == "jogar" = setRunMenu False state
    | opSelectec (menu state) (getDecision state) == "records" = state {screenDecision = RECORD}
    | opSelectec (menu state) (getDecision state) == "criadores" = state {screenDecision = CREATORS}
    | otherwise = setMenu (moveSelection (menu state) (getDirection state)) state

decisionState :: Bool -> State -> State
decisionState boolean state = if boolean
                                then runSnake state
                                else runHangman state

runSnake state
    | itsEatingFood (doStep state) = setControl False state
    | otherwise = doStep state
    where
        doStep =  makeFood . doMoveSnake . setScore (sizeBody (getSnake state))


runHangman :: State -> State
runHangman state
    | hangmanHasInput (getHangman state) = (setHangman newHangman state) {getSnake = newSnake, getFood = Nothing, control = True}
    | otherwise = state
    where
        guessed = discoveredLetter (getHangman state)
        hangman = updateHangman (getHangman state)

        newHangman = checkChangeLevel hangman
        newSnake = getSnake (growSnake guessed state)


-- --------------------------------------------------- -- 

-- Functions:

discoveredLetter :: Hangman -> Bool
discoveredLetter hangman = discovered
    where
        myChallenge = challenge hangman
        myCurrentLetter = currentLetter hangman
        discovered = myCurrentLetter `elem` secretWord myChallenge && notElem myCurrentLetter (kickedLetters myChallenge)

guessedLetter :: Int -> Int -> Bool
guessedLetter oldNumLetters newNumLetters = newNumLetters > oldNumLetters

-- --------------------------------------------------- -- 

-- Functions:

-- Snake Functions

-- Do the move
doMoveSnake :: State -> State
doMoveSnake state = setSnake newSnake state
    where
        oldSnake = getSnake state
        diretion = getDirection state
        newSnake = move (setDirectionHead oldSnake diretion)

-- A new food here
makeFood :: State -> State
makeFood state = if isNothing(getFood state)
                then if overlapping
                     then makeFood $ setSeed newSeed state
                     else setFood (Just food) $ setSeed newSeed state
                else state
    where
        tuple = coordinateFood (seed state)
        newSeed = snd tuple
        food = moveFood (initialFood {foodPosition =  fst tuple})
        overlapping = overllaping (getSnake state) (foodPosition food)

-- Getting Bigger
growSnake :: Bool -> State -> State
growSnake decision state =
    if decision
        then setSnake newGrowSnake state
        else setSnake newSnakeDecrease state
    where
        snake = getSnake state
        food = fromJust (getFood state)
        newGrowSnake = growning snake food
        newSnakeDecrease = decreasing snake

-- Eating to Grow
itsEatingFood :: State -> Bool
itsEatingFood state
    | isNothing (getFood state) = False
    | eating snake food        = True
    | otherwise                = False
    where
        snake = getSnake state
        food  = fromJust (getFood state)

-- --------------------------------------------------- -- 

-- Settings
fps = 10 :: Int

-- --------------------------------------------------- -- 

-- GameTest

game :: IO ()
game = do
    seed <- newStdGen
    allWords <- getWords
    let hangman = initialStateHangman seed allWords
    play displayWindow (windowBackgroundColor Nothing) fps (initialState seed hangman) drawState handleKey gameStep
