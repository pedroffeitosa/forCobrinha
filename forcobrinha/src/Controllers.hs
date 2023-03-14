{-# LANGUAGE PatternSynonyms #-}

module Controllers where

import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import State
import Util
import System.Exit
import Snake
import Data.Char
import Hangman
import Menu


-- Pattern for Key Event
pattern KeyHeld k <- EventKey k Down _ _ -- x y
pattern KeyHeldUp k <- EventKey k Up _ _
pattern SKeyHeld k <- KeyHeld (SpecialKey k)
pattern SKeyHeldUp k <- KeyHeldUp(SpecialKey k)
pattern CKeyHeld k <- KeyHeld (Char k)

-- Glossary Diretions for a given key
keyToDiretion :: Key -> Maybe Directions
keyToDiretion (SpecialKey KeyUp)  = Just UP
keyToDiretion (SpecialKey KeyLeft) = Just LEFT
keyToDiretion (SpecialKey KeyDown) = Just DOWN
keyToDiretion (SpecialKey KeyRight) = Just RIGHT
keyToDiretion _ = Nothing

-- MenuKeys
keyToDiretionMenu :: Key -> Maybe Directions
keyToDiretionMenu (SpecialKey KeyUp)  =  Just UP
keyToDiretionMenu (SpecialKey KeyDown) = Just DOWN
keyToDiretionMenu _ = Nothing

-- Glossary Decision for a given key
keyToDecision :: Key -> Maybe Decisions
keyToDecision (SpecialKey KeyEsc) = Just EXIT
keyToDecision (SpecialKey KeySpace) = Just PAUSE
keyToDecision (SpecialKey KeyBackspace) = Just BACK
keyToDecision (SpecialKey KeyEnter) = Just ACCEPT
keyToDecision _ = Nothing

-- Handles

-- Menu handle
handleMenuKey :: Event -> State -> State
handleMenuKey (CKeyHeld 'q') state = setDecision BACKMENU state
handleMenuKey (SKeyHeld KeyEnter) state = setDecision ACCEPT state
handleMenuKey (SKeyHeldUp KeyEnter) state = setDecision DEFAULT state
handleMenuKey (KeyHeld k) state =  verifyDirMenu (keyToDiretion k) state
handleMenuKey (KeyHeldUp k) state =  setDirection Nothing state
handleMenuKey _ state = state

verifyDirMenu :: Maybe Directions -> State -> State
verifyDirMenu diretions state = if getDirection  state == diretions
                                then state
                                else setDirection diretions state

-- Snake handle
handleSnakeKey :: Event -> State -> State
handleSnakeKey (CKeyHeld 'q') state = if getPaused state then setDecision BACKMENU state else state
handleSnakeKey (SKeyHeld KeySpace) state = setPaused (not $ getPaused state) state
handleSnakeKey (SKeyHeldUp KeyBackspace) state = setDecision DEFAULT state
handleSnakeKey (KeyHeld k) state = verifyDirSnake (keyToDiretion k) state
handleSnakeKey _ state = state

-- Hangman handle
handleHangmanKey :: Event -> State -> State
handleHangmanKey (CKeyHeld k) state = verifyHangmanDecision k state
handleHangmanKey _ state = state

-- Handler Manager
handleKey :: Event -> State -> State
handleKey event state
    | menuScreen state = handleMenuKey event state
    | not (control state) && not (menuScreen state)  = handleHangmanKey event state
    | control state && not (menuScreen state) = handleSnakeKey event state
    | otherwise = state


verifyDirSnake :: Maybe Directions -> State -> State
verifyDirSnake diretion state
    | isNothing diretion = state
    | direction (getSnake  state) == fromJust diretion = state
    | otherwise = setDirection diretion state

verifyHangmanDecision :: Char -> State -> State
verifyHangmanDecision letterKicked state =
    setHangman newHangman state
    where newHangman = hangmanInput letterKicked (getHangman state)

