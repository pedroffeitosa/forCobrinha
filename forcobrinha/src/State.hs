module State 
        ( State (..)
        , initialState
        , setSnake
        , setHangman
        , setDirection
        , setFood
        , setPaused
        , setGameOver
        , setGameWin
        , setSeed
        , setControl
        , setScore
        , setRunMenu
        , setMenu
        , setDecision
        , resetState
        ) where

import Snake
import Hangman
import Graphics.Gloss.Interface.Pure.Game
import System.Exit
import Util
import Boards
import Food
import Data.Maybe
import Menu
import System.Random (StdGen)


data State =
     State{ getSnake     :: Snake
          , getDirection :: Maybe Directions  
          , getHangman   :: Hangman
          , getFood      :: Maybe Food
          , getPaused    :: Bool
          , getBoard     :: [Board]
          , getOver      :: Bool
          , getWin       :: Bool
          , seed         :: StdGen
          , control      :: Bool
          , score        :: Int
          , menuScreen   :: Bool
          , menu         :: Menu
          , getDecision  :: Decisions
          , screenDecision :: Decisions
          }

initialState :: StdGen -> Hangman -> State 
initialState gen hangman = State initialSnake (Just RIGHT) hangman Nothing False [sBoard gameBoard, hBoard gameBoard] False False gen True 0 True initalMenu DEFAULT MENU

{- 
initialState :: Maybe Food -> State
initialState food = State initialSnake RIGHT food False [sBoard gameBoard, hBoard gameBoard] False
-}

setSnake :: Snake -> State -> State
setSnake snake state = state {getSnake = snake}

setHangman :: Hangman -> State -> State
setHangman hangman state = state {getHangman = hangman}

setFood :: Maybe Food -> State -> State
setFood food state = state {getFood = food}

setDirection :: Maybe Directions -> State -> State
setDirection dir state = state {getDirection = dir}

setPaused :: Bool -> State -> State
setPaused paused state = state {getPaused = paused}

setDecision :: Decisions -> State -> State
setDecision decision state = state {getDecision = decision}

setGameOver :: Bool -> State -> State
setGameOver over state = state {getOver = over}

setGameWin :: Bool -> State -> State
setGameWin win state = state {getWin = win}

setSeed :: StdGen -> State -> State
setSeed newSeed state = state {seed = newSeed}

setControl :: Bool -> State -> State
setControl newControl state = state {control = newControl} 

setScore :: Int -> State -> State
setScore value state = state {score = value}

setRunMenu :: Bool -> State -> State
setRunMenu decision state = state {menuScreen = decision}

setMenu :: Menu -> State -> State
setMenu newMenu state = state {menu = newMenu}

resetState :: State -> State
resetState state = newState
        where 
                newHangman = initialStateHangman' (getHangman state)
                newState = initialState (seed state) newHangman


