{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use init" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Snake
       (Snake(..)
       , initialSnake
       , viewSnake
       , move
       , setDirectionHead
       , itsNotValid
       , eating
       , growning
       , decreasing
       , overllaping
       , sizeBody
       ) where

import Graphics.Gloss
import Util
import Food
import Data.Maybe

-- Snake
data Snake =
  Snake
       { snakeHeadPos :: Coordinate
       , snakeBodyPos :: [Coordinate]
       , direction :: Directions
       }

-- A view for a snake
viewSnake =
    View { pixelView = pixelSnake
         , gridView  = sizeGridSnake
         , colorList = [blue, black]
         }

-- Initial Snake         
initialSnake =
  Snake
       { snakeHeadPos = (12,12) -- middle of the game grid
       , snakeBodyPos = [(11,12), (10,12)]
       , direction = RIGHT
       }



-- Moviment
{- 
  The head it's who ditact the moviment, the body just follows 
-}
move :: Snake -> Snake
move snake  = snake {snakeHeadPos = head, snakeBodyPos = body}
 where
    ((hx, hy), dir) = (snakeHeadPos snake, direction snake)
    head
      | dir == UP = (hx, hy - 1)
      | dir == LEFT = (hx - 1, hy)
      | dir == RIGHT = (hx + 1, hy)
      | dir == DOWN = (hx, hy + 1)
    body = (hx, hy) : movebody (snakeBodyPos snake)

-- Move the body  - updtades the head of the body and remove the last element 
{- 
  Why not use "init"? Because init blow up if it's a empty list. 
    Then, you can do something like:
      pop :: [a] -> [a]
      pop [] = []
      pop xs = init xs
    or something like this: 
      pop:: [a] -> [a]
      pop = reverse . tail . reverse
-}
movebody :: [a] -> [a]
movebody = reverse . tail . reverse

-- Set a direction of the Head
setDirectionHead :: Snake -> Maybe Directions -> Snake
setDirectionHead snake diretion 
  | isNothing diretion = snake
  | otherwise = snake {direction = fromJust diretion}

-- It's inside the grid?
itsNotInTheGrid :: View -> Snake -> Bool
itsNotInTheGrid view snake = sx <= 0 || sx >= (bx / px) - 1 || sy <= 0  || sy >= (by / py) - 1
  where
    grid    = gridView view
    gWidth  = gridWidth grid
    gHeight = gridHeight grid
    pixel   = pixelView view
    pWidth  = pixelWidth pixel
    pHeight = pixelHeight pixel
    (sx, sy) = snakeHeadPos snake
    (bx, by) = (gWidth , gHeight)
    (px, py) = (pWidth , pHeight)

-- It's eating itself?
canibalism :: Snake -> Bool
canibalism snake = snakeHeadPos snake  `elem` snakeBodyPos snake

-- Bye bye little body
itsAemptyBody :: Snake -> Bool
itsAemptyBody snake 
  | null (snakeBodyPos snake) = True
  | otherwise = False


-- It's a valid snake moviment?
itsNotValid :: Snake ->  Bool
itsNotValid snake = board || canibal || onlyTheSoul
  where
    board        = itsNotInTheGrid viewSnake snake
    canibal      = canibalism snake
    onlyTheSoul  = itsAemptyBody snake


-- Eating (Nhom Nhom)
eating :: Snake -> Food -> Bool
eating snake food
  | snakeHeadPos snake == foodPosition food = True
  | otherwise = False

-- It's getting bigger. That's what she said.
growning :: Snake -> Food -> Snake
growning snake food = snake {snakeHeadPos = head, snakeBodyPos = body}
  where
    head = foodPosition food
    body = snakeHeadPos snake : snakeBodyPos snake

-- Oh no! it's not a food, it's me
overllaping :: Snake -> Coordinate -> Bool
overllaping snake coordinate
  | coordinate `elem` snakePosition = True
  | otherwise = False
  where
     snakePosition =  snakeHeadPos snake : snakeBodyPos snake

-- Sh*t, getting lesser and lesser
decreasing :: Snake -> Snake
decreasing snake = snake  {snakeHeadPos = head, snakeBodyPos = body}
  where
    head = snakeHeadPos snake
    body = movebody (snakeBodyPos snake)

-- This it's my three numbers
sizeBody :: Snake -> Int
sizeBody snake 
  | null (snakeBodyPos snake) || length(snakeBodyPos snake) <3  = 0
  | otherwise = length(snakeBodyPos snake) - length(snakeBodyPos initialSnake)
  