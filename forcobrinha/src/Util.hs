module Util where

import Graphics.Gloss
import System.Random
import Window


-- --------------------------------------------------- -- 

-- Coordinate of something
type Coordinate = (Float, Float)

-- --------------------------------------------------- -- 

-- Inputs
data Directions =  UP | DOWN | LEFT | RIGHT deriving (Eq)
data Decisions = DEFAULT | ACCEPT | MENU | START | CREATORS | RECORD | PAUSE | EXIT | BACK | BACKMENU deriving (Eq)
nameGame = "ForCobrinha" :: String

-- --------------------------------------------------- -- 

-- Sizes

-- SizeWindow
data SizeGrid =
    SizeGrid{ gridWidth  :: Float
            , gridHeight :: Float
            }

-- SizePixel
data SizePixel =
    SizePixel{ pixelWidth  :: Float
             , pixelHeight :: Float
             }

 -- Default Sizes

sizeGridDefault =
    SizeGrid {gridWidth = windowWidth defaultConfigWindow, gridHeight = windowHeight defaultConfigWindow}
sizeGridSnake   =
    SizeGrid {gridWidth = 432, gridHeight = 288}
sizeGridHangman =
    SizeGrid {gridWidth = 432, gridHeight = 96}

sizePixelDefault =
    SizePixel {pixelWidth = 9, pixelHeight = 6}
pixelSnake =
    SizePixel {pixelWidth  = 18, pixelHeight = 12}
pixelFood =
    SizePixel {pixelWidth  = 18, pixelHeight = 12}
-- --------------------------------------------------- -- 

-- Board
data Board =
    Board { sizeGrid        :: SizeGrid
          , translateXPos   :: Float
          , translateYPos   :: Float
          , boardColor      :: Color
          }

-- Default Board

boardDefault =
    Board { sizeGrid        = sizeGridDefault
          , translateXPos   = 0
          , translateYPos   = 0
          , boardColor      = dark (dark green)
          }

-- --------------------------------------------------- -- 

-- View

data View =
    View { pixelView   :: SizePixel
         , gridView    :: SizeGrid
         , colorList   :: [Color]
         }

-- --------------------------------------------------- -- 

-- Game Board

data GameBoardBackGround =
    GameBoardBackGround {sBoardBack   :: Picture
                        ,hBoardBack   :: Picture
                        }
data GameBoard =
    GameBoard { sBoard   :: Board
              , hBoard   :: Board
              }

-- --------------------------------------------------- -- 

-- Functions:

-- -- Backgroud

-- -- -- Make tha picture of the backgroung
createBackground :: Board -> Picture
createBackground o  = translate (translateXPos o) (translateYPos o) fig
    where
        width  = gridWidth (sizeGrid o)
        height = gridHeight (sizeGrid o)
        fig    = color (boardColor o) (rectangleSolid width height)

-- -- Random Number

-- -- -- Generator for a  RandomNumber
getRandomNumber :: StdGen -> Int -> Int
getRandomNumber gen limit = x
    where
        (x, g') = randomR (0, limit -1) gen

getRandomNumber' :: StdGen -> Float -> Float -> Float
getRandomNumber' gen limitx limity = x
    where
        (x, g') = randomR (limitx, limity -1) gen

-- -- -- New seed
makeNewSeed :: StdGen -> Float -> StdGen
makeNewSeed gen limit = g'
    where
        (x, g') = randomR (0, limit -1) gen

-- --------------------------------------------------- -- 
