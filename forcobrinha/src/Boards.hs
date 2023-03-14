module Boards where


import Graphics.Gloss
import Util
import Snake


-- Boards

-- SnakeBoard -- Default

snakeBoard = 
    Board { sizeGrid        = sizeGridSnake
          , translateXPos   = 0
          , translateYPos   = -60
          , boardColor      = dark green
          }

-- HangmanBoard -- Default
hangmanBoard = 
    Board { sizeGrid        = sizeGridHangman
          , translateXPos   = 0
          , translateYPos   = 168
          , boardColor      = dark green
          }

-- Game Board
gameBoard =
    GameBoard { sBoard = snakeBoard
              , hBoard = hangmanBoard
              }

-- --------------------------------------------------- -- 

-- Backgrounds

-- Hangman background
hangmanBackground :: Picture
hangmanBackground = createBackground hangmanBoard

-- Snake background
snakeBackground :: Picture
snakeBackground = createBackground snakeBoard

-- Game background
gameBoardBackground =
    GameBoardBackGround { sBoardBack    = snakeBackground
                        , hBoardBack    = hangmanBackground
                        }
