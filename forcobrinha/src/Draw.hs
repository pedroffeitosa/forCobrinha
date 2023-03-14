module Draw where

import Snake
import Graphics.Gloss
import State
import Boards
import Food
import Data.Maybe
import Util
import Hangman
import Menu


snakePicture :: Picture
snakePicture = drawSnake viewSnake initialSnake

{- 
    Objetives: 
                - Move the begin of the center of grid to the Upper Left 
                - Translate to -60 (position of grid of snake game in the window)
-}
translator :: View -> Coordinate -> Picture -> Picture
translator view (x,y) = Translate x1 y1
  where
    grid    = gridView view
    gw      = gridWidth grid
    gh      = gridHeight grid
    pixel   = pixelView view
    pw      = pixelWidth pixel
    ph      = pixelHeight pixel
    x1      =  x*pw - gw / 2 + pw / 2
    y1      = gh / 2  - ph / 2 - (y * ph) -60


drawRect :: View -> Coordinate -> Picture
drawRect  view (x, y) = translator view (x,y) (rectangleSolid width height)
  where
    pixel  = pixelView view
    width  = pixelWidth pixel
    height = pixelHeight pixel

drawSnake :: View -> Snake -> Picture
drawSnake view snake = Pictures (h : t)
  where
    headColor = head (colorList view)
    bodyColor = last (colorList view)
    h         = Color headColor $ drawRect view (snakeHeadPos snake)
    t         = map (Color bodyColor . drawRect view) (snakeBodyPos snake)

drawFood :: View -> Maybe Food -> Picture
drawFood view food = if isJust food
                     then Pictures [fig]
                     else Pictures [blank]
  where
    colorFood = head (colorList view)
    fig =  Color colorFood $ drawRect view (foodPosition (fromJust food))

drawState :: State -> Picture
drawState state
  | (screenDecision state) == RECORD = Pictures [drawMaximumScore]
  | (screenDecision state) == CREATORS = Pictures [drawCreators]
  | menuScreen state && (screenDecision state) == MENU = Pictures [drawMenu (menu state)]
  | getOver state = Pictures [background, grid, sP, hangman, fP, drawGameOver, score]
  | getWin state = Pictures [background, grid, sP, hangman, fP, drawGameWin, score]
  | getPaused state = Pictures [background, sP, hangman, fP, drawPause, score]
  | not (control state) = Pictures [background, grid, sP, hangman, fP, drawEnterLetter,score]
  | otherwise = Pictures [background, grid, sP, hangman, fP, score]
  where
      s = getSnake state
      f = getFood state
      background = drawForCobrinha
      sP = drawSnake viewSnake s
      fP = drawFood viewFood  f
      score = drawScore state
      grid = translate (-207) (78) $ drawGridSnake (getHangman state)
      hangman = translate (-200) (150) $ renderHangman (getHangman state)


drawEnterLetter :: Picture
drawEnterLetter = pictures
        [color black  (translate (-135) (92) (scale 0.2 0.2 (text "Please enter a letter")))]

drawScore :: State -> Picture
drawScore state = Pictures [fig1, fig2]
  where
    fig1 = color black  (translate (240) (180) (scale 0.2 0.2 (text "Score:")))
    fig2 = color black  (translate (240) (140) (scale 0.2 0.2 (text (show (score state)))))

-- draw the background of the game
drawForCobrinha :: Picture
drawForCobrinha = pictures [sBoardBack gameBoardBackground, hBoardBack gameBoardBackground]

-- Draw the Win, Lose and Pause

-- Game Over
drawGameOver :: Picture
drawGameOver = pictures
        [rect, color red  (translate (-110) (-0)(scale 0.3 0.3 (text "Game Over")))]
        where rect = color (dark white) $ translate (0) (11) $ rectangleSolid 250 60

-- Game Win
drawGameWin :: Picture
drawGameWin = pictures
        [rect, color black  (translate (-100) (0)(scale 0.3 0.3 (text "Game Win")))]
        where rect = color (dark white) $ translate (0) (11) $ rectangleSolid 250 60


drawCreators :: Picture
drawCreators = pictures
        [color black  (translate (-300) (0)(scale 0.2 0.2 (text "Gabriel Cavalcanti Leandro de Lima"))),
        color black  (translate (-300) (-50)(scale 0.2 0.2 (text "Renaldo Silva Santino"))),
        color black  (translate (-300) (-100)(scale 0.1 0.1 (text "Press char 'q' to go back to Menu")))]

drawMaximumScore :: Picture
drawMaximumScore = pictures
        [color black  (translate (-200) (0)(scale 0.3 0.3 (text "Maximum Score: 6"))),
         color black  (translate (-300) (-100)(scale 0.1 0.1 (text "Press char 'q' to go back to Menu")))]

-- Game Pause
drawPause :: Picture
drawPause = pictures [ rect, paused, pressKeySpace, pressKeyBack]
  where
    paused   = color red  (translate (-130) (-0)(scale 0.3 0.3 (text "Game Paused")))
    pressKeySpace = color black (translate (-100) (-30) (scale 0.1 0.1 (text "Press SpaceBar to back to play")))
    pressKeyBack  = color black (translate (-130) (-60) (scale 0.1 0.1 (text "Press char 'q' to go back to Menu")))
    rect = color (dark white) $ translate (5) (-20) $ rectangleSolid 290 200

-- Snake Grid
drawGridSnake :: Hangman -> Picture
drawGridSnake hangman = pictures [node x y | x <- [0,pWidth..(gWidth - pWidth)], y <- [0,pHeight..(gHeight - pHeight)]]
  where
    pixel = pixelSnake
    pWidth = floor $ pixelWidth pixel
    pHeight = floor $ pixelHeight pixel

    grid = sizeGridSnake
    gWidth = floor $ gridWidth grid
    gHeight = floor $ gridHeight grid

    node x y = Color (if even ((x `div` pWidth) + (y `div` pHeight))
      then dark $ dark $ selectColor hangman
      else dark $ selectColor hangman) $
      translate (fromIntegral x) (-fromIntegral y) $ rectangleSolid 18 12


selectColor :: Hangman -> Color
selectColor hangman =
  if even (level hangman)
    then green
    else yellow
