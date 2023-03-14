module Menu where

import Boards
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Util

newtype Menu = Menu {selectPosition :: Coordinate}

initalMenu = 
    Menu { selectPosition = (0,40) }

menuBoard = 
    Board { sizeGrid        = sizeGridDefault
          , translateXPos   = 0
          , translateYPos   = 0
          , boardColor      = dark green
          }

drawMenu :: Menu ->Picture
drawMenu menu = pictures [jogo, jogar, records, criadores, quit, selecao]
    where 
        jogo      = color black (translate (-75) (165) (scale 0.2 0.2 (text "ForCobrinha")))  
        jogar     = color black (translate (-40) (30) (scale 0.2 0.2 (text "JOGAR")))
        records   = color black (translate (-55) (0) (scale 0.2 0.2 (text "RECORDS")))
        criadores = color red (translate (-65) (-30) (scale 0.2 0.2 (text "CRIADORES")))
        quit      = color red (translate (-121) (-90) (scale 0.1 0.1 (text "Press Enter to select or ESC to quit")))
        selecao   = seletionRec x y 200 30 blue
        (x,y)     = selectPosition menu 

seletionRec :: Float -> Float -> Float -> Float -> Color -> Picture
seletionRec x y width height c = Color c $ translate x y  $ rectangleWire width height


moveSelection :: Menu -> Maybe Directions -> Menu
moveSelection menu dir = menu {selectPosition = coordinate}
    where 
        (hx, hy) = selectPosition menu
        coordinate
            | dir == Just UP = (hx, limite (hy + 30))
            | dir == Just DOWN = (hx, limite (hy - 30))
            | otherwise = (hx, hy)
        limite = limiteSelection 

limiteSelection :: Float -> Float
limiteSelection valor 
    | valor >  40 = -20
    | valor < -20 = 40
    | otherwise = valor


opSelectec :: Menu -> Decisions -> String
opSelectec menu decision
    | selectPosition menu == (0,40) && decision == ACCEPT  = "jogar" 
    | selectPosition menu == (0,10) && decision == ACCEPT  =  "records" 
    | selectPosition menu == (0,-20) && decision == ACCEPT  =  "criadores" 
    | otherwise = ""

