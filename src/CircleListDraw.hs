module CircleListDraw where

import Control.Monad
import Data.List (any, sortOn)
import CircleList
import Graphics.WorldTurtle hiding (red, green, blue)
import qualified Graphics.Gloss.Data.Picture as G
import qualified Graphics.Gloss.Data.Color as G (makeColorI)

-- | Custom colors for our circle list rendering.
red, green, blue :: Color
red = G.makeColorI 192 1 20 255
green = G.makeColorI  0 166 76 255
blue = G.makeColorI 51 103 185 255

-- | Generates a circle of a specific color with a textual representation of
--   the given number placed inside of it.
circleNumber :: Color -> Int -> G.Picture
circleNumber c n = G.pictures 
  [ G.color black $ G.circleSolid 110
  , G.color c $ G.circleSolid 100
  , G.translate (-50) (-50) $ G.color white $ G.text $ show n
  ]

-- Stamps a circle-number of the given color and integer at the current turtle 
-- position.
stampPrisoner :: Color -> Int -> TurtleCommand ()
stampPrisoner c n = branch $ do
  setHeading 0 -- Ensure text is printed at correct orientation.
  setRepresentation $ circleNumber c n -- Change the turtle shape.
  stamp -- Stamp the turtle to the canvas.

-- Converts the CircleList to a regular polygon with each vertex labelled and 
-- color coded as a prisoner. 
drawCircleList :: CircleList Int -> TurtleCommand ()
drawCircleList cs = do
  let prisoners = indexList cs
  let angle = 360.0 / fromIntegral (length prisoners)
  branch $ drawEdges 1000 angle (length prisoners)
  branch $ drawPrisoners 1000 angle prisoners

-- | Converts CircleList into an ordered list of prisoner indices and their 
--   corresponding color (based on where in the CircleList the data is found.)
indexList :: CircleList Int -> [(Int, Color)]
indexList (CL left x right) = 
  sortOn fst $ (x, green) : zip right (repeat blue) ++ zip left (repeat red)

-- Draws the prisoner vertices of the regular polygon. 
-- I.E. the numbered, colored circles.
drawPrisoners :: Float -> Float -> [(Int, Color)] -> TurtleCommand ()
drawPrisoners side_length angle prisoners = do
  setHeading angle
  forM_ prisoners $ \(i, c) -> do
    setVisible True
    stampPrisoner c i
    setVisible False
    fd side_length
    lt angle

-- Draws the edges of the regular polygon.
-- I.E. the lines between the circles.
drawEdges :: Float -> Float -> Int -> TurtleCommand ()
drawEdges _ _ 1 = return ()
drawEdges side_length angle n = branch $ do
  setHeading angle
  setPenDown True
  replicateM_ n $ do 
    fd side_length
    lt angle

-- Draws each CircleList in sequence, letting each CircleList be rendered for 
-- 1 second before moving onto the subsequent CircleList.
animatePrisoners :: [CircleList Int] -> WorldCommand ()
animatePrisoners cls = do
  t <- makeTurtle
  t >/> do
    setVisible False
    setPenSize 10
    setSpeed 0
    setRotationSpeed 0
    setPenDown False
  forM_ cls $ \ cl -> do
    clear 
    t >/> drawCircleList cl
    sleep 1
