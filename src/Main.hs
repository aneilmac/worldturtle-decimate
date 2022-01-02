module Main where

import CircleListDraw
import Graphics.WorldTurtle

main :: IO ()
main = runWorld $ animatePrisoners 6 3

