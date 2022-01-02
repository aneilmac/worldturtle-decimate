module Main where

import CircleList
import CircleListDraw
import Graphics.WorldTurtle

main :: IO ()
main = runWorld $ animatePrisoners $ romanHistory 6 3
