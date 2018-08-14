-- Utilities for Machine learning
module Util where

import Graphics.Gnuplot.Simple
import Numeric.LinearAlgebra

nums :: Int -> Int -> Double -> Matrix Double
nums m n i = reshape n ((m * n) |> repeat i)

zeros, ones :: Int -> Int -> Matrix Double
zeros m n = nums m n 0
ones  m n = nums m n 1

plot :: [Attribute] -> Vector Double -> Vector Double -> IO ()
plot attr x y = plotPathsStyle attr [(defaultStyle {plotType = Points, lineSpec = CustomStyle [LineType 2]}, zip (toList x) (toList y))]
