-- Utilities for Machine learning
module Util where

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import Graphics.Gnuplot.Simple
import Numeric.LinearAlgebra

nums :: Int -> Int -> Double -> Matrix Double
nums m n i = reshape n ((m * n) |> repeat i)

zeros, ones :: Int -> Int -> Matrix Double
zeros m n = nums m n 0
ones  m n = nums m n 1

plot :: [Attribute] -> Vector Double -> Vector Double -> IO ()
plot attr x y = plotPathsStyle attr [(defaultStyle {plotType = Points, lineSpec = CustomStyle [LineType 2]}, zip (toList x) (toList y))]

plotPoints :: [Double] -> [Double] -> Plot2D.T Double Double
plotPoints x y = Plot2D.function Graph2D.points x lookupValue
  where lookupValue v = maybe 0 id (lookup v (zip x y))
  
plotLine :: [Double] -> [Double] -> Plot2D.T Double Double
plotLine x y = Plot2D.function Graph2D.lines x lookupValue
  where lookupValue v = maybe 0 id (lookup v (zip x y))
