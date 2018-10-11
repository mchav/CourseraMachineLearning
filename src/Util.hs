-- Utilities for Machine learning
module Util where


import qualified Data.Vector.Storable as V
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import Graphics.Gnuplot.Simple
import Numeric.LinearAlgebra

import qualified Statistics.Sample as Stats

nums :: Int -> Int -> Double -> Matrix Double
nums m n i = reshape n ((m * n) |> repeat i)

zeros, ones :: Int -> Int -> Matrix Double
zeros m n = nums m n 0
ones  m n = nums m n 1

columnWise :: (V.Vector Double -> Double) -> Matrix Double -> Matrix Double
columnWise f x = row $ map f (toColumns x)

mean, std :: Matrix Double -> Matrix Double
mean = columnWise Stats.mean
std = columnWise Stats.stdDev

plot :: [Attribute] -> Vector Double -> Vector Double -> IO ()
plot attr x y = plotPathsStyle attr [(defaultStyle {plotType = Points, lineSpec = CustomStyle [LineType 2]}, zip (toList x) (toList y))]

plotPoints :: [Double] -> [Double] -> Plot2D.T Double Double
plotPoints x y = Plot2D.function Graph2D.points x lookupValue
  where lookupValue v = maybe 0 id (lookup v (zip x y))

plotLine :: [Double] -> [Double] -> Plot2D.T Double Double
plotLine x y = Plot2D.function Graph2D.lines x lookupValue
  where lookupValue v = maybe 0 id (lookup v (zip x y))


plotFunction :: [Double] -> (Double -> Double) -> Plot2D.T Double Double
plotFunction x f = Plot2D.function Graph2D.lines x f


broadcast :: (Double -> Double) -> Matrix Double -> Matrix Double
broadcast f m = fromRows $ map (cmap f) (toRows m)

-- Type synonym for a cost function.
type Cost =  Matrix Double            -- Parameters
          -> Matrix Double            -- Features
          -> Matrix Double            -- Labels
          -> (Double, Matrix Double)  -- (Cost, Optimal theta)

-- Stochastic gradient descent.
sgd :: Matrix Double -- Features
    -> Matrix Double -- Labels
    -> Matrix Double -- Parameters
    -> Cost          -- Cost function
    -> Double        -- Learning Rate
    -> Int           -- Number of Iterations
    -> (Double, Matrix Double)
sgd x y theta f alpha iterations
  | iterations > 0   = sgd x y theta' f alpha (iterations - 1)
  | otherwise        = (cost , theta)
    where (cost, grad) = f theta x y
          theta' = theta - (scalar alpha * (tr grad))
