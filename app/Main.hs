module Main where

import Control.Monad
import Graphics.Gnuplot.Simple
import LinearRegression (computeCost, gradientDescent)
import Numeric.LinearAlgebra
import Util (plot, zeros, ones)

-- replace this with the path to your data.
exerciseOneDataOne :: String
exerciseOneDataOne = "<app-directory>/ex1data1.txt"

a :: Matrix Double
a = ident 5

xLabel = XLabel "Population of the City in 10'000s"
yLabel = YLabel "Profit in $10'000s"

exerciseOne = do
  -- Part 1: Basic function
  putStrLn "Running warmUpExercise ..."
  putStrLn "5x5 Identity Matrix: "
  print a
  -- Part 2: Plotting
  putStrLn "Plotting Data ..."
  [x', y] <- liftM toColumns (loadMatrix exerciseOneDataOne)
  plot [xLabel, yLabel] x' y
  -- Part 3: Cost and Gradient Descent
  let m = size y
  let x = (ones m 1) ||| (reshape 1 x')
  let theta = zeros 2 1
  let iterations = 1500
  let alpha = 0.01
  putStrLn "Testing the cost function ..."
  print $ computeCost x y theta
  let theta' = (asColumn $ 2 |> [-1, 2])
  print $ computeCost x y theta'
  print $ gradientDescent x y theta alpha iterations
  
main = do
  exerciseOne
