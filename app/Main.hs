module Main where

import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import LinearRegression
import Numeric.LinearAlgebra
import Prelude hiding ((<>))
import System.Directory
import Util

-- replace this with the path to your data.
exerciseOneDataOne :: IO String
exerciseOneDataOne = do
  home <- getHomeDirectory
  return (home ++ "/code/haskell/CourseraMachineLearning/app/ex1data1.txt")

exerciseOneDataTwo :: IO String
exerciseOneDataTwo = do
  home <- getHomeDirectory
  return (home ++ "/code/haskell/CourseraMachineLearning/app/ex1data2.txt")

exerciseOne = do
  -- Part 1: Basic function
  putStrLn "Running warmUpExercise ..."
  putStrLn "5x5 Identity Matrix: "
  let a = ident 5 :: Matrix Double
  print a

  -- Part 2: Plotting
  putStrLn "Plotting Data ..."
  dataOne <- exerciseOneDataOne
  [x', y'] <- fmap toColumns (loadMatrix dataOne)
  GP.plotSync DefaultTerm.cons (plotPoints (toList x') (toList y'))

  -- Part 3: Cost and Gradient Descent
  let m = size y'
  let x = ones m 1 ||| reshape 1 x'
  let y = asColumn y'
  let xPoints = toList x'
  let yPoints = toList y'
  let theta = zeros 2 1
  let iterations = 1500
  let alpha = 0.01
  putStrLn "Testing the cost function ..."
  let j = computeCost x y theta
  putStrLn ("With theta = [0, 0]\n Cost computed = " ++ show j)
  putStrLn "Expected cost value (approx) 32.07"
  let k = computeCost x y (asColumn $ 2 |> [-1, 2])
  putStrLn ("With theta = [-1, 2]\n Cost computed = " ++ show k)
  putStrLn "Expected cost value (approx) 54.24"

  putStrLn "Running Gradient Descent ..."
  let thetaOpt = gradientDescent  x y theta alpha iterations
  putStrLn ("Theta found by gradient descent: " ++ show thetaOpt)
  putStrLn "Expected theta values (approx)\n"
  putStrLn " -3.6303\n  1.1664"
  let yOptPoints = (toList (flatten $ x <> thetaOpt))
  GP.plotSync DefaultTerm.cons $ plotLine xPoints yOptPoints `mappend` plotPoints xPoints yPoints

  -- Predicting values
  let predict1 = row [1, 3.5] <> thetaOpt
  putStrLn "For population = 35,000 we predict a profit of "
  print ((predict1 * 1000) `atIndex` (0,0))

  let predict2 = row [1, 20] <> thetaOpt
  putStrLn "For population = 70,000 we predict a profit of "
  print ((predict2 * 1000) `atIndex` (0,0))

  -- Visualising J(theta_0, theta_1)
  let theta0 = toList (linspace 100 (-10 :: Double, 10 :: Double))
  let theta1 = toList (linspace 100 (-1  :: Double, 4  :: Double))
  let jVals = [computeCost x y (col [i, j]) | i <- theta0, j <- theta1]
  -- TODO(mchav): Draw contour plot as well.
  GP.plotSync DefaultTerm.cons $ Plot3D.surface theta0 theta1 (\i j -> computeCost x y (col [i, j]))

exerciseOneOptional = do
  dataTwo <- exerciseOneDataTwo
  columns <- fmap toColumns (loadMatrix dataTwo)
  let y' = last columns
  let y = asColumn y'
  let x' = fromColumns (init columns)
  let m = size y'

  let (xNorm, mu, sigma) = featureNormalize x'
  let x = ones m 1 ||| xNorm

  let theta = zeros 3 1
  let iterations = 50
  let alpha = 1

  -- TODO(mchav): Include cost history in gradient descent function and then plot it.
  let thetaOpt = gradientDescent x y theta alpha iterations
  putStrLn ("Theta found by gradient descent: " ++ show thetaOpt)
  putStrLn "Predicted price of a 1650 sq-ft, 3 br house (using gradient descent)"
  let xTest = ones 1 1 ||| ((row [1650, 3] - mu) / sigma)
  print (xTest <> thetaOpt)
  let x'' = ones m 1 ||| x'
  let normTheta = normalEquation x'' y
  print normTheta
  let xTest' = ones 1 1 ||| row [1650, 3]
  print (xTest' <> normTheta)

main = do
  exerciseOne
  exerciseOneOptional

