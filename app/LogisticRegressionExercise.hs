module LogisticRegressionExercise where

import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

import qualified Data.ByteString.Lazy as B
import Data.Csv.HMatrix
import Data.Csv.Incremental
import HVX
import LogisticRegression
import Numeric.LinearAlgebra
import Prelude hiding ((<>))
import System.Directory
import Util

-- replace this with the path to your data.
exerciseTwoDataOne :: IO String
exerciseTwoDataOne = do
  home <- getHomeDirectory
  return (home ++ "/code/haskell/CourseraMachineLearning/app/ex2data1.txt")

exerciseTwoDataTwo :: IO String
exerciseTwoDataTwo = do
  home <- getHomeDirectory
  return (home ++ "/code/haskell/CourseraMachineLearning/app/ex2data2.txt")

costFunction :: Matrix Double -> Matrix Double -> Matrix Double -> (Double, Matrix Double)
costFunction theta x y = ((sumElements cost) / m, grad)
  where sig = sigmoid $ x <> theta
        m = fromIntegral $ fst (size y)
        n = snd (size x)
	-- Cost function for logistic regression. Faithful formula translation.
        cost = (((-1) * (tr y)) <> (log sig)) - ((1 - (tr y)) <> (log (1 - sig)))
	-- Gradient. Faithful translation of the formula.
        grad = (columnWise sumElements (x * repmat (sig - y) 1 n)) / (scalar m)

plotData :: Matrix Double -> IO (Plot2D.T Double Double)
plotData completeMatrix = do
  let matrixRows = toRows completeMatrix
  -- select all rows whose third column is 1.
  let pos = fromRows $ filter (((==) 1) . (flip atIndex 2)) matrixRows
  -- do the same for those whose third column is 0.
  let neg = fromRows $ filter (((==) 0) . (flip atIndex 2)) matrixRows
  -- Helper function: Convert column vectors to lists for plotting.
  let getPointLists p = map toList $ toColumns (p ?? (All, Take 2))
  let [xPos, yPos] = getPointLists pos
  let [xNeg, yNeg] = getPointLists neg
  return $ plotPoints xPos yPos `mappend` plotPoints xNeg yNeg

plotDecisionBoundary :: Matrix Double -> Matrix Double -> IO (Plot2D.T Double Double)
plotDecisionBoundary theta x = do
  let theta' = flatten theta
  -- We only need two points to plot a line.
  -- So we pick the smallest and largest.
  let lowerPoint = minimum ((toList . flatten) $ x ¿ [1]) - 2
  let upperPoint = maximum ((toList . flatten) $ x ¿ [1]) + 2
  let plotX = [lowerPoint, upperPoint]
  -- A helper function to calculate the y values given by our theta.
  let f val = (-1) / (atIndex theta' 2) * ((atIndex theta' 1) * val + (atIndex theta' 0))
  let plotY = [f lowerPoint, f upperPoint]
  return $ plotLine plotX plotY

exerciseTwo = do
  dataOne <- exerciseTwoDataOne
  contents <- B.readFile dataOne
  let completeMatrix = decodeMatrix NoHeader contents
  let x'' = completeMatrix ?? (All {- Rows -}, Take 2 {- Columns -})
  let y = completeMatrix ¿ [2]
  let (m, n) = size x''
  let x = ones m 1 ||| x''
  let initialTheta = zeros (n + 1) 1
  let (cost, grad) = costFunction initialTheta x y
  putStrLn $ "Cost at initial theta (zeros): " ++ (show cost)
  putStrLn $ "Expected cost (approx): 0.693"
  putStrLn $ "Gradient at initial theta (zeros): " ++ (show grad)
  putStrLn $ "Expected gradients (approx):\n -0.1000\n -12.0092\n -11.2628\n"
  -- Compute and display cost and gradient with non-zero theta.
  -- the learning rate was achieved through empirical tests.
  -- Gives theta closest to fminunc.
  let (_, theta) = sgd x y initialTheta costFunction 0.0041 1000000
  decisionBoundary <- plotDecisionBoundary theta x
  points <- plotData completeMatrix
  GP.plotSync DefaultTerm.cons $ decisionBoundary `mappend` points

