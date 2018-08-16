{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module LinearRegression where

import qualified Data.Vector as V
import Numeric.LinearAlgebra
import Prelude hiding ((<>))
import Util

data HyperParameters = HyperParam { theta :: Matrix Double
                                  , alpha :: Double
                                  , maxIterations :: Int
                                  , tolerance :: Double }

computeCost :: Matrix Double -- Features
            -> Matrix Double -- Labels
            -> Matrix Double -- Theta
            -> Double
computeCost x y theta = (sumElements squareDifference) / (2.0 * m)
  where m = fromIntegral ((fst . size) y)
        h = x <> theta
        squareDifference = (h - y) ^ 2

-- A simple gradient descent algorithm that specifies an arbitrary number of iterations.
gradientDescent :: Matrix Double -- Features
                -> Matrix Double -- Labels
                -> Matrix Double -- Parameters
                -> Double        -- Learning Rate
                -> Int           -- Number of iterations
                -> ([Double], Matrix Double)
gradientDescent x y theta alpha numIters = gradientDescent' x y theta alpha numIters []

gradientDescent' :: Matrix Double -- Features
                 -> Matrix Double -- Labels
                 -> Matrix Double -- Parameters
                 -> Double        -- Learning Rate
                 -> Int           -- Number of Iterations
                 -> [Double]      -- Cost history
                 -> ([Double], Matrix Double)
gradientDescent' x y theta alpha iterations hist
  | iterations > 0   = gradientDescent' x y theta' alpha (iterations - 1) hist'
  | otherwise        = (hist' , theta)
    where cost  = computeCost x y theta
          hist' = cost : hist
          theta' = theta - step
          step = (scalar alpha) * ((tr' x) <> ((x <> theta) - y)) / (scalar m)
          m = fromIntegral ((fst . size) y)

featureNormalize :: Matrix Double -> (Matrix Double, Matrix Double, Matrix Double)
featureNormalize x = ((x - mu) / sigma, mu, sigma)
  where mu = mean x
        sigma = std x

normalEquation :: Matrix Double -> Matrix Double -> Matrix Double
normalEquation x y = (pinvTol 1e-2 (tr' x <> x)) <> (tr' x) <> y

