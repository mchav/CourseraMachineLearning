{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module LinearRegression where

import Numeric.LinearAlgebra
import Prelude hiding ((<>))

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
                -> Matrix Double -- Lables
                -> Matrix Double -- Parameters
                -> Double        -- Learning Rate
                -> Int           -- Number of iterations
                -> Matrix Double
gradientDescent _ _ theta _     0        = theta
gradientDescent x y theta alpha numIters = gradientDescent x y theta' alpha (numIters - 1)
  where theta' = theta - step
        step = (scalar alpha) * ((tr' x) <> ((x <> theta) - y)) / (scalar m)
        m = fromIntegral ((fst . size) y)

gradientDescent' :: Matrix Double -- Features
                 -> Matrix Double -- labels
                 -> Matrix Double -- theta
                 -> Double        -- alpha
                 -> Double        -- tolerance
                 -> [Double]      -- cost history
                 -> ([Double], Matrix Double)
gradientDescent' x y theta alpha tolerance hist
  | cost > tolerance = gradientDescent' x y theta' alpha tolerance hist'
  | otherwise        = (hist' , theta)
    where cost  = computeCost x y theta
          hist' = cost : hist
          theta' = theta - step
          step = (scalar alpha) * ((tr' x) <> ((x <> theta) - y)) / (scalar m)
          m = fromIntegral ((fst . size) y)




