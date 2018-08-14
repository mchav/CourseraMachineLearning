module LinearRegression where

import Numeric.LinearAlgebra
import Prelude hiding ((<>))

computeCost :: Matrix Double -> Vector Double -> Matrix Double -> Double
computeCost x y theta = (sumElements squareDifference) / (2.0 * m)
  where m = fromIntegral (size y)
        h = x <> theta
        squareDifference = (h - (asColumn y)) ^ 2

gradientDescent :: Matrix Double -> Vector Double -> Matrix Double -> Double -> Int -> Matrix Double
gradientDescent _ _ theta _     0        = theta
gradientDescent x y theta alpha numIters = gradientDescent x y theta' alpha (numIters - 1)
  where theta' = theta - step
        step = (scalar alpha) * ((tr' x) <> ((x <> theta) - (asColumn y))) / (scalar m) :: Matrix Double
        m = fromIntegral (size y)
