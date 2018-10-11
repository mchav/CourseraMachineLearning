module LogisticRegression where

import Numeric.LinearAlgebra

sigmoid :: Matrix Double -> Matrix Double
sigmoid x = fromRows $ map (cmap sigmoid') (toRows x)

-- sigmoid applied to a single real number.
sigmoid' :: Double -> Double
sigmoid' x = 1 / (1 + (exp (-1 * x)))
