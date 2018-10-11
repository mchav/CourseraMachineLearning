module Main where

import LinearRegressionExercise
import LogisticRegressionExercise
import Util

import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

main = do
  -- exerciseOne
  -- exerciseOneOptional
  -- exerciseTwo
  exerciseTwo
  -- GP.plotSync DefaultTerm.cons $ plotFunction [-10.0,-9.9..10.0] (\x -> minimum [0.5, x, x * x])

