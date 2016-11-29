module Classifiers.QLearning.Prelude
  ( module Prelude
  , module Control.Monad.Primitive
  , module Statistics.Distribution
  , module Statistics.Distribution.Normal
  , module System.Random.MWC
  , Prelude.show
  , Control.Exception.assert
  , Data.List.intercalate
  , (Data.Vector.!)
  , (Data.Vector.//)
  ) where

import Control.Monad.Primitive
import Control.Exception (assert)
import Data.List (intercalate)
import Data.Vector ((!), (//))
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC

import Prelude


