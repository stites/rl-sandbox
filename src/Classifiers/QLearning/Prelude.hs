module Classifiers.QLearning.Prelude
  ( module Prelude
  , module Control.Monad.Primitive
  , module Statistics.Distribution
  , module Statistics.Distribution.Normal
  , module System.Random.MWC
  , module Data.Sequence
  , module GHC.Float
  , module Control.Exception
  , module Data.List
  , module Data.Vector
  ) where

import Control.Monad.Primitive
import Control.Exception (assert)
import Data.List (intercalate)
import Data.Vector (Vector, (!), (//))
import Data.Sequence (Seq, (<|), (|>))
import Statistics.Distribution
import Statistics.Distribution.Normal
import System.Random.MWC
import GHC.Float

import Prelude


