{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------------------
-- Tabular Q-Learning Algorithms for prototypes
-------------------------------------------------------------------------------
module Classifiers.QLearning where

import qualified Data.Vector as V

import Classifiers.QLearning.Prelude

type Qs = Vector Reward
type Counts = Vector Integer
type Rewards = Vector NormalDistribution

type Reward = Float
type Action = Int
type History = Seq (Action, Reward)

type Bandit m = (PrimMonad m)

newtype Interval =
  Interval { runInterval :: Float }
  deriving Eq

instance Show Interval where
  show = show . runInterval

toInterval :: Float -> Interval
toInterval f = Interval $ assert (f >= 0 && f <= 1) f

data QLearning = QLearn
  { alpha   :: Interval
  , gamma   :: Interval
  , epsilon :: Interval
  , epsilonFn :: Interval -> Integer -> Interval
  , trials  :: Integer
  , actions :: Int
  , states  :: Int
  }

instance Show QLearning where
  show (QLearn a g e _ t as ss) = "QLearn {" ++ intercalate ", "
    [ "alpha: "   ++ show a
    , "epsilon: " ++ show e
    , "gamma: "   ++ show g
    , "trials: "  ++ show t
    , "actions: " ++ show as
    , "states: "  ++ show ss
    ] ++ " }"

instance Eq QLearning where
  (QLearn a g e _ t as ss) == (QLearn a' g' e' _ t' as' ss') =
    a == a' && g == g' && e == e' && t == t' && as == as' && ss == ss'

initQs :: Int -> Qs
initQs len = V.replicate len 0

initCounts :: Int -> Counts
initCounts len = V.replicate len 0

initRewards :: Bandit m => Gen (PrimState m) -> Int -> m Rewards
initRewards gen len =
  return . V.fromList $ map (`normalDistr` 0.5) [5 .. lastIdx]
  where
    lastIdx :: Double
    lastIdx = 5 + fromIntegral len - 1


simpleBanditConfig :: QLearning
simpleBanditConfig =
  QLearn (toInterval 0.75) (toInterval 0) (toInterval 0) epsFn nTrials 10 1
  where
    nTrials :: Integer
    nTrials = 500

    epsFn :: Interval -> Integer -> Interval
    epsFn _ n = toInterval (0.99 ** fromIntegral n)

simpleBandits :: forall m . Bandit m
              => Gen (PrimState m) -> QLearning -> m (History, Action)
simpleBandits gen q@(QLearn alp gma eps epsFn iters arms _) = do
  rewards <- initRewards gen arms
  history <- go rewards iters mempty (initQs arms) (initCounts arms)
  let optimal = V.maxIndex (V.map mean rewards)
  return (history, optimal)
  where
    go :: Bandit m => Rewards -> Integer -> History -> Qs -> Counts -> m History
    go _ 0 history _ _ = return history
    go rewards countdown hist qs counts = do
      a <- choose gen (argMax qs) arms (runInterval $ epsFn eps (iters - countdown))
      r <- cashIn gen rewards a
      let counts' = nextCounts a
          q' = calcQ q counts' qs a r
      go rewards (countdown - 1) (hist |> (a, q')) (nextQs a q') counts'
      where
        nextCounts :: Action -> Counts
        nextCounts = bumpCounts counts

        nextQs :: Action -> Reward -> Qs
        nextQs = updateQs qs

choose :: Bandit m => Gen (PrimState m) -> Action -> Int -> Float -> m Action
choose gen a arms eps = do
  p  <- uniform gen
  a' <- uniformR (0, arms - 1) gen
  return (if p <= eps then a' else a)

argMax :: Qs -> Action
argMax  = V.maxIndex

bumpCounts :: Counts -> Action -> Counts
bumpCounts counts a = counts // [(a, 1 + (counts ! a))]

updateQs :: Qs -> Action -> Reward -> Qs
updateQs qs a r = qs // [(a, r)]

-- new estimate is difference between old estimate and target (ie, the error)
-- times by the step size, added to the old estimate.
calcQ :: QLearning -> Counts -> Qs -> Action -> Reward -> Reward
calcQ (QLearn (Interval alph) (Interval gma) _ _ _ _ _) counts qs a nextRwd =
  oldVal + alph * (nextRwd + gma * maxQ - oldVal)
  where
    oldVal :: Reward
    oldVal = qs ! a

    maxQ :: Float
    maxQ = V.maximum qs


cashIn :: Bandit m => Gen (PrimState m) -> Rewards -> Action -> m Reward
cashIn gen rwds a = do
  rwd <- genContVar (rwds ! a) gen
  return (double2Float rwd)


