{-# LANGUAGE GHC2021 #-}

module Benign.TimeStats where

import Benign qualified
import Control.Exception
import Debug.TimeStats qualified as TimeStats
import System.IO.Unsafe (unsafePerformIO)

-- | @'measure' label strat thing@ measures, in the style of
-- 'TimeStats.measurePure', of running @strat thing@.
measure :: String -> Benign.Strat a -> a -> a
measure label strat thing = unsafePerformIO $ TimeStats.measureM label $ do Benign.E <- evaluate (strat thing); return thing
{-# NOINLINE measure #-}
