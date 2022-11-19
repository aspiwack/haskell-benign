{-# LANGUAGE GHC2021 #-}

module Benign.TimeStats where

import Benign qualified
import Debug.TimeStats qualified as TimeStats
import System.IO.Unsafe (unsafePerformIO)

measure :: Benign.Eval a => String -> a -> Benign.Result a
measure label thing = unsafePerformIO $ TimeStats.measureM label $ Benign.evalIO (Benign.PureEval thing)
{-# NOINLINE measure #-}
