{-# LANGUAGE GHC2021 #-}

-- | This module emits events in the eventlog which can then be analyzed with
-- [ghc-events-analyze](https://hackage.haskell.org/package/ghc-events-analyze).
--
-- TLDR:
-- * Compile with `ghc -eventlog`
-- * Run the program as `myprogram +RTS -l`
-- * Observe the produced event log with `ghc-events-analyze <log>`
module Benign.GhcEventsAnalyze where

import Benign qualified
import Debug.Trace (traceEventIO)

event :: String -> Benign.Strat a -> a -> a
event event_name =
  Benign.unsafeSpanBenign
    (traceEventIO $ "START " ++ event_name)
    (traceEventIO $ "STOP " ++ event_name)
