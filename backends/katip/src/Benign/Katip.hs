{-# LANGUAGE GHC2021 #-}

module Benign.Katip where

import Benign qualified
import GHC.Stack
import Katip qualified
import System.IO.Unsafe (unsafePerformIO)
import Prelude

katipEnv :: Benign.Field Katip.LogEnv
katipEnv = unsafePerformIO Benign.newField
{-# NOINLINE katipEnv #-}

katipContext :: Benign.Field Katip.LogContexts
katipContext = unsafePerformIO Benign.newField
{-# NOINLINE katipContext #-}

katipNamespace :: Benign.Field Katip.Namespace
katipNamespace = unsafePerformIO Benign.newField
{-# NOINLINE katipNamespace #-}

-- | See 'Katip.katipAddContext'.
withKatipContext :: (Katip.LogItem i) => i -> Benign.Strat a -> a -> a
withKatipContext item = Benign.withAltering katipContext addContext
  where
    addContext (Just st) = Just $ st <> Katip.liftPayload item
    addContext Nothing = error "todo"

-- | See 'Katip.katipAddNamespace'.
withKatipNamespace :: Katip.Namespace -> Benign.Strat a -> a -> a
withKatipNamespace namespace = Benign.withAltering katipNamespace addNamespace
  where
    addNamespace (Just st) = Just $ st <> namespace
    addNamespace Nothing = error "todo"

-- | Within this computation, Katip is configured.
withKatip ::
  (Katip.LogItem c) =>
  Katip.LogEnv ->
  c ->
  Katip.Namespace ->
  Benign.Strat a ->
  IO a ->
  IO a
withKatip env ctx namespace strat =
  Benign.withSettingIO' katipEnv env
    . Benign.withSettingIO' katipContext (Katip.liftPayload ctx)
    . Benign.withSettingIO katipNamespace namespace strat

logLocM :: forall a. (HasCallStack) => Katip.Severity -> Katip.LogStr -> Benign.Strat a -> a -> a
logLocM severity str = withFrozenCallStack spanLog
  where
    -- The whole purpose of naming `span` is to freeze the call stack. It's
    -- important to freeze the call site, so that the log is recorded as
    -- happening on the line where `logLocM` was called, not on this file. But
    -- since `HasCallStack` is magical, it's quite possible for the type system
    -- to “lose track” of the current `HasCallStack` and create a new one from
    -- scratch. This would be invisible. I tried to harden this function by
    -- declaring type signatures everywhere. I haven't tested it yet though. It
    -- may be wrong.
    spanLog :: HasCallStack => Benign.Strat a -> a -> a
    spanLog = Benign.unsafeSpanBenign doLog (return ())

    doLog :: HasCallStack => IO ()
    doLog = do
      -- Making an intermediary `KatipContextT` is a little roundabout, but it's
      -- easier than reaching to Katip's internals.
      --
      -- TODO: catch errors
      Just env <- Benign.lookupLocalState katipEnv
      Just ctx <- Benign.lookupLocalState katipContext
      Just namespace <- Benign.lookupLocalState katipNamespace
      Katip.runKatipContextT env ctx namespace $ Katip.logLocM severity str
