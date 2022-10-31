{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RoleAnnotations #-}

module Benign
  ( Field,
    newField,
    withAltering,
    get,
    get',
    getWithDefault,
  )
where

import Control.Concurrent
import Control.Concurrent.Async (async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM.TVar
import Control.Exception (evaluate, finally)
import Control.Monad.STM
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Unique (Unique)
import Data.Unique qualified as Unique
import GHC.Exts (Any)
import GHC.Stack (HasCallStack)
import System.IO.Unsafe
import Unsafe.Coerce

newtype Field a = MkField Unique
  deriving newtype (Eq, Ord)

type role Field nominal

newField :: IO (Field a)
newField = MkField <$> Unique.newUnique

type Vault = Map Unique Any

lookupVault :: Field a -> Vault -> Maybe a
lookupVault (MkField u) = unsafeCoerce $ Map.lookup u

alterVault :: (Maybe a -> Maybe a) -> Field a -> Vault -> Vault
alterVault f (MkField u) = Map.alter (unsafeCoerce f) u

type LocalStates = Map ThreadId Vault

localStates :: TVar LocalStates
localStates = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE localStates #-}

myLocalState :: IO Vault
myLocalState = do
  tid <- myThreadId
  atomically $ do
    currentStates <- readTVar localStates
    return $ Map.findWithDefault Map.empty tid currentStates

lookupInLocalState :: Field a -> IO (Maybe a)
lookupInLocalState f = lookupVault f <$> myLocalState

setLocalState :: Vault -> IO ()
setLocalState vault = do
  tid <- myThreadId
  atomically $ modifyTVar' localStates $ Map.insert tid vault

get :: Field a -> Maybe a
get f = unsafePerformIO $ lookupInLocalState f
{-# NOINLINE get #-}

get' :: HasCallStack => Field a -> a
get' f = case get f of
  Just a -> a
  Nothing -> error "Trying to retrieve an absent field"

-- It would be nice if fields had a name that we could use here to display
-- more helpful error messages.

getWithDefault :: a -> Field a -> a
getWithDefault deflt = fromMaybe deflt . get

withAltering :: Field a -> (Maybe a -> Maybe a) -> b -> b
withAltering f g thing = unsafePerformIO $ do
  outer_vault <- myLocalState
  inner_vault <- evaluate $ alterVault g f outer_vault
  me <- async $ do
    setLocalState inner_vault
    evaluate thing
  Async.wait me
    `finally` atomically (modifyTVar' localStates (Map.delete (Async.asyncThreadId me)))
{-# NOINLINE withAltering #-}
