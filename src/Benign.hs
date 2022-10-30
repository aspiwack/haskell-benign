{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RoleAnnotations #-}

module Benign (Field) where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Unique (Unique)
import Data.Unique qualified as Unique
import GHC.Exts (Any)
import System.IO.Unsafe
import Unsafe.Coerce

newtype Field a = MkField Unique
  deriving newtype (Eq, Ord)

type role Field nominal

newField :: IO (Field a)
newField = MkField <$> Unique.newUnique

type Vault = Map Unique Any

lookupVault :: Field a -> Vault -> Maybe a
lookupVault (MkField id) = unsafeCoerce $ Map.lookup id

insertVault :: Field a -> a -> Vault -> Vault
insertVault (MkField id) a = unsafeCoerce $ Map.insert id a

type LocalStates = Map ThreadId Vault

localStates :: TVar LocalStates
localStates = unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE localStates #-}

-- modifyLocalState :: (Vault -> Vault) -> STM ()
-- modifyLocalState f = modifyTVar' (Map.alter f' id) localStates
--   where
--     f' = Map.alter

-- insertInLocalState :: Field a -> a -> IO ()
-- insertInLocalState f a = do
--   tid <- myThreadId
--   atomically $ modifyLocalState (insertVault )

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
