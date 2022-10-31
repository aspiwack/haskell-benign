{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Benign effects are actions which are nominally effects but really doesn't
-- affect the semantics of your programs such as logging and tracing. Such
-- effects can be run in pure code.
--
-- Laziness doesn't always make it easy to add benign effects. Running a pure
-- lazy expression has a beginning (when the thunk is being evaluated) but no
-- real end. For tracing, for instance, you need to mark the end of
-- evaluation.
--
-- This library uses the 'Eval' type class to add a (programmable)
-- end to expressions. If you have a function `span "begin" "end"` to log
-- strings before and after an expression, you could write
--
-- > span "begin" "end" $ Seq $ u`
--
-- To mean that `"end"` is logged after calling `seq u` (evaluating `u` in weak
-- head normal form). `Seq` is called an /evaluation strategy/ for the purpose
-- of this module. Evaluation strategies are all given as data type wrapper.
module Benign
  ( Field,
    newField,
    withAltering,
    get,
    get',
    getWithDefault,
    unsafeSpanBenign,
    Eval,
    Seq,
    SeqIsEval,
    NF,
  )
where

import Control.Concurrent
import Control.Concurrent.Async (async)
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM.TVar
import Control.DeepSeq
import Control.Exception (evaluate, finally)
import Control.Monad.STM
import Data.Int
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Unique (Unique)
import Data.Unique qualified as Unique
import Data.Word
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

withAltering :: Eval b => Field a -> (Maybe a -> Maybe a) -> b -> Result b
withAltering f g thing = unsafePerformIO $ do
  outer_vault <- myLocalState
  inner_vault <- evaluate $ alterVault g f outer_vault
  me <- async $ do
    setLocalState inner_vault
    thunk <- evaluate $ eval thing
    return $ extractEval thunk
  Async.wait me
    `finally` atomically (modifyTVar' localStates (Map.delete (Async.asyncThreadId me)))
-- Note: this implementation relies of the fact that thread ids can't be
-- reused, otherwise there may be races. I'm not sure that GHC guarantees
-- this property.
{-# NOINLINE withAltering #-}

-- | @'unsafeSpanBenign' before after thing@ runs the `before` action before
-- evaluating `thing`, then runs the `after` action.
--
-- 'unsafeSpanBenign' is not typically used directly in programs, but used to
-- write safe benign-effect-spanning functions.
--
-- To call 'unsafeSpanBenign' safely, make sure that the `before` and `after`
-- actions are benign.
unsafeSpanBenign ::
  Eval a =>
  -- | Action to run before evaluation
  IO () ->
  -- | Action to run after evaluation
  IO () ->
  a ->
  Result a
unsafeSpanBenign before after thing = unsafePerformIO $ do
  before
  thunk <- evaluate $ eval thing
  after
  return $ extractEval thunk
{-# NOINLINE unsafeSpanBenign #-}

class Eval a where
  data Thunk a

  -- TODO: make Result representation parametric, possibly? This is kind of the
  -- result to introduce the `Thunk` type to begin with.
  type Result a

  -- | Evaluating the @eval x@ thunk executes the evaluation strategy.
  eval :: a -> Thunk a

  extractEval :: Thunk a -> Result a

-- | Evaluation strategy: evaluates `a` by simply calling `seq` on it.
newtype Seq a = Seq a

instance SeqIsEval a => Eval (Seq a) where
  newtype Thunk (Seq a) = SeqThunk a
  type Result (Seq a) = a
  eval (Seq x) = SeqThunk x
  extractEval (SeqThunk x) = x

-- | Qualifies type where `seq` evaluates sufficiently. This is a fail-safe to
-- avoid using the `'Seq'` strategy by mistake. /E.g./ `'Seq' []` is unlikely to
-- be intended.
class SeqIsEval a

instance SeqIsEval Int

instance SeqIsEval Int32

instance SeqIsEval Int64

instance SeqIsEval Word

instance SeqIsEval Word32

instance SeqIsEval Word64

instance SeqIsEval Char

-- | Evaluation strategy: evaluates `a` in normal form (see "Control.Deepseq").
--
-- This is a pretty blunt strategy, as it necessarily traverse the result type,
-- even if it's already in normal form. As a consequence it's not recommended to
-- use this lightly.
newtype NF a = NF a

instance NFData a => Eval (NF a) where
  newtype Thunk (NF a) = NFThunk a
  type Result (NF a) = a
  eval (NF a) = NFThunk $ force a
  extractEval (NFThunk a) = a
