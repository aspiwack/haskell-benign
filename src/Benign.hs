{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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
-- end to expressions. If you have a function @span "begin" "end"@ to log
-- strings before and after an expression, you could write
--
-- > span "begin" "end" $ Seq $ u
--
-- To mean that `"end"` is logged after calling `seq u` (evaluating `u` in weak
-- head normal form). `Seq` is called an /evaluation strategy/ for the purpose
-- of this module. Evaluation strategies are all given as data type wrapper.
module Benign
  ( Field,
    newField,
    withAltering,
    withSetting,
    withAlteringIO,
    withSettingIO,
    lookupLocalState,
    lookupLocalState',
    lookupLocalStateWithDefault,
    unsafeSpanBenign,
    Eval (..),
    Seq (..),
    Lazy (..),
    SeqIsEval,
    NF (..),
    EvalIO (..),
    PureEval (..),

    -- * Monads
    EvalM (..),
    withAlteringM,
    withSettingM,
    unsafeSpanBenignM,
  )
where

import Control.Concurrent
import Control.Concurrent.Async (async)
import Control.Concurrent.Async qualified as Async
import Control.DeepSeq
import Control.Exception (bracket_, evaluate, finally)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy
import Data.Coerce
import Data.Functor.Identity
import Data.Int
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Strict.Wrapper
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

-- Implementation note: the `Vault` is immutable data, because `localStates` is
-- a lexical state rather than a global state: the state is modified within a
-- scope, like in `Reader`'s `local`, rather than globally, like in `State`'s
-- `put`. It's easier to implement the local aspect with immutable data
-- (otherwise what would happen if, say, two threads run in parallel?)
--
-- The outer Map (from `TId` to `Vault`) is a global state, and need not be a
-- `Map`. But it was easier for now. But it may be worth it in the future to
-- make an implementation that has “one TVar per thread” (whatever this actually
-- means) to avoid contention of the outer `TVar`. To design this, it's worth
-- keeping in mind, though, that the value associated with a thread is never
-- modified: only set (once) then deleted. So this is what the structure would
-- have to recognise is non-interfering for STM actions.
localStates :: MVar LocalStates
localStates = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE localStates #-}

myLocalState :: IO Vault
myLocalState = do
  tid <- myThreadId
  currentStates <- readMVar localStates
  return $ Map.findWithDefault Map.empty tid currentStates

lookupLocalState :: Field a -> IO (Maybe a)
lookupLocalState f = lookupVault f <$> myLocalState

lookupLocalState' :: HasCallStack => Field a -> IO a
lookupLocalState' f = fromMaybe errmsg <$> lookupLocalState f
  where
    -- It would be nice if fields had a name that we could use here to display
    -- more helpful error messages.
    errmsg = error "Trying to retrieve an absent field"

lookupLocalStateWithDefault :: a -> Field a -> IO a
lookupLocalStateWithDefault deflt f = fromMaybe deflt <$> lookupLocalState f

setLocalState :: Vault -> IO ()
setLocalState vault = do
  tid <- myThreadId
  modifyMVar_ localStates (evaluate . Map.insert tid vault)

-- | @'withAltering' f g thing@ evaluates 'thing' with the local state's field
-- `f` set by `g` in the style of 'Map.alter'.
--
-- Why do we need this? If we are to do some logging in pure code, we still need
-- to know /where/ to log too. That is we need configuration. It is out of
-- question to modify all the pure code to take the configuration as arguments
-- (either explicit, implicit, or with a monad): this would force us to pass
-- arguments down all the functions that call functions that ultimately call a
-- logging function. It would be majorly inconvenient, but mostly it goes
-- against the fact that the code is pure. If we go this way, we may as well
-- write all the code in monadic style. It is an assumption of this library that
-- we don't want to do this (and it seems to be supported by experience that
-- most Haskell programmers don't want to write most of their code in a monad).
--
-- So we need a way to pass some state to pure code, to be used in benign
-- effects (see 'lookupLocalState' and 'unsafeSpanBenign'). This is what
-- 'withAltering' (and friends) achieves.
--
-- Another example of state which makes it especially obvious that we want a
-- lexical state (à la `Reader` monad) rather that a global state (à la `State`
-- monad): some logging frameworks, such as Katip, let you add some context to
-- logging strings. This context, of course, is lexical (you don't modify the
-- logging context in another thread to reflect what's going on in your thread).
--
-- At any rate, since 'withAltering' is exposed as a pure function, it wouldn't
-- make much sense to modify a global state. The modification could happen at
-- any time and in any order. The result would be quite ill-defined.
--
-- The reason why we need the 'Eval' constraint is that, as a direct consequence
-- of the design, the lexical state is passed dynamically to `thing`. That is,
-- the state that is seen by a piece of code depends on where it's executed: if
-- a lazy thunk escapes 'withAltering', then it's going to be picking up a
-- different state. 'Eval' lets us be deliberate about what escapes and what
-- doesn't. Namely the altered state is available precisely during the
-- evaluation of `eval thing`.
withAltering :: Eval b => Field a -> (Maybe a -> Maybe a) -> b -> Result b
withAltering f g thing = unsafePerformIO $ withAlteringIO f g (PureEval thing)
{-# NOINLINE withAltering #-}

-- | TODO
withAlteringIO :: EvalIO b => Field a -> (Maybe a -> Maybe a) -> b -> IO (ResultIO b)
withAlteringIO f g thing = do
  outer_vault <- myLocalState
  inner_vault <- evaluate $ alterVault g f outer_vault
  -- We make an `async` here, not because of concurrency: it's not going to be
  -- run concurrently with anything. It's because `myThreadId` is the only piece
  -- of implicit state that GHC provides. Everything runs in a thread, so even
  -- in pure code, we can call `myThreadId` and get state that is local where we
  -- are. This is what lets us implement a lexical state. We keep the global
  -- `localStates` map from thread ids to the lexical state, and use the nesting
  -- of `async`s to represent the stack discipline. It's a little clunky, but it
  -- works. The cost of spawning an async is, I believe, less than the cost of
  -- I/O that will be required for the actual logging. So we are well within
  -- bounds of the desired performance.
  --
  -- I've been pointed to two other implementation of thread-local state. I
  -- could take inspiration from either to harden this. What happens, for
  -- instance, if the main thread receives an asynchronous exception between
  -- starting the `async` and `wait`-ing? It may be that the local state is
  -- leaked. Maybe `withAsync` could help.
  --
  -- - Very featureful implementation: https://hackage.haskell.org/package/context
  -- - Uses the GC to collect thread-local state: https://hackage.haskell.org/package/thread-utils-context
  me <- async $ do
    setLocalState inner_vault
    evalIO thing
  -- Note: this implementation relies of the fact that thread ids can't be
  -- reused, otherwise there may be races. I'm not sure that GHC guarantees
  -- this property.
  Async.wait me
    `finally` modifyMVar_ localStates (evaluate . Map.delete (Async.asyncThreadId me))

-- | @'withSetting f a thing@ evaluates 'thing' with the local state's field `f`
-- set to `a`.
--
-- See 'withAltering' for more explanations.
withSetting :: Eval b => Field a -> a -> b -> Result b
withSetting f a = withAltering f (\_ -> Just a)

-- | TODO
withSettingIO :: EvalIO b => Field a -> a -> b -> IO (ResultIO b)
withSettingIO f a = withAlteringIO f (\_ -> Just a)

-- | @'unsafeSpanBenign' before after thing@ runs the `before` action before
-- evaluating `thing`, then runs the `after` action.
--
-- 'unsafeSpanBenign' is not typically used directly in programs, but used to
-- write safe benign-effect-spanning functions.
--
-- To call 'unsafeSpanBenign' safely, make sure that the `before` and `after`
-- actions are indeed benign.
unsafeSpanBenign ::
  Eval a =>
  -- | Action to run before evaluation
  IO () ->
  -- | Action to run after evaluation
  IO () ->
  a ->
  Result a
unsafeSpanBenign before after thing = unsafePerformIO $ do
  thunk <- bracket_ before after (evaluate $ eval thing)
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

instance (Eval a, Eval b) => Eval (a, b) where
  data Thunk (a, b) = PairThunk (Thunk a) (Thunk b)
  type Result (a, b) = (Result a, Result b)
  eval (a, b) = PairThunk (eval a) (eval b)
  extractEval (PairThunk a b) = (extractEval a, extractEval b)

-- | Evaluation strategy: evaluates `a` by simply calling `seq` on it.
newtype Seq a = Seq a
  deriving anyclass (EvalIO)

instance SeqIsEval a => Eval (Seq a) where
  newtype Thunk (Seq a) = SeqThunk a
  type Result (Seq a) = a
  eval (Seq x) = SeqThunk x
  extractEval (SeqThunk x) = x

-- | Qualifies type where `seq` evaluates sufficiently. This is a fail-safe to
-- avoid using the 'Seq' strategy by mistake. /E.g./ @'Seq' []@ is unlikely to
-- be intended.
class SeqIsEval a

instance SeqIsEval Int

instance SeqIsEval Int32

instance SeqIsEval Int64

instance SeqIsEval Integer

instance SeqIsEval Word

instance SeqIsEval Word32

instance SeqIsEval Word64

instance SeqIsEval Float

instance SeqIsEval Double

instance SeqIsEval Char

instance SeqIsEval Bool

instance SeqIsEval Ordering

instance SeqIsEval ()

instance SeqIsEval a => SeqIsEval (Strict (Maybe a))

instance (SeqIsEval a, SeqIsEval b) => SeqIsEval (Strict (Either a b))

instance (SeqIsEval a, SeqIsEval b) => SeqIsEval (Strict (a, b))

instance (SeqIsEval a, SeqIsEval b, SeqIsEval c) => SeqIsEval (Strict (a, b, c))

instance (SeqIsEval a, SeqIsEval b, SeqIsEval c, SeqIsEval d) => SeqIsEval (Strict (a, b, c, d))

-- | Sometimes, you actually want an `a` to escape the context. You really do
-- want `a` to be lazily evaluated. For this, you can use the type @'Lazy' a@.
-- @'Lazy' a@ tells the 'Seq' strategy that, yes, indeed, you do want `a` to be
-- lazy. The point being that @'seq' Lazy u@ doesn't evaluate @u@.
data Lazy a = Lazy a

instance SeqIsEval (Lazy a)

-- | Evaluation strategy: evaluates `a` in normal form (see the `deepseq`
-- package).
--
-- This is a pretty blunt strategy, as it necessarily traverse the result type,
-- even if it's already in normal form. As a consequence it's not recommended to
-- use this lightly.
newtype NF a = NF a
  deriving anyclass (EvalIO)

instance NFData a => Eval (NF a) where
  newtype Thunk (NF a) = NFThunk a
  type Result (NF a) = a
  eval (NF a) = NFThunk $ force a
  extractEval (NFThunk a) = a

-- | TODO: evaluate in an IO context. Different monad because I want `EvalIO (IO a)`
-- but not `Eval (IO a)` (since it would let me `unsafePerformIO`)
class EvalIO a where
  type ResultIO a
  type ResultIO a = Result a

  evalIO :: a -> IO (ResultIO a)
  default evalIO :: (Eval a, ResultIO a ~ Result a) => a -> IO (ResultIO a)
  evalIO = evalInIO

evalInIO :: Eval a => a -> IO (Result a)
evalInIO thing = extractEval <$> evaluate (eval thing)

instance EvalIO (IO a) where
  type ResultIO (IO a) = a
  evalIO = id

newtype PureEval a = PureEval a

instance Eval a => EvalIO (PureEval a) where
  type ResultIO (PureEval a) = Result a
  evalIO (PureEval a) = evalInIO a

---------------------------------------------------------------------------
--
-- Evaluate in a monad
--
---------------------------------------------------------------------------

-- | In non-IO monadic code (that is when monads are used as a way to organise
-- pure code), naturally, we'll be wanting to use benign effect as well. How
-- scopes and running monadic code interleave doesn't have a generic
-- answer. This is because monadic code is fundamentally staged: first you build
-- a monadic expression, then it is run. Benign effects, and in particular
-- local state updates, must happen when the monad is run, not when the
-- expression is built.
--
-- Just like there isn't a generic `run` function, since all monads interpret
-- the monadic expression differently, each monad needs to explain how they
-- implement 'withAltering' and 'unsafeSpanBenign'. This is what the (admittedly
-- poorly named) 'EvalM' class lets monad do.
class EvalM m where
  spliceEval :: Eval b => (forall a. Eval a => (a -> Result a)) -> m b -> m (Result b)

withAlteringM :: (EvalM m, Eval b) => Field a -> (Maybe a -> Maybe a) -> m b -> m (Result b)
withAlteringM f g = spliceEval (withAltering f g)

withSettingM :: (EvalM m, Eval b) => Field a -> a -> m b -> m (Result b)
withSettingM f a = withAlteringM f (\_ -> Just a)

unsafeSpanBenignM :: (EvalM m, Eval a) => IO () -> IO () -> m a -> m (Result a)
unsafeSpanBenignM before after = spliceEval (unsafeSpanBenign before after)

instance EvalM Identity where
  spliceEval :: forall b. Eval b => (forall a. Eval a => a -> Result a) -> Identity b -> Identity (Result b)
  spliceEval f = coerce $ f @b

instance (EvalM m, Eval s, Result s ~ s) => EvalM (StateT s m) where
  spliceEval f (StateT thing) = StateT $ \s -> spliceEval f (thing s)

instance (EvalM m) => EvalM (ReaderT e m) where
  spliceEval f (ReaderT thing) = ReaderT $ \e -> spliceEval f (thing e)
