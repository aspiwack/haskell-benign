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
    withAlteringIO',
    withSettingIO,
    withSettingIO',
    lookupLocalState,
    lookupLocalState',
    lookupLocalStateWithDefault,
    unsafeSpanBenign,

    -- * Monads
    EvalM (..),
    withAlteringM,
    withAlteringM',
    withSettingM,
    withSettingM',
    unsafeSpanBenignM,
    unsafeSpanBenignM',

    -- * Strategies
    Strat,
    E (..),
    lazy,
    whnf,
    nf,
    Eval (..),
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

-- | @'withAltering' f g strat thing@ evaluates 'thing' with the local state's
-- field `f` set by `g` in the style of 'Map.alter'.
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
-- The reason why we need the 'strat' argument is that, as a direct consequence
-- of the design, the lexical state is passed dynamically to `thing`. That is,
-- the state that is seen by a piece of code depends on where it's executed: if
-- a lazy thunk escapes 'withAltering', then it's going to be picking up a
-- different state. 'strat' lets us be deliberate about what escapes and what
-- doesn't. Namely the altered state is available precisely during the
-- evaluation of `strat thing`.
withAltering :: Field a -> (Maybe a -> Maybe a) -> Strat b -> b -> b
withAltering f g strat thing = unsafePerformIO $ withAlteringIO f g strat (return thing)
{-# NOINLINE withAltering #-}

-- | TODO
withAlteringIO :: Field a -> (Maybe a -> Maybe a) -> Strat b -> IO b -> IO b
withAlteringIO f g strat thing = do
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
    r <- thing
    E <- evaluate $ strat r
    return r
  -- Note: this implementation relies of the fact that thread ids can't be
  -- reused, otherwise there may be races. I'm not sure that GHC guarantees
  -- this property.
  Async.wait me
    `finally` modifyMVar_ localStates (evaluate . Map.delete (Async.asyncThreadId me))

-- | Like 'withAltering', but the `strat` is `lazy`. This is often the right
-- choice in a monadic context.
withAlteringIO' :: Field a -> (Maybe a -> Maybe a) -> IO b -> IO b
withAlteringIO' f g = withAlteringIO f g lazy

-- | @'withSetting f a thing@ evaluates 'thing' with the local state's field `f`
-- set to `a`.
--
-- See 'withAltering' for more explanations.
withSetting :: Field a -> a -> Strat b -> b -> b
withSetting f a = withAltering f (\_ -> Just a)

-- | TODO
withSettingIO :: Field a -> a -> Strat b -> IO b -> IO b
withSettingIO f a = withAlteringIO f (\_ -> Just a)

-- | Like 'withSetting', but the `strat` is `lazy`. This is often the right
-- choice in a monadic context.
withSettingIO' :: Field a -> a -> IO b -> IO b
withSettingIO' f a = withSettingIO f a lazy

-- | @'unsafeSpanBenign' before after thing@ runs the `before` action before
-- evaluating `strat thing`, then runs the `after` action.
--
-- 'unsafeSpanBenign' is not typically used directly in programs, but used to
-- write safe benign-effect-spanning functions.
--
-- To call 'unsafeSpanBenign' safely, make sure that the `before` and `after`
-- actions are indeed benign.
unsafeSpanBenign ::
  -- | Action to run before evaluation
  IO () ->
  -- | Action to run after evaluation
  IO () ->
  Strat a ->
  a ->
  a
unsafeSpanBenign before after strat thing = unsafePerformIO $ do
  E <- bracket_ before after (evaluate $ strat thing)
  return thing
{-# NOINLINE unsafeSpanBenign #-}

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
  spliceEval :: (forall a. Strat a -> a -> a) -> Strat b -> m b -> m b

withAlteringM :: (EvalM m) => Field a -> (Maybe a -> Maybe a) -> Strat b -> m b -> m b
withAlteringM f g = spliceEval (withAltering f g)

-- | Like 'withAlteringM', but the `strat` is `lazy`. This is often the right
-- choice in a monadic context.
withAlteringM' :: (EvalM m) => Field a -> (Maybe a -> Maybe a) -> m b -> m b
withAlteringM' f g = withAlteringM f g lazy

-- | Like 'withSetting', but in a monadic context. The `strat` is `lazy`. This
-- is often the right choice in a monadic context.
withSettingM :: (EvalM m) => Field a -> a -> Strat b -> m b -> m b
withSettingM f a = withAlteringM f (\_ -> Just a)

-- | Like 'withSettingM', but the `strat` is `lazy`. This is often the right
-- choice in a monadic context.
withSettingM' :: (EvalM m) => Field a -> a -> m b -> m b
withSettingM' f a = withSettingM f a lazy

unsafeSpanBenignM :: (EvalM m) => IO () -> IO () -> Strat a -> m a -> m a
unsafeSpanBenignM before after = spliceEval (unsafeSpanBenign before after)

-- | Like 'unsafeSpanBenignM', but the `strat` is `lazy`. This is often the
-- right choice in a monadic context.
unsafeSpanBenignM' :: (EvalM m) => IO () -> IO () -> m a -> m a
unsafeSpanBenignM' before after = unsafeSpanBenignM before after lazy

instance EvalM Identity where
  spliceEval :: forall b. (forall a. Strat a -> a -> a) -> Strat b -> Identity b -> Identity b
  spliceEval f = coerce $ f @b

-- | Doesn't evaluate the state. It would be possible to require `'Eval' s` so
-- that the state can also be evaluated. Unclear what is the most natural.
instance (EvalM m) => EvalM (StateT s m) where
  spliceEval f strat (StateT thing) = StateT $ \s -> spliceEval f tupStrat (thing s)
    where
      tupStrat (b, _) = strat b

instance (EvalM m) => EvalM (ReaderT e m) where
  spliceEval f strat (ReaderT thing) = ReaderT $ \e -> spliceEval f strat (thing e)

---------------------------------------------------------------------------
--
-- Evaluate in a monad
--
---------------------------------------------------------------------------

-- | Evaluation strategies. The idea is that evaluating with strategy `strat` is
-- the same as evaluating `strat a` in whnf.
--
-- This is inspired by `Control.Seq.Strategy` from the
-- [parallel](https://hackage.haskell.org/package/parallel) package. It's
-- actually roughly the same type, but a little more modern. Making sure in
-- particular that 'E', as a monoid, is strict, so that
--
-- > 'foldMap' :: Foldable t => Strat a -> Strat (t a)
--
-- Evaluates all the position in a container.
type Strat a = a -> E

data E = E
  deriving stock (Eq, Ord, Enum, Bounded, Show, Read)

instance Semigroup E where
  E <> E = E

instance Monoid E where
  mempty = E

-- | Doesn't do any evaluation.
lazy :: Strat a
lazy = mempty

-- | Evaluates in whnf. Like 'seq'
whnf :: Strat a
whnf a = a `seq` E

nf :: (NFData a) => Strat a
nf a = a `deepseq` E

class Eval a where
  -- | A canonical strategy for type `a`.
  eval :: Strat a

instance Eval Int where
  eval = (`seq` E)

instance Eval Int32 where
  eval = (`seq` E)

instance Eval Int64 where
  eval = (`seq` E)

instance Eval Word where
  eval = (`seq` E)

instance Eval Word32 where
  eval = (`seq` E)

instance Eval Word64 where
  eval = (`seq` E)

instance Eval Bool where
  eval = (`seq` E)

instance Eval Char where
  eval = (`seq` E)

instance Eval Double where
  eval = (`seq` E)

instance Eval Float where
  eval = (`seq` E)

instance Eval () where
  eval = (`seq` E)

instance Eval Ordering where
  eval = (`seq` E)

instance Eval a => Eval (Maybe a) where
  eval = foldMap eval

instance Eval a => Eval [a] where
  eval = foldMap eval

instance (Eval a, Eval b) => Eval (Either a b) where
  eval (Left a) = eval a
  eval (Right b) = eval b

instance (Eval a, Eval b) => Eval (a, b) where
  eval (a, b) = eval a <> eval b

instance (Eval a, Eval b, Eval c) => Eval (a, b, c) where
  eval (a, b, c) = eval a <> eval b <> eval c

instance (Eval a, Eval b, Eval c, Eval d) => Eval (a, b, c, d) where
  eval (a, b, c, d) = eval a <> eval b <> eval c <> eval d
