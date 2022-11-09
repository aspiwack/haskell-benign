{-# LANGUAGE GHC2021 #-}

module Main where

import Benign (Seq (..))
import Benign qualified
import Data.Maybe
import GHC.IO.Unsafe (unsafePerformIO)
import Prelude hiding (log)

------------------------------------------------------------------------------
--
-- Creating benign effects
--
------------------------------------------------------------------------------

-- Let's first create some benign effect primitives. Here we are going for the
-- simplest possible form of logging: printing a string on stdout. Typically,
-- you would write such primitives in a separate module. The primitives are
-- meant to be safe, but will always be defined using unsafe lower-level
-- primitives.
--
-- To add a little bit of structure, we'll give ourselves the ability to
-- annotate a context with a string. Which will simply be prepended to the log
-- string.

-- We set up a (global) local-state field to store the context string.
context :: Benign.Field String
context = unsafePerformIO Benign.newField
{-# NOINLINE context #-}

-- 'withContext' is called to extend the context in its scope.
withContext :: Benign.Eval a => String -> a -> Benign.Result a
withContext additional_ctx = Benign.withAltering context $ \ctx ->
  Just $
    fromMaybe "" ctx ++ " " ++ additional_ctx

-- The 'log' function is the one performing the logging. It is a little
-- degenerate, as far as benign effects go, as it doesn't do anything after the
-- end of the evaluation.
log :: Benign.Eval a => String -> a -> Benign.Result a
log log_line = Benign.unsafeSpanBenign do_log (return ())
  where
    do_log :: IO ()
    do_log = do
      ctx <- Benign.lookupLocalState context
      putStrLn (fromMaybe "" ctx ++ " " ++ log_line)

------------------------------------------------------------------------------
--
-- Writing instrumented pure code
--
------------------------------------------------------------------------------

(+:) :: Int -> Int -> Int
n +: p = (withContext "left:" (Seq n)) + (withContext "right:" (Seq p))

incr :: Int -> Int
incr n = log (show n) $ Seq $ n + 1

myvalue :: Int
myvalue = incr 18 +: incr 42 +: incr 57

------------------------------------------------------------------------------
--
-- Running the program
--
------------------------------------------------------------------------------

-- For the sake of this example, we only want to trigger all the logging for
-- demonstration. So 'myvalue' needs to be evaluated. A simple way to do so is
-- to simply 'print'.
main :: IO ()
main = print myvalue

-- The program prints:
--
--  left: left: 18
--  left: right: 42
--  right: 57
-- 120
