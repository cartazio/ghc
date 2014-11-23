{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Main where
{-
this test has been updated to reflect new design of pure prefetch operations
in Trac #9353, which uses state tokens always.


-}

import GHC.Prim
import GHC.Types



{-newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))-}

wrapFetch :: (a -> State# RealWorld -> State# RealWorld) -> (a -> IO ())
wrapFetch prefetch  a = IO (\ s -> (# prefetch a s, ()#))







main :: IO ()
main = do
    wrapFetch prefetchValue1# (1 ::Int)
    wrapFetch prefetchValue2# "hiiii"
    wrapFetch prefetchValue3# (Just "testing")
    wrapFetch prefetchValue0# (error "this shouldn't get evaluated")
    --  -- ^^ this is to make sure it doesn't force thunks!
    --incontrast,
    --wrapFetch prefetchValue0#  $! (error "this shouldn't get evaluated")
    -- would trigger an exception
    putStrLn "success"





