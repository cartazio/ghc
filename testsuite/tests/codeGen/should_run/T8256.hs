{-# LANGUAGE MagicHash, UnboxedTuples #-}

{-
this test has been updated to reflect new design of pure prefetch operations
in Trac #9353, which uses state tokens always.


-}

import GHC.Prim (prefetchValue3#,prefetchValue2#,prefetchValue1#,prefetchValue0#)

import Data.Vector.Storable.Mutable
import Foreign.Ptr
import GHC.ST
import Data.Primitive.ByteArray
import Control.Monad.Primitive
import GHC.Types
import qualified Control.Monad.ST as ST


{-newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))-}

wrapFetch :: (a -> State# s-> State# s)-> (a -> IO ())
wrapFetch prefetch  a = IO (\ s -> (# prefetch a s, ()#))

{-# NOINLINE sameByteArray #-}
sameByteArray  :: ByteArray -> ByteArray ->  Bool
sameByteArray ar1 ar2 = runST $
        do  v1 <- unsafeThawByteArray ar1
            v2 <- unsafeThawByteArray ar2
            return $ sameMutableByteArray v1 v2





main :: IO ()
main = do
    mv1 <- newByteArray 17
    v1 <- unsafeFreezeByteArray mv1
    --return ()
    wrapFetch prefetchValue0# v1
    wrapFetch prefetchValue1# (1 ::Int)
    wrapFetch prefetchValue2# "hiiii"
    wrapFetch prefetchValue3# (Maybe "testing")
    if v1 `sameByteArray`  v1
      then putStrLn "success" else error "bad prefetch operation! please report"




