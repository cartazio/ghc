{-# LANGUAGE MagicHash, UnboxedTuples #-}

{-
this test has been updated to reflect new design of pure prefetch operations
in Trac #9353
-}

import GHC.Prim

import Data.Vector.Storable.Mutable
import Foreign.Ptr
import GHC.ST
import Data.Primitive.ByteArray
import Control.Monad.Primitive

import qualified Control.Monad.ST as ST


sameByteArray  :: ByteArray -> ByteArray ->  Bool
sameByteArray ar1 ar2 = runST $
        do  v1 <- unsafeThawByteArray ar1
            v2 <- unsafeThawByteArray ar2
            return $ sameMutableByteArray v1 v2



appBy :: Monad m => (ByteArray# -> Int# -> a -> (#a#)) -> ByteArray  -> a -> m a
appBy f (ByteArray by) a = (return . untup )$  f by #1   a
      where
        untup :: (#a#)-> a
        untup (#a#) = a
--isSameByteArray :: ByteArray -> ByteArray -> Bool
--isSameByteArray = sameByteArray

main :: IO ()
main = do
    mv1 <- newByteArray 17
    v1 <- unsafeFreezeByteArray mv1
    --return ()
    t0<- appBy (prefetchByteArray0) v1 v1
    t1 <- appBy (prefetchByteArray0) v1 v1
    t2 <- appBy (prefetchByteArray0) v1 v1
    t3 <- appBy (prefetchByteArray0) v1 v1
    if t0 `sameByteArray`  v1
      && t1 `sameByteArray`  v1
      && t2 `sameByteArray`  v1
      && t3  `sameByteArray`  v1
      then putStrLn "success" else error "bad prefetch operation! please report"




