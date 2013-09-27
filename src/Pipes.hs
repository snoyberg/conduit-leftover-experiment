module Pipes where

import Control.Monad
import Control.Applicative
import Test.Hspec
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid
import Prelude hiding (take)

data Pipe i o m r
    = Pure r
    | M (m (Pipe i o m r))
    | Await (i -> Pipe i o m r)
    | Yield (Pipe i o m r) o

instance Monad m => Monad (Pipe i o m) where
    return = Pure

    Pure r >>= fp = fp r
    M m >>= fp = M (liftM (>>= fp) m)
    Await f >>= fp = Await (f >=> fp)
    Yield f o >>= fp = Yield (f >>= fp) o

instance MonadTrans (Pipe i o) where
    lift = M . liftM Pure

(>->) :: Monad m
      => Pipe a b m r
      -> Pipe b c m r
      -> Pipe a c m r
_ >-> Pure r = Pure r
up >-> M m = M (liftM (up >->) m)
up >-> Await f =
    case up of
        Pure r -> Pure r
        M m -> M (liftM (>-> Await f) m)
        Await g -> Await (\a -> g a >-> Await f)
        Yield up' b -> up' >-> f b
up >-> Yield down o = Yield (up >-> down) o

idP :: Monad m => Pipe i i m r
idP = Await (Yield idP)

runPipe :: Monad m
        => Pipe () o m r
        -> m r
runPipe (Pure r) = return r
runPipe (M m) = m >>= runPipe
runPipe (Await f) = runPipe (f ())
runPipe (Yield p _) = runPipe p

runPipeW :: Monoid w => Pipe () o (Writer w) r -> (r, w)
runPipeW = runWriter . runPipe

await :: Monad m => Pipe i o m i
await = Await Pure

yield :: Monad m => o -> Pipe i o m ()
yield = Yield (Pure ())

take :: Monad m => Int -> Pipe o o m ()
take count = replicateM_ count (await >>= yield)

main :: IO ()
main = hspec $ do
    describe "basics" $ do
        it "no idP" $
            runPipeW (mapM_ yield [1..] >-> take 10 >-> forever (await >>= lift . tell . return))
                `shouldBe` ((), [1..10])
        it "idP front" $
            runPipeW (idP >-> mapM_ yield [1..] >-> take 10 >-> forever (await >>= lift . tell . return))
                `shouldBe` ((), [1..10])
        it "idP middle1" $
            runPipeW (mapM_ yield [1..] >-> idP >-> take 10 >-> forever (await >>= lift . tell . return))
                `shouldBe` ((), [1..10])
        it "idP middle2" $
            runPipeW (mapM_ yield [1..] >-> take 10 >-> idP >-> forever (await >>= lift . tell . return))
                `shouldBe` ((), [1..10])
        it "idP back" $
            runPipeW (mapM_ yield [1..] >-> take 10 >-> forever (await >>= lift . tell . return) >-> idP)
                `shouldBe` ((), [1..10])
    return ()