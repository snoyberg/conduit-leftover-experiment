module Pipes where

import Control.Monad
import Control.Applicative
import Test.Hspec
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Monoid
import Prelude hiding (take)

data Pipe i o r m a
    = Pure (Either r a)
    | M (m (Pipe i o r m a))
    | Await (i -> Pipe i o r m a)
    | Yield (Pipe i o r m a) o

instance Monad m => Monad (Pipe i o r m) where
    return = Pure . Right

    Pure (Left r) >>= _ = Pure (Left r)
    Pure (Right a) >>= fp = fp a
    M m >>= fp = M (liftM (>>= fp) m)
    Await f >>= fp = Await (f >=> fp)
    Yield f o >>= fp = Yield (f >>= fp) o

instance MonadTrans (Pipe i o r) where
    lift = M . liftM (Pure . Right)

(>->) :: Monad m
      => Pipe a b r m r
      -> Pipe b c r m r
      -> Pipe a c r m r
_ >-> Pure r = Pure r
up >-> M m = M (liftM (up >->) m)
up >-> Await f =
    case up of
        Pure r -> Pure r
        M m -> M (liftM (>-> Await f) m)
        Await g -> Await (\a -> g a >-> Await f)
        Yield up' b -> up' >-> f b
up >-> Yield down o = Yield (up >-> down) o

idP :: Monad m => Pipe i i r m a
idP = Await (Yield idP)

runPipe :: Monad m
        => Pipe () o r m a
        -> m (Either r a)
runPipe (Pure r) = return r
runPipe (M m) = m >>= runPipe
runPipe (Await f) = runPipe (f ())
runPipe (Yield p _) = runPipe p

runPipeW :: Monoid w => Pipe () o r (Writer w) a -> (Either r a, w)
runPipeW = runWriter . runPipe

await :: Monad m => Pipe i o r m i
await = Await (Pure . Right)

yield :: Monad m => o -> Pipe i o r m ()
yield = Yield (Pure (Right ()))

take :: Monad m => Int -> Pipe o o r m ()
take count = replicateM_ count (await >>= yield)

main :: IO ()
main = hspec $ do
    describe "basics" $ do
        it "no idP" $
            runPipeW (mapM_ yield [1..] >-> take 10 >-> forever (await >>= lift . tell . return))
                `shouldBe` (Right (), [1..10])
        it "idP front" $
            runPipeW (idP >-> mapM_ yield [1..] >-> take 10 >-> forever (await >>= lift . tell . return))
                `shouldBe` (Right (), [1..10])
        it "idP middle1" $
            runPipeW (mapM_ yield [1..] >-> idP >-> take 10 >-> forever (await >>= lift . tell . return))
                `shouldBe` (Right (), [1..10])
        it "idP middle2" $
            runPipeW (mapM_ yield [1..] >-> take 10 >-> idP >-> forever (await >>= lift . tell . return))
                `shouldBe` (Right (), [1..10])
        it "idP back" $
            runPipeW (mapM_ yield [1..] >-> take 10 >-> forever (await >>= lift . tell . return) >-> idP)
                `shouldBe` (Right (), [1..10])
    return ()