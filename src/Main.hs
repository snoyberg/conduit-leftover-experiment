{-# OPTIONS_GHC -Wall #-}
import Control.Monad hiding (foldM)
import Control.Applicative
import Test.Hspec
import Data.Functor.Identity
import Data.Void
import Control.Monad.Trans.Writer
import Data.Monoid
import Prelude hiding (drop)

data Conduit i o m r
    = Pure [i] r
    | M (m (Conduit i o m r))
    | Await (i -> Conduit i o m r) (Conduit i o m r)
    | Yield (Conduit i o m r) o
    | Cleanup (Conduit i o m r) ([o] -> Conduit i o m ())

instance Monad m => Monad (Conduit i o m) where
    return = Pure []

    Pure is r >>= f = inject is (f r)
    M m >>= f = M (liftM (>>= f) m)
    Await push close >>= f = Await (push >=> f) (close >>= f)
    Yield c o >>= f = Yield (c >>= f) o
    Cleanup c cleanup >>= f = Cleanup (c >>= f) cleanup

instance Monad m => Functor (Conduit i o m) where
    fmap = liftM
instance Monad m => Applicative (Conduit i o m) where
    pure = return
    (<*>) = ap

inject :: Monad m => [i] -> Conduit i o m r -> Conduit i o m r
inject [] c = c
inject is (Pure [] r) = Pure is r
inject is (Pure is' r) = Pure (is' ++ is) r
inject is (M m) = M (liftM (inject is) m)
inject (i:is) (Await f _) = inject is (f i)
inject is (Yield c o) = Yield (inject is c) o
inject is (Cleanup c cleanup) = Cleanup (inject is c) (inject is . cleanup)

(>->) :: Monad m
      => Conduit a b m ()
      -> Conduit b c m r
      -> Conduit a c m r
left >-> right = goR (getCleanup left) left right

getCleanup :: Monad m
           => Conduit i o m r
           -> ([o] -> Conduit i o m ())
getCleanup (Cleanup _ cleanup) = cleanup
getCleanup _ = defaultCleanup

defaultCleanup :: Monad m => [o] -> Conduit i o m ()
defaultCleanup _ = Pure [] ()

goR :: Monad m
    => ([b] -> Conduit a b m ())
    -> Conduit a b m ()
    -> Conduit b c m r
    -> Conduit a c m r
goR cleanup _left (Pure bs r) = dropOutput (cleanup bs) >> Pure [] r
goR cleanup left (M m) = M (liftM (goR cleanup left) m)
goR cleanup left (Await push close) = goL cleanup push close left
goR cleanup left (Yield right' c) = Yield (goR cleanup left right') c
goR cleanup left (Cleanup right' cleanupR) = Cleanup (goR cleanup left right') (doCleanup cleanup cleanupR)

goL :: Monad m
    => ([b] -> Conduit a b m ())
    -> (b -> Conduit b c m r)
    -> Conduit b c m r
    -> Conduit a b m ()
    -> Conduit a c m r
goL cleanup _push close (Pure as ()) = goR (inject as . cleanup) (Pure [] ()) close
goL cleanup push close (M m) = M (liftM (goL cleanup push close) m)
goL cleanup push close (Await pushL closeL) = Await
    (goL cleanup push close . pushL)
    (goL cleanup push close closeL)
goL cleanup push _close (Yield left b) = goR cleanup left (push b)
goL _cleanup push close (Cleanup left cleanup) = goL cleanup push close left

doCleanup :: Monad m
          => ([b] -> Conduit a b m ())
          -> ([c] -> Conduit b c m ())
          -> ([c] -> Conduit a c m ())
doCleanup left right cs = goR left (Pure [] ()) (right cs)

dropOutput :: Monad m
           => Conduit i o m r
           -> Conduit i o' m r
dropOutput (Pure is r) = Pure is r
dropOutput (M m) = M (liftM dropOutput m)
dropOutput (Await push close) = Await (dropOutput . push) (dropOutput close)
dropOutput (Yield c _) = dropOutput c
dropOutput (Cleanup c cleanup) = Cleanup (dropOutput c) (const (dropOutput (cleanup []))) -- this doesn't feel quite right

runConduit :: Monad m => Conduit () Void m r -> m r
runConduit (Pure _ r) = return r
runConduit (M m) = m >>= runConduit
runConduit (Await _ close) = runConduit close
runConduit (Yield _ o) = absurd o
runConduit (Cleanup c _) = runConduit c -- FIXME should we do something with cleanup?

runConduitI :: Conduit () Void Identity r -> r
runConduitI = runIdentity . runConduit

runConduitW :: Monoid w => Conduit () Void (Writer w) r -> (r, w)
runConduitW = runWriter . runConduit

await :: Monad m => Conduit i o m (Maybe i)
await = Await (Pure [] . Just) (Pure [] Nothing)

awaitForever :: Monad m => (i -> Conduit i o m ()) -> Conduit i o m ()
awaitForever f =
    loop
  where
    loop = await >>= maybe (return ()) (\i -> f i >> loop)

yield :: Monad m => o -> Conduit i o m ()
yield = Yield (Pure [] ())

leftover :: Monad m => i -> Conduit i o m ()
leftover i = Pure [i] ()

setCleanup :: Monad m => ([o] -> Conduit i o m ()) -> Conduit i o m ()
setCleanup = Cleanup (Pure [] ())

setFinalizer :: Monad m => m () -> Conduit i o m ()
setFinalizer = Cleanup (Pure [] ()) . const . lift

clearCleanup :: Monad m => Conduit i o m ()
clearCleanup = setCleanup defaultCleanup

lift :: Monad m => m a -> Conduit i o m a
lift = M . liftM (Pure [])

idC :: Monad m => Conduit i i m ()
idC = setCleanup (flip Pure ()) >> awaitForever yield

consume :: Monad m => Conduit i o m [i]
consume =
    loop id
  where
    loop front = await >>= maybe (return (front [])) (\i -> loop (front . (i:)))

foldM :: Monad m => (r -> i -> m r) -> r -> Conduit i o m r
foldM f =
    loop
  where
    loop r = await >>= maybe (return r) (\i -> lift (f r i) >>= loop)

takeExactly :: Monad m => Int -> Conduit i i m ()
takeExactly =
    loop
  where
    loop 0 = clearCleanup
    loop i = setCleanup (const $ drop i) >> await >>= maybe (return ()) (\x -> yield x >> loop (i - 1))

drop :: Monad m => Int -> Conduit i o m ()
drop =
    loop
  where
    loop 0 = return ()
    loop i = await >>= maybe (return ()) (const (loop (i - 1)))

main :: IO ()
main = hspec $ do
    describe "basic ops" $ do
        it "consume" $
            runConduitI (mapM_ yield [1..10] >-> consume) `shouldBe` [1..10 :: Int]
        it "foldM" $
            runConduitI (mapM_ yield [1..10] >-> (foldM (\x y -> return (x + y)) 0)) `shouldBe` (sum [1..10] :: Int)
        it "consume + leftover" $
            runConduitI
                (mapM_ yield [2..10] >-> do
                    leftover (1 :: Int)
                    consume) `shouldBe` [1..10]
    describe "identity without leftovers" $ do
        it "front" $
            runConduitI (idC >-> mapM_ yield [1..10] >-> consume) `shouldBe` [1..10 :: Int]
        it "middle" $
            runConduitI (mapM_ yield [1..10] >-> idC >-> consume) `shouldBe` [1..10 :: Int]
    describe "identity with leftovers" $ do
        it "single" $
            runConduitI (mapM_ yield [2..10] >-> do
                idC >-> leftover (1 :: Int)
                consume) `shouldBe` [1..10]
        it "multiple, separate blocks" $
            runConduitI (mapM_ yield [3..10] >-> do
                idC >-> leftover (2 :: Int)
                idC >-> leftover (1 :: Int)
                consume) `shouldBe` [1..10]
        it "multiple, single block" $
            runConduitI (mapM_ yield [3..10] >-> do
                idC >-> do
                    leftover (2 :: Int)
                    leftover (1 :: Int)
                consume) `shouldBe` [1..10]
    describe "cleanup" $ do
        describe "takeExactly" $ do
            it "undrained" $
                runConduitI (mapM_ yield [1..10 :: Int] >-> do
                    takeExactly 5 >-> return ()
                    consume) `shouldBe` [6..10]
            it "drained" $
                runConduitI (mapM_ yield [1..10 :: Int] >-> do
                    void $ takeExactly 5 >-> consume
                    consume) `shouldBe` [6..10]
    describe "finalizers" $ do
        it "left grouping" $ do
            runConduitW (
                ((setCleanup (const $ say "first") >> say "not called") >->
                (setCleanup (const $ say "second") >> say "not called")) >->
                return ()) `shouldBe` ((), ["second", "first"])
        it "right grouping" $ do
            runConduitW (
                (setCleanup (const $ say "first") >> say "not called") >->
                ((setCleanup (const $ say "second") >> say "not called") >->
                return ())) `shouldBe` ((), ["second", "first"])

say :: String -> Conduit i o (Writer [String]) ()
say = lift . tell . return