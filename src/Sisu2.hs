{-# OPTIONS_GHC -Wall #-}
module Sisu2 where

import Control.Monad
import Control.Applicative
import Test.Hspec
import Data.Functor.Identity
--import Data.Void
import Control.Monad.Trans.Writer
--import Data.Monoid
import Prelude hiding (drop)

type IsDownstreamOpen = Bool

data Pipe ds dr da us ur m ua
    = M (m (Pipe ds dr da us ur m ua))
    | Done [us] (Either ur ua)
    | Pause ([ds] -> Either dr da -> Pipe ds dr da us ur m ua)
    | Empty (Pipe ds dr da us ur m ua)
    | Check (IsDownstreamOpen -> Pipe ds dr da us ur m ua)
    | Yield (IsDownstreamOpen -> Pipe ds dr da us ur m ua) ds
    | Await (Maybe us -> Pipe ds dr da us ur m ua)

inject :: Monad m => [us] -> Pipe ds dr da us ur m ua -> Pipe ds dr da us ur m ua
inject [] p = p
inject is (M m) = M (liftM (inject is) m)
inject is (Done is' a) = Done (is' ++ is) a
inject is (Pause f) = Pause (\x y -> inject is (f x y))
inject is (Empty f) = Empty (inject is f)
inject is (Check f) = Check (inject is . f)
inject is (Yield f o) = Yield (inject is . f) o
inject (i:is) (Await f) = inject is (f (Just i))

instance Monad m => Functor (Pipe ds dr da us ur m) where
    fmap = liftM

instance Monad m => Applicative (Pipe ds dr da us ur m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Pipe ds dr da us ur m) where
    return = Done [] . Right

    M m >>= fp = M (liftM (>>= fp) m)
    Done uss (Left ur) >>= _ = Done uss (Left ur)
    Done uss (Right ua) >>= fp = inject uss (fp ua)
    Pause f >>= fp = Pause (\x y -> f x y >>= fp)
    Empty close >>= fp = Empty (close >>= fp)
    Check p >>= fp = Check (p >=> fp)
    Yield p ds >>= fp = Yield (p >=> fp) ds
    Await f >>= fp = Await (f >=> fp)

runPipeE :: Monad m => Pipe ds dr () us ur m ua -> m (Either ur ua)
runPipeE (M m) = m >>= runPipeE
runPipeE (Done _ x) = return x
runPipeE (Pause f) = runPipeE (f [] (Right ()))
runPipeE (Empty f) = runPipeE f
runPipeE (Check f) = runPipeE (f False)
runPipeE (Yield f _) = runPipeE (f False)
runPipeE (Await f) = runPipeE (f Nothing)

runPipe :: Monad m => Pipe ds dr () us ua m ua -> m ua
runPipe = liftM (either id id) . runPipeE

idP :: Monad m => Pipe ds dr da ds dr m da
idP =
    Check go
  where
    go True = Await (maybe (Empty (Pause Done)) (Yield go))
    go False = Pause Done

fuse :: Monad m
     => Pipe bs br ba as ar m aa
     -> Pipe cs cr ca bs br m ba
     -> Pipe cs cr ca as ar m aa
up `fuse` M m = M (liftM (up `fuse`) m)
up `fuse` Done bss be = closeDown bss be up
up `fuse` Empty down = Empty (up `fuse` down)
up `fuse` Check down = Check (\x -> up `fuse` down x)
up `fuse` Yield down cs = Yield (\x -> up `fuse` down x) cs
up `fuse` Await f = takeUp f up
_up `fuse` Pause _ = error "fuse Pause"

takeUp :: Monad m
       => (Maybe bs -> Pipe cs cr ca bs br m ba)
       -> Pipe bs br ba as ar m aa
       -> Pipe cs cr ca as ar m aa
takeUp f (M m) = M (liftM (takeUp f) m)
takeUp f (Done ass ae) = Done ass ae `fuse` f Nothing
takeUp f (Pause close) = Pause close `fuse` f Nothing
takeUp f (Empty close) = Empty close `fuse` f Nothing
takeUp f (Check up) = takeUp f (up True)
takeUp f (Yield up bs) = up True `fuse` f (Just bs)
takeUp down (Await up) = Await (takeUp down . up)

closeDown :: Monad m
          => [bs]
          -> Either br ba
          -> Pipe bs br ba as ar m aa
          -> Pipe cs cr ca as ar m aa
closeDown bss be (M m) = M (liftM (closeDown bss be) m)
closeDown _ _ (Done bss x) = Done bss x
closeDown bss be (Pause close) = closeDown [] be (close bss be)
closeDown bss be (Empty close) = closeDown bss be close
closeDown bss be (Check close) = closeDown bss be (close False)
closeDown bss be (Yield close _) = closeDown bss be (close False)
closeDown bss be (Await f) = Await (closeDown bss be . f)

-- BEGIN TESTS

runPipeI :: Pipe ds dr () us ua Identity ua -> ua
runPipeI = runIdentity . runPipe

{-
runConduitW :: Monoid w => Conduit () Void (Writer w) r -> (r, w)
runConduitW = runWriter . runConduit

await :: Monad m => Conduit i o m (Maybe i)
await = Await (Pure [] . Just) (Pure [] Nothing)

awaitForever :: Monad m => (i -> Conduit i o m ()) -> Conduit i o m ()
awaitForever f =
    loop
  where
    loop = await >>= maybe (return ()) (\i -> f i >> loop)
-}

yield :: Monad m => ds -> Pipe ds dr da us dr m da
yield =
    Yield (const $ Pause done)
  where
    done _ = Done []

sourceList :: Monad m => [ds] -> Pipe ds dr da us dr m da
sourceList [] = Pause (\_ -> Done [])
sourceList (ds:dss) = Yield (\x -> if x then sourceList dss else sourceList []) ds

leftover :: Monad m => us -> Pipe ds dr da us ur m ()
leftover i = Done [i] (Right ())

{-
setCleanup :: Monad m => ([o] -> Conduit i o m ()) -> Conduit i o m ()
setCleanup = Cleanup (Pure [] ())

setFinalizer :: Monad m => m () -> Conduit i o m ()
setFinalizer = Cleanup (Pure [] ()) . const . lift

clearCleanup :: Monad m => Conduit i o m ()
clearCleanup = setCleanup defaultCleanup
-}

lift :: Monad m => m ua -> Pipe ds dr da us ur m ua
lift m = M (liftM (Done [] . Right) m)

awaitMaybe :: Monad m => Pipe ds dr da us ur m (Maybe us)
awaitMaybe = Await (Done [] . Right)

consume :: Monad m => Pipe ds dr da us ur m [us]
consume =
    loop id
  where
    loop front = awaitMaybe >>= maybe (return (front [])) (\i -> loop (front . (i:)))

(>->) :: Monad m
     => Pipe bs br ba as ar m aa
     -> Pipe cs cr ca bs br m ba
     -> Pipe cs cr ca as ar m aa
(>->) = fuse

foldM :: Monad m => (a -> us -> m a) -> a -> Pipe ds dr da us ur m a
foldM f =
    loop
  where
    loop r = awaitMaybe >>= maybe (return r) (\i -> lift (f r i) >>= loop)

{-
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
-}

{-
foo :: Monad m => Pipe i Int r r m ()
foo = idP `fuse` passUp (mapM_ yield [1..10])

bar :: Monad m => Pipe i Int r r m a
bar = passUp (mapM_ yield [1..10]) `fuse` idP

baz :: Monad m => Pipe i Int r r m a
baz = passUp (mapM_ yield [1..10])

bin :: Monad m => Pipe Int o r d m ()
bin = {-idP `fuse` -}leftover (1 :: Int)
-}

main :: IO ()
main = hspec $ do
    return ()
    describe "basic ops" $ do
        it "consume" $
            runPipeI (sourceList [1..10] `fuse` consume) `shouldBe` [1..10 :: Int]
        it "two source lists" $
            runPipeI ((sourceList [1..5] >> sourceList [6..10]) >-> consume) `shouldBe` [1..10 :: Int]
    {-
        it "mapM_ yield" $
            runPipeI (mapM_ yield [1..10] >-> consume) `shouldBe` [1..10 :: Int]
        it "foldM" $
            runPipeI (mapM_ yield [1..10] >-> (foldM (\x y -> return (x + y)) 0)) `shouldBe` (sum [1..10] :: Int)
        it "consume + leftover" $
            runPipeI
                (mapM_ yield [2..10] >-> do
                    leftover (1 :: Int)
                    consume) `shouldBe` [1..10]
    describe "identity without leftovers" $ do
        it "front" $
            runPipeI ((idP `fuse` passUp (mapM_ yield [1..10])) >-> consume) `shouldBe` [1..10 :: Int]
        it "middle" $
            runPipeI ((passUp (mapM_ yield [1..10]) `fuse` idP) >-> consume) `shouldBe` [1..10 :: Int]
        it "back" $
            runPipeI (mapM_ yield [1..10] >-> (consume `fuse` idP)) `shouldBe` [1..10 :: Int]
            -}
{-
    describe "identity with leftovers" $ do
        it "single" $
            runPipeI (mapM_ yield [2..10] >-> do
                idP `fuse` leftover (1 :: Int)
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
-}

say :: String -> Pipe i o r d b (Writer [String]) ()
say = lift . tell . return