module Sisu where

import Control.Monad hiding (foldM)
import Control.Applicative
import Test.Hspec
import Data.Functor.Identity
import Data.Void
import Control.Monad.Trans.Writer
import Data.Monoid
import Prelude hiding (drop)

data Pipe i o r d b m a
    = M (m (Pipe i o r d b m a))
    | Done [i] (Either r a)
    | Empty ([o] -> Either d b -> Pipe i o r d b m a)
    | Yield ([o] -> Either d b -> Pipe i o r d b m a) (Pipe i o r d b m a) (Maybe o)
    | Await (i -> Pipe i o r d b m a) (Pipe i o r d b m a)

inject :: Monad m => [i] -> Pipe i o r d b m a -> Pipe i o r d b m a
inject [] p = p
inject is (M m) = M (liftM (inject is) m)
inject is (Done is' a) = Done (is' ++ is) a
inject is (Empty f) = Empty (\os d -> inject is (f os d))
inject is (Yield f p o) = Yield (\os d -> inject is (f os d)) (inject is p) o
inject (i:is) (Await more _) = inject is (more i)

instance Monad m => Functor (Pipe i o r d b m) where
    fmap = liftM
instance Monad m => Applicative (Pipe i o r d b m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (Pipe i o r d b m) where
    return = Done [] . Right

    M m >>= fp = M (liftM (>>= fp) m)
    Done is (Left r) >>= _ = Done is (Left r)
    Done is (Right a) >>= fp = inject is (fp a)
    Empty f >>= fp = Empty (\os d -> f os d >>= fp)
    Yield f p o >>= fp = Yield (\os d -> f os d >>= fp) (p >>= fp) o
    Await more none >>= fp = Await (more >=> fp) (none >>= fp)

runPipe :: Monad m => Pipe () () a () () m a -> m a
runPipe (M m) = m >>= runPipe
runPipe (Done _ x) = return (either id id x)
runPipe (Empty f) = runPipe (f [] (Right ()))
runPipe (Yield f _ _) = runPipe (Empty f)
runPipe (Await _ none) = runPipe none

idP :: Monad m => Pipe i i r r a m a
idP =
    Yield Done (Await (Yield Done idP . Just) (Empty Done)) Nothing

{-
fuse :: Monad m
     => Pipe i j r b m a
     -> Pipe j k b c m b
     -> Pipe i k r c m a
-}
up `fuse` M m = M (liftM (up `fuse`) m)
up `fuse` Done js (Left b) = closeDown js b up
up `fuse` Done js (Right b) = closeDown js b up
up `fuse` Empty f = Empty (\ks c -> up `fuse` f ks c)
up `fuse` Yield f p o = Yield (\ks c -> up `fuse` f ks c) (up `fuse` p) o
up `fuse` Await more none = takeUp more none up

-- | take from upstream
{-
takeUp :: Monad m
       => (j -> Pipe j k b c m b)
       -> Pipe j k b c m b
       -> Pipe i j r b m a
       -> Pipe i k r c m a
-}
takeUp more none (M m) = M (liftM (takeUp more none) m)
takeUp _ none (Done is a) = Done is a `fuse` none
takeUp _ none (Empty f) = Empty f `fuse` none
takeUp more _ (Yield _ p (Just o)) = p `fuse` more o
takeUp more none (Yield _ p Nothing) = takeUp more none p
takeUp moreD noneD (Await moreU noneU) = Await
    (takeUp moreD noneD . moreU)
    (takeUp moreD noneD noneU)

-- | close from downstream
{-
closeDown :: Monad m
          => [j]
          -> b
          -> Pipe i j r b m a
          -> Pipe i k r c m a
-}
closeDown :: Monad m
          => [j]
          -> Either b t8
          -> Pipe i j r b t8 m a
          -> Pipe i k r c b m a
closeDown js b (M m) = M (liftM (closeDown js b) m)
closeDown _ _ (Done is a) = Done is a
closeDown js b (Empty f) = closeDown [] b (f js b)
closeDown js b (Yield f _ _) = closeDown [] b (f js b)
closeDown js b (Await more none) = Await
    (closeDown js b . more)
    (closeDown js b none)

-- BEGIN TESTS

runPipeI :: Pipe () () a () () Identity a -> a
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

yield :: Monad m => o -> Pipe i o r r b m ()
yield = Yield (\_ _ -> Done [] (error "yield")) (Done [] (Right ())) . Just

sourceList :: Monad m => [o] -> Pipe i o d d b m ()
sourceList [] = Done [] (Right ())
sourceList (o:os) = Yield (\_ _ -> Done [] (error "sourceList")) (sourceList os) (Just o)

leftover :: Monad m => i -> Pipe i o r d b m ()
leftover i = Done [i] (Right ())

{-
setCleanup :: Monad m => ([o] -> Conduit i o m ()) -> Conduit i o m ()
setCleanup = Cleanup (Pure [] ())

setFinalizer :: Monad m => m () -> Conduit i o m ()
setFinalizer = Cleanup (Pure [] ()) . const . lift

clearCleanup :: Monad m => Conduit i o m ()
clearCleanup = setCleanup defaultCleanup
-}

lift :: Monad m => m a -> Pipe i o r d b m a
lift m = M (liftM (\a -> Done [] (Right a)) m)

awaitMaybe :: Monad m => Pipe i o r d b m (Maybe i)
awaitMaybe = Await (Done [] . Right . Just) (Done [] (Right Nothing))

--consume :: Monad m => Pipe i o [i] d m [i]
consume :: Monad m => Pipe i o r d b m [i]
consume =
    loop id
  where
    loop front = awaitMaybe >>= maybe (return (front [])) (\i -> loop (front . (i:)))

{-
(>->) :: Monad m
      => Pipe i j a a m ()
      -> Pipe j k a c m a
      -> Pipe i k a c m a
-}
up >-> down = (passUp up) `fuse` down

passUp :: Monad m => Pipe i o r r b m () -> Pipe i o r r b m b
passUp = (>> Empty (\_ r -> Done [] (r)))

--foldM :: Monad m => (a -> i -> m a) -> a -> Pipe i o r d m a
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
    {-
    describe "basic ops" $ do
        it "consume" $
            runPipeI (sourceList [1..10] `fuse` consume) `shouldBe` [1..10 :: Int]
        it "two source lists" $
            runPipeI ((sourceList [1..5] >> sourceList [6..10]) >-> consume) `shouldBe` [1..10 :: Int]
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