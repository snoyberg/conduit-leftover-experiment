{-# OPTIONS_GHC -Wall #-}
module FiveParam where

import Control.Monad hiding (foldM)
import Control.Applicative
import Test.Hspec
import Data.Functor.Identity
--import Data.Void
import Control.Monad.Trans.Writer
--import Data.Monoid
import Prelude hiding (drop)

data Pipe i o r m a
    = Pure [i] (Either r a)
    | M (m (Pipe i o r m a))
    | Await (Maybe i -> Pipe i o r m a)
    | Check ([o] -> Pipe i o r m a) (Pipe i o r m a)
    | Yield (Pipe i o r m a) o

inject :: Monad m => [i] -> Pipe i o r m a -> Pipe i o r m a
inject [] p = p
inject is (Pure is' e) = Pure (is' ++ is) e
inject is (M m) = M (liftM (inject is) m)
inject (i:is) (Await f) = inject is (f (Just i))
inject is (Check close pipe) = Check (inject is . close) (inject is pipe)
inject is (Yield pipe o) = Yield (inject is pipe) o

instance Monad m => Functor (Pipe i o r m) where
    fmap = liftM
instance Monad m => Applicative (Pipe i o r m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (Pipe i o r m) where
    return = Pure [] . Right
    
    Pure is (Left r) >>= _ = Pure is (Left r)
    Pure is (Right a) >>= f = inject is (f a)
    M m >>= f = M (liftM (>>= f) m)
    Await p >>= f = Await (p >=> f)
    Check close pipe >>= f = Check (close >=> f) (pipe >>= f)
    Yield pipe o >>= f = Yield (pipe >>= f) o

runPipeE :: Monad m
         => Pipe i o r m a
         -> m (Either r a)
runPipeE (Pure _ e) = return e
runPipeE (M m) = m >>= runPipeE
runPipeE (Yield pipe _) = runPipeE pipe
runPipeE (Check close _) = runPipeE (close [])
runPipeE (Await f) = runPipeE (f Nothing)

runPipe :: Monad m => Pipe i o r m r -> m r
runPipe = liftM (either id id) . runPipeE

idP :: Monad m => Pipe i i r m ()
idP =
    Check (flip Pure (Right ())) (Await go)
  where
    go Nothing = Pure [] (Right ())
    go (Just o) = Yield idP o

fuse :: Monad m
     => Pipe a b () m ()
     -> Pipe b c r m s
     -> Pipe a c r m s
up `fuse` Pure is e = closeDown is e up
up `fuse` M m = M (liftM (up `fuse`) m)
up `fuse` Await p = takeUp p up
up `fuse` Check close down = Check (fuse up . close) (up `fuse` down)
up `fuse` Yield down mc = Yield (up `fuse` down) mc

takeUp :: Monad m
       => (Maybe b -> Pipe b c r m s)
       -> Pipe a b () m ()
       -> Pipe a c r m s
takeUp down (Pure as e) = Pure as e `fuse` down Nothing
takeUp down (M m) = M (liftM (takeUp down) m)
takeUp down (Await up) = Await (takeUp down . up)
takeUp down (Yield up b) = up `fuse` down (Just b)
takeUp down (Check _ up) = takeUp down up

closeDown :: Monad m
          => [b]
          -> Either r s
          -> Pipe a b () m () -- FIXME try making first () an `r`
          -> Pipe a c r m s
closeDown _ ers (Pure as _) = Pure as ers
closeDown bs ers (M m) = M (liftM (closeDown bs ers) m)
closeDown bs ers (Await p) = Await (closeDown bs ers . p)
closeDown bs ers (Check close _) = closeDown [] ers (close bs)
closeDown bs ers (Yield pipe _) = closeDown bs ers pipe

-- BEGIN TESTS

runPipeI :: Pipe i o r Identity r -> r
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

yield :: Monad m => o -> Pipe i o r m ()
yield = Yield (return ())

sourceList :: Monad m => [o] -> Pipe i o r m ()
sourceList = mapM_ yield
{-
sourceList [] = Pause (\_ -> Done [])
sourceList (ds:dss) = Yield (\x -> if x then sourceList dss else sourceList []) ds
-}

leftover :: Monad m => i -> Pipe i o r m ()
leftover i = Pure [i] (Right ())

{-
setCleanup :: Monad m => ([o] -> Conduit i o m ()) -> Conduit i o m ()
setCleanup = Cleanup (Pure [] ())

setFinalizer :: Monad m => m () -> Conduit i o m ()
setFinalizer = Cleanup (Pure [] ()) . const . lift

clearCleanup :: Monad m => Conduit i o m ()
clearCleanup = setCleanup defaultCleanup
-}

lift :: Monad m => m a -> Pipe i o r m a
lift m = M (liftM return m)

awaitMaybe :: Monad m => Pipe i o r m (Maybe i)
awaitMaybe = Await return

consume :: Monad m => Pipe i o r m [i]
consume =
    loop id
  where
    loop front = awaitMaybe >>= maybe (return (front [])) (\i -> loop (front . (i:)))

{-
(>->) :: Monad m
     => Pipe bs br ba as ar m aa
     -> Pipe cs cr ca bs br m ba
     -> Pipe cs cr ca as ar m aa
-}
(>->) = fuse

foldM :: Monad m => (a -> i -> m a) -> a -> Pipe i o r m a
foldM f =
    loop
  where
    loop r = awaitMaybe >>= maybe (return r) (\i -> lift (f r i) >>= loop)

takeExactly :: Monad m => Int -> Pipe i i r m ()
takeExactly =
    loop
  where
    loop 0 = return ()
    loop i = Check (const $ drop i) (awaitMaybe >>= maybe (return ()) (\x -> yield x >> loop (i - 1)))

drop :: Monad m => Int -> Pipe i o r m ()
drop =
    loop
  where
    loop 0 = return ()
    loop i = awaitMaybe >>= maybe (return ()) (const (loop (i - 1)))

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
            runPipeI ((idP `fuse` (mapM_ yield [1..10])) >-> consume) `shouldBe` [1..10 :: Int]
        it "middle" $
            runPipeI (((mapM_ yield [1..10]) `fuse` idP) >-> consume) `shouldBe` [1..10 :: Int]
{-
        it "back" $
            runPipeI (mapM_ yield [1..10] >-> (consume `fuse` idP)) `shouldBe` [1..10 :: Int]
-}
    describe "identity with leftovers" $ do
        it "single" $
            runPipeI (mapM_ yield [2..10] >-> do
                idP `fuse` leftover (1 :: Int)
                consume) `shouldBe` [1..10]
        it "multiple, separate blocks" $
            runPipeI (mapM_ yield [3..10] >-> do
                idP >-> leftover (2 :: Int)
                idP >-> leftover (1 :: Int)
                consume) `shouldBe` [1..10]
        it "multiple, single block" $
            runPipeI (mapM_ yield [3..10] >-> do
                idP >-> do
                    leftover (2 :: Int)
                    leftover (1 :: Int)
                consume) `shouldBe` [1..10]
    describe "cleanup" $ do
        describe "takeExactly" $ do
            it "undrained" $
                runPipeI (mapM_ yield [1..10 :: Int] >-> do
                    takeExactly 5 >-> return ()
                    consume) `shouldBe` [6..10]
            it "drained" $
                runPipeI (mapM_ yield [1..10 :: Int] >-> do
                    void $ takeExactly 5 >-> consume
                    consume) `shouldBe` [6..10]
{-
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

say :: String -> Pipe i o r (Writer [String]) ()
say = lift . tell . return