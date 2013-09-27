{-# LANGUAGE TypeFamilies, EmptyDataDecls #-}
{-# OPTIONS_GHC -Wall #-}
module Conduit where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Test.Hspec
import Prelude hiding (take, id, (.))
import Control.Category

data Pipe i o d m r
    = Pure r
    | M (m (Pipe i o d m r))
    | Await (Maybe i -> Pipe i o d m r)
    | Yield (Maybe d -> Pipe i o d m r) (Maybe o) -- Nothing means I have no more output

instance Monad m => Monad (Pipe i o d m) where
    return = Pure
    
    Pure r >>= f = f r
    M m >>= f = M (liftM (>>= f) m)
    Await f >>= g = Await (f >=> g)
    Yield f o >>= g = Yield (f >=> g) o
instance Monad m => Functor (Pipe i o d m) where
    fmap = liftM
instance Monad m => Applicative (Pipe i o d m) where
    pure = return
    (<*>) = ap
instance MonadTrans (Pipe i o d) where
    lift = M . liftM return

idP :: Monad m => Pipe i i r m r
idP = Await (Yield (maybe idP Pure))

runPipe :: Monad m
        => Pipe i o () m r
        -> m r
runPipe (Pure r) = return r
runPipe (M m) = m >>= runPipe
runPipe (Await f) = runPipe (f Nothing)
runPipe (Yield f _) = runPipe (f (Just ()))

(>->) :: Monad m
      => Pipe i j b m a
      -> Pipe j k c m b
      -> Pipe i k c m a
up >-> down = fuse (const up) down

type family Stream a
type family Result a

data Pair i r

type instance Stream (Pair i r) = i
type instance Result (Pair i r) = r

newtype ComposePipe m up down = ComposePipe (Maybe (Result down) -> Pipe (Stream up) (Stream down) (Result down) m (Result up))

compose :: Monad m
        => ComposePipe m b c
        -> ComposePipe m a b
        -> ComposePipe m a c
compose (ComposePipe down) (ComposePipe up) = ComposePipe $ \mc -> up `fuse` down mc

instance Monad m => Category (ComposePipe m) where
    id = ComposePipe $ maybe idP Pure
    ComposePipe down . ComposePipe up = ComposePipe $ \mc -> up `fuse` down mc

fuse :: Monad m
     => (Maybe b -> Pipe i j b m a)
     -> Pipe j k c m b
     -> Pipe i k c m a
up `fuse` Pure r = downClosed r (up (Just r))
up `fuse` M m = M (liftM (up `fuse`) m)
up `fuse` Await f = upWait f (up Nothing)
up `fuse` Yield f k = Yield (fuse up . f) k

upWait :: Monad m
       => (Maybe j -> Pipe j k c m b)
       -> Pipe i j b m a
       -> Pipe i k c m a
upWait f (Pure a) = Pure a >-> f Nothing
upWait f (M m) = M (liftM (upWait f) m)
upWait f (Await g) = Await (upWait f . g)
upWait f (Yield g j) = g `fuse` f j

downClosed :: Monad m
           => b
           -> Pipe i j b m a
           -> Pipe i k c m a
downClosed _ (Pure a) = Pure a
downClosed b (M m) = M (liftM (downClosed b) m)
downClosed b (Await f) = Await (downClosed b . f)
downClosed b (Yield f _) = downClosed b (f (Just b))

-- BEGIN TESTS

runPipeW :: Monoid w => Pipe i o () (Writer w) r -> (r, w)
runPipeW = runWriter . runPipe

await :: Monad m => Pipe i o d m (Maybe i)
await = Await Pure

awaitForever :: Monad m
             => (i -> Pipe i o d m ())
             -> Pipe i o d m d
awaitForever f =
    loop
  where
    loop = await >>= maybe stop (f >=> const loop)

stop :: Monad m => Pipe i o r m r
stop = Yield (maybe stop Pure) Nothing

yield :: Monad m => o -> Pipe i o r m (Maybe r)
yield = Yield Pure . Just

sourceList :: Monad m => [o] -> Pipe i o r m ()
sourceList [] = return ()
sourceList (o:os) = Yield (maybe (sourceList os) (const (Pure ()))) (Just o)

take :: Monad m => Int -> Pipe o o r m ()
take 0 = return ()
take count = await >>= maybe (return ()) (yield >=> maybe (take (count - 1)) (const $ return ()))

main :: IO ()
main = hspec $ do
    describe "basics" $ do
        it "no idP" $
            runPipeW (sourceList ([1..10] ++ [undefined]) >-> take 10 >-> awaitForever (lift . tell . return))
                `shouldBe` ((), [1..10 :: Int])
        it "idP front" $
            runPipeW (idP >-> sourceList [1..10] >-> take 10 >-> awaitForever (lift . tell . return))
                `shouldBe` ((), [1..10 :: Int])
        it "idP middle1" $
            runPipeW (sourceList [1..] >-> idP >-> take 10 >-> awaitForever (lift . tell . return))
                `shouldBe` ((), [1..10 :: Int])
        it "idP middle2" $
            runPipeW (sourceList [1..] >-> take 10 >-> idP >-> awaitForever (lift . tell . return))
                `shouldBe` ((), [1..10 :: Int])
        it "idP back" $
            runPipeW (sourceList [1..] >-> take 10 >-> awaitForever (lift . tell . return) >-> idP)
                `shouldBe` ((), [1..10 :: Int])
    return ()