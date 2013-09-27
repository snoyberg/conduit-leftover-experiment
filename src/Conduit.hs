{-# LANGUAGE TypeFamilies, EmptyDataDecls, TupleSections #-}
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
import Control.Arrow (first)
import Data.Functor.Identity

leftover :: Monad m => i -> PipeL i o d m ()
leftover i = PipeL $ const $ return ([i], ())

yieldL :: Monad m => o -> PipeL i o d m (Maybe d)
yieldL o = PipeL $ \_mosd -> do
    mres <- yield o
    case mres of
        Nothing -> return ([], Nothing)
        Just (_downstreamleftovers, x) -> return ([], Just x)

awaitL :: Monad m => PipeL i o d m (Maybe i)
awaitL = PipeL $ const $ liftM (\x -> ([], x)) await

takeL :: Monad m => Int -> PipeL i i d m ()
takeL 0 = return ()
takeL i = awaitL >>= maybe (return ()) (\x -> yieldL x >> (takeL $ i - 1))

consumeL :: Monad m => PipeL i o d m [i]
consumeL =
    loop id
  where
    loop front = awaitL >>= maybe (return $ front []) (\x -> loop (front . (x:)))

sourceListL :: Monad m => [o] -> PipeL i o d m ()
sourceListL [] = return ()
sourceListL (o:os) = yieldL o >>= maybe (sourceListL os) (const $ return ())

stopL :: Monad m => PipeL i o r m r
stopL = PipeL $ \mosd ->
    case mosd of
        Just (_, r) -> return ([], r)
        Nothing -> do
            (_leftovers, r) <- stop
            return ([], r)

newtype ComposePipeL m up down = ComposePipeL
    { runComposePipeL :: PipeL (Stream up) (Stream down) (Result down) m (Result up)
    }

idL :: Monad m => PipeL i i r m r
idL = PipeL $ \mosd ->
        case mosd of
            Just (os, d) -> return (os, d)
            Nothing -> idP


instance Monad m => Category (ComposePipeL m) where
    id = ComposePipeL $ PipeL $ \mosd ->
        case mosd of
            Just (os, d) -> return (os, d)
            Nothing -> idP
    ComposePipeL down . ComposePipeL up = ComposePipeL $ up `pipeL` down

newtype PipeL i o d m r = PipeL
    { runPipeL :: Maybe ([o], d) -> Pipe i o ([o], d) m ([i], r) -- FIXME return (Maybe d) next to r
    }
    
runPipeLI :: PipeL i o () Identity r -> r
runPipeLI (PipeL p) = snd $ runIdentity $ runPipe' ([], ()) (p Nothing)

pipeL :: Monad m
      => PipeL i j b m a
      -> PipeL j k c m b
      -> PipeL i k c m a
pipeL (PipeL up) (PipeL down) = PipeL $ fuse up . down

instance Monad m => Monad (PipeL i o d m) where
    return = PipeL . const . return . ([], )
    PipeL f >>= g = PipeL $ \mosd -> do
        (is, r) <- f mosd
        let PipeL g' = g r
        inject is $ g' $ fmap (first $ const []) mosd

inject :: Monad m => [i] -> Pipe i o d m ([i], r) -> Pipe i o d m ([i], r)
inject [] p = p
inject is (Pure (is', r)) = Pure (is' ++ is, r)
inject is (M m) = M (liftM (inject is) m)
inject (i:is) (Await f) = inject is (f (Just i))
inject is (Yield f o) = Yield (inject is . f) o

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

runPipe' :: Monad m
         => d
         -> Pipe i o d m r
         -> m r
runPipe' _ (Pure r) = return r
runPipe' d (M m) = m >>= runPipe' d
runPipe' d (Await f) = runPipe' d (f Nothing)
runPipe' d (Yield f _) = runPipe' d (f (Just d))

runPipe :: Monad m => Pipe i o () m r -> m r
runPipe = runPipe' ()

(>->) :: Monad m
      => Pipe i j b m a
      -> Pipe j k c m b
      -> Pipe i k c m a
up >-> down = fuse (const up) down

type family Stream a
type family Result a

newtype ComposePipe m up down = ComposePipe
    { runComposePipe :: Maybe (Result down)
                     -> Pipe (Stream up) (Stream down) (Result down) m (Result up)
    }

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

runPipeI :: Pipe i o () Identity r -> r
runPipeI = runIdentity . runPipe

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

consume :: Monad m => Pipe i o r m [i]
consume =
    go id
  where
    go front = await >>= maybe (return (front [])) (\i -> go (front . (i:)))

(>+>) :: Monad m
      => Pipe i j b m ()
      -> Pipe j k c m b
      -> Pipe i k c m b
up >+> down = (up >> stop) >-> down

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
    describe "consume" $ do
        it "no idP" $
            runPipeI (sourceList [1 :: Int ..] >+> take 10 >+> consume)
                `shouldBe` [1..10]
        it "idP front" $
            runPipeI (idP >-> sourceList [1 :: Int ..] >+> take 10 >+> consume)
                `shouldBe` [1..10]
        it "idP middle1" $
            runPipeI (sourceList [1 :: Int ..] >+> idP >-> take 10 >+> consume)
                `shouldBe` [1..10]
        it "idP middle2" $
            runPipeI (sourceList [1 :: Int ..] >+> take 10 >+> idP >-> consume)
                `shouldBe` [1..10]
        it "idP back" $
            runPipeI (sourceList [1 :: Int ..] >+> take 10 >+> consume >-> idP)
                `shouldBe` [1..10]
    describe "leftovers" $ do
        it "works" $ do
            let sink :: PipeL Int () () Identity [Int]
                sink = do
                    leftover (1 :: Int)
                    pipeL (takeL 5 >> stopL) consumeL
                --src :: PipeL () Int [Int] Identity [Int]
                src = sourceListL [2..20] >> stopL
                res = runPipeLI $ pipeL src sink
            res `shouldBe` [1..5]
        it "idL front" $ do
            let sink :: PipeL Int () () Identity [Int]
                sink = do
                    leftover (1 :: Int)
                    pipeL (takeL 5 >> stopL) consumeL
                --src :: PipeL () Int [Int] Identity [Int]
                src = sourceListL [2..20] >> stopL
                res = runPipeLI $ pipeL idL (pipeL src sink)
            res `shouldBe` [1..5]
        it "idL middle" $ do
            let sink :: PipeL Int () () Identity [Int]
                sink = do
                    leftover (1 :: Int)
                    pipeL (takeL 5 >> stopL) consumeL
                --src :: PipeL () Int [Int] Identity [Int]
                src = sourceListL [2..20] >> stopL
                res = runPipeLI $ pipeL src (pipeL idL sink)
            res `shouldBe` [1..5]
        it "idL back" $ do
            let sink :: PipeL Int () () Identity [Int]
                sink = do
                    leftover (1 :: Int)
                    pipeL (takeL 5 >> stopL) consumeL
                --src :: PipeL () Int [Int] Identity [Int]
                src = sourceListL [2..20] >> stopL
                res = runPipeLI $ pipeL src (pipeL sink idL)
            res `shouldBe` [1..5]
    return ()