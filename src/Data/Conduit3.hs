module Data.Conduit3 where

import           Control.Applicative
import           Control.Monad
import Data.Void

data Conduit i o m r
    = Pure r
    | M (m (Conduit i o m r)) (Conduit i Void m ())
    | Await (i -> Conduit i o m r) (Conduit i o m r) (Conduit i Void m ())
    | Yield (Conduit i o m r) (Conduit i Void m ()) o
    | Leftover (Conduit i o m r) i

instance Monad m => Monad (Conduit i o m) where
    return = Pure

    Pure r >>= fp = fp r
    M mp done >>= fp = M ((>>= fp) `liftM` mp) done
    Await p c done >>= fp = Await (p >=> fp) (c >>= fp) done
    Yield p done o >>= fp = Yield (p >>= fp) done o
    Leftover p i >>= fp = Leftover (p >>= fp) i

instance Monad m => Functor (Conduit i o m) where
    fmap = liftM
instance Monad m => Applicative (Conduit i o m) where
    pure = return
    (<*>) = ap

idP :: Monad m => Conduit a a m ()
idP = Await (Yield idP (return ())) (Pure ()) (Pure ())

runConduit :: Monad m => Conduit () Void m r -> m r
runConduit (Pure r) = return r
runConduit (M action _) = action >>= runConduit
runConduit (Await p _ _) = runConduit (p ())
runConduit (Yield _ _ o) = absurd o
runConduit (Leftover p ()) = runConduit p

{-
    = Pure r
    | M (m (Conduit i o m r)) (Conduit i Void m ())
    | Await (i -> Conduit i o m r) (Conduit i o m r) (Conduit i Void m ())
    | Yield (Conduit i o m r) (Conduit i Void m ()) o
    | Leftover (Conduit i o m r) i
-}

fuse :: Monad m
     => Conduit a b m ()
     -> Conduit b c m r
     -> Conduit a c m r
fuse left0 =
    goR (cleanup left0) left0
  where
    goR done left right =
        case right of
            Pure r -> cleanup left >> Pure r
            M action _ -> M (liftM recurse action) done
      where
        recurse = goR done left

cleanup :: Monad m => Conduit i o m () -> Conduit i o' m ()
cleanup (Pure ()) = Pure ()
cleanup (M _ x) = noOutput x
cleanup (Await _ _ x) = noOutput x
cleanup (Yield _ x _) = noOutput x
cleanup (Leftover p _) = cleanup p

noOutput :: Monad m => Conduit i Void m r -> Conduit i o m r
noOutput (Pure r) = Pure r
noOutput (M action done) = M (liftM noOutput action) done
noOutput (Await p c done) = Await (noOutput . p) (noOutput c) done
noOutput (Leftover p i) = Leftover (noOutput p) i
    {-
    goR (return ())
  where
    goR left right =
        case right of
            Pure r -> Pure r
            M mp done -> M (liftM recurse mp) done
            Await l rp rc -> goL l rp rc left
            Yield p c o -> Yield (recurse . p) (c >> final) o
      where
        recurse = goR final left

    goL leftover rp rc final left =
        case left of
            Pure r -> goR (return ()) (Pure r) rc
            M mp -> M (liftM recurse mp)
            Await l lp lc -> Await l (recurse . lp) (recurse lc)
            Yield left' final' o -> goR final' (left' leftover) (rp o)
      where
        recurse = goL leftover rp rc final
-}