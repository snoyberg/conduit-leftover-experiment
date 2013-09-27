{-# LANGUAGE RankNTypes #-}
module Data.Conduit2 where

import           Control.Applicative
import           Control.Monad

data Conduit i o m r
    = Pure [i] r
    | M (m (Conduit i o m r))
    | Await (i -> Conduit i o m r) (Conduit i o m r)
    | Yield (Maybe o -> Conduit i o m r) (m ()) o

instance Monad m => Monad (Conduit i o m) where
    return = Pure

    Pure r >>= fp = fp r
    M mp >>= fp = M ((>>= fp) `liftM` mp)
    Await i p c >>= fp = Await i (p >=> fp) (c >>= fp)
    Yield p f o >>= fp = Yield (p >=> fp) f o

instance Monad m => Functor (Conduit i o m) where
    fmap = liftM
instance Monad m => Applicative (Conduit i o m) where
    pure = return
    (<*>) = ap

idP :: Monad m => Conduit a a m ()
idP =
    go Nothing
  where
    go x = Await x (Yield go (return ())) (Pure ())

fuse :: Monad m
     => Conduit a b m ()
     -> Conduit b c m r
     -> Conduit a c m r
fuse =
    goR (return ())
  where
    goR final left right =
        case right of
            Pure r -> M (final >> return (Pure r))
            M mp -> M (liftM recurse mp)
            Await l rp rc -> goL l rp rc final left
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
