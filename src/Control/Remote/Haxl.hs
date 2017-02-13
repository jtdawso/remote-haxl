{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Haxl
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson (jdawson@ku.edu)
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Haxl
  ( -- * Remote Haxl
    RemoteHaxlMonad
--  , RemoteMonadException(..)
    -- * The primitive lift function
  , query
    -- * The run functions
  , RunHaxlMonad(runHaxlMonad)
  , runWeakMonad
--  , runApplicativeMonad
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Haxl.Applicative as A
import           Control.Remote.Haxl.Packet.Applicative as A
import           Control.Remote.Haxl.Packet.Weak as Weak
import           Control.Remote.Haxl.Types as T
import           Control.Applicative

import Control.Natural


-- | promote a procedure into the remote monad
query :: q a -> RemoteHaxlMonad q a
query = Appl . A.query

-- | 'RunHaxlMonad' is the overloading for choosing the appropriate bundling strategy for a monad.
class RunHaxlMonad f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runHaxlMonad :: (Monad m) => (f q :~> m) -> (RemoteHaxlMonad q :~> m)

instance RunHaxlMonad WeakPacket where
  runHaxlMonad = runWeakMonad

--instance RunHaxlMonad ApplicativePacket where
--  runHaxlMonad = runApplicativeMonad

-- | This is a remote monad combinator, that takes an implementation
--   of a remote applicative, splits the monad into applicatives
--   without any merge stragegy, and uses the remote applicative.
--   Every '>>=' will generate a call to the 'RemoteHaxlApplicative'
--   handler; as well as one terminating call.
--   Using 'runBindeeMonad' with a 'runWeakApplicative' gives the weakest remote monad.
runMonadSkeleton :: (Monad m) => (RemoteHaxlApplicative q :~> m) -> (RemoteHaxlMonad q :~> m)
runMonadSkeleton f = wrapNT $ \ case
  Appl g   -> unwrapNT f g
  Bind g k -> (runMonadSkeleton f # g) >>= \ a -> runMonadSkeleton f # (k a)
  Ap' g h  -> (runMonadSkeleton f # g) <*> (runMonadSkeleton f # h)
-- | This is the classic weak remote monad, or technically the
--   weak remote applicative weak remote monad.
runWeakMonad :: (Monad m) => (WeakPacket q :~> m) -> (RemoteHaxlMonad q :~> m)
runWeakMonad = runMonadSkeleton . A.runHaxlWeakApplicative

{-
-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteApplicative') as large as possible,
--   including over some monadic binds.
runApplicativeMonad :: forall m q . (MonadCatch m) => (A.ApplicativePacket q :~> m) -> (RemoteMonad q :~> m)
runApplicativeMonad (NT rf) = wrapNT $ \ p -> do
    (r,h) <-  runStateT (runMaybeT (go2 p)) (pure ())
    case  pk h of -- should we stub out the call with only 'Pure'?
      Pure' a ->  return a
      Pkt f b ->  do res <- rf $ b
                     return $ f res
    case r of
      Nothing -> throwM RemoteEmptyException
      Just v -> return v
  where
    go2 :: forall a . RemoteMonad q a -> MaybeT (StateT (RemoteApplicative q ()) m) a
    go2 (Appl app)   = lift $ unwrap $ go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' g h)    = go2 g <*> go2 h

    go :: forall a . T.RemoteApplicative q a -> Wrapper (RemoteApplicative q) a

    go (T.Pure a) = pure a
    go (T.Query p) = Value (T.Query p)
    go (T.Ap g h)      = (go g) <*> (go h)

    -- g is a function that will take the current state as input
    discharge :: forall a f . Applicative f => (f () ->RemoteApplicative q a )-> StateT (f ()) m a
    discharge g = do
                 ap' <- get
                 put (pure ()) -- clear state
                 case pk $ g ap' of
                    Pure' a -> return a
                    Pkt f pkt -> do
                                    res <- lift $ rf pkt
                                    return $ f res
    -- Given a wrapped applicative discharge via local monad
    unwrap :: forall a . Wrapper(RemoteApplicative q) a -> StateT (RemoteApplicative q ()) m a
    unwrap (Value ap) = case superApplicative ap of
                            Nothing ->do
                                      discharge $ \ap' -> (ap' *> ap)
                            Just a  ->do
                                       modify (\ap' -> ap' <* ap)
                                       return a

    unwrap (Throw' ap) = do
                         discharge $ \ap' -> (ap' <* ap)
                         throwM RemoteEmptyException

    -- Either A or a Packet to return A
    pk :: RemoteApplicative q a -> X q a
    pk (T.Pure a)      = Pure' a
    pk (T.Query p) = Pkt id $ A.Query p
    pk (T.Ap g h)      = case (pk g, pk h) of
                           (Pure' a, Pure' b)   -> Pure' (a b)
                           (Pure' a, Pkt f b)   -> Pkt (\b' -> a (f b')) b
                           (Pkt f a, Pure' b)   -> Pkt (\a' -> f a' b) a
                           (Pkt f a, Pkt g b)   -> Pkt id $ A.Zip (\ a' b' -> f a' (g b')) a b
data X q a where
   Pure' :: a -> X q a
   Pkt  :: (a -> b) -> ApplicativePacket q a -> X q b
-}
