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
    -- * The primitive lift function
  , query
    -- * The run functions
  , RunHaxlMonad(runHaxlMonad)
  , runWeakMonad
  , runApplicativeMonad
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import qualified Control.Remote.Haxl.Applicative as A
import           Control.Remote.Haxl.Packet.Applicative as A
import           Control.Remote.Haxl.Packet.Weak as Weak
import           Control.Remote.Haxl.Types as T
import           Control.Applicative

import Control.Monad
import Control.Natural
import Debug.Trace

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

instance RunHaxlMonad ApplicativePacket where
  runHaxlMonad = runApplicativeMonad

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


-- | The is the strong applicative strong remote monad. It bundles
--   packets (of type 'RemoteHaxlApplicative') as large as possible,
--   including over some monadic binds.
runApplicativeMonad :: forall m q . (Monad m) => (A.ApplicativePacket q :~> m) -> (RemoteHaxlMonad q :~> m)
runApplicativeMonad (NT rf) = wrapNT $ \ p -> do
    (r,h) <- runStateT (go2 (helper p)) (pure ())
    rf $ pk $ h -- should we stub out the call with only 'Pure'?
    return r
  where
    
    go2 :: forall a . RemoteHaxlMonad q a -> StateT (T.RemoteHaxlApplicative q ()) m a
    go2 (Appl app)   = go app
    go2 (Bind app k) = go2 app >>= \ a -> go2 (k a)
    go2 (Ap' x y)  = go2 x <*> go2 y
    go :: forall a .  T.RemoteHaxlApplicative q a -> StateT (T.RemoteHaxlApplicative q ()) m a
    go ap = case superApplicative ap of
                Nothing -> do
                  ap' <- get
                  put (pure ())
                  lift $ rf $ (pk (ap' *> ap))
                Just a -> do
                  modify (\ ap' -> ap' <* ap)
                  return a

    superApplicative :: T.RemoteHaxlApplicative q a -> Maybe a
    superApplicative (T.Pure a)      = pure a
    superApplicative (T.Query p) = Nothing
    superApplicative (T.Ap g h)      = superApplicative g <*> superApplicative h


    -- It all comes down to this. Converting quickly between T.RemoteHaxlApplicative and ApplicativePacket.
    
    pk :: T.RemoteHaxlApplicative q a -> ApplicativePacket q a
    pk (T.Pure a)      = A.Pure a
    pk (T.Query q)     = A.Query  q
    pk (T.Ap g h)      = A.Zip ($) (pk g) (pk h)

    helper:: RemoteHaxlMonad q a -> RemoteHaxlMonad q a
    helper (Ap' x@(Ap' _ _) y@(Ap' _ _))    = trace "1" $ helper x <*> helper y
    helper (Ap' (Bind m1 k1) (Bind m2 k2) ) = trace "2" $ liftA2 (,)  (helper m1) (helper m2) >>= \(x1,x2) ->helper ( k1 x1) <*> helper (k2 x2) 
    helper (Ap' (Bind m1 k1) app)           = trace "3" $ liftA2 (,) (helper m1) (helper app) >>= \(x1,x2) -> helper (k1 x1) <*> (pure x2)
    helper (Ap' (Ap' app (Bind m1 k1))   (Bind m2 k2))         = trace "4" $ liftA3 (,,) (helper app) (helper m1) (helper  m2) >>= \(x1,x2,x3) -> (pure x1 <*> k1 x2) <*> helper (k2 x3)
    helper (Bind m k) = trace "5" $ (helper m) >>= \ x -> helper (k x)
    helper x = x   
