{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Haxl.Types
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson (jdawson@ku.edu)
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Haxl.Types 
  ( RemoteHaxlMonad(..)
  , RemoteHaxlMonad
  ) where


import            Control.Natural
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class
import            Control.Remote.Applicative.Types

-- | 'RemoteHaxlMonad' is our monad that can be executed in a remote location.
data RemoteHaxlMonad  (q :: * -> *) a where
   Appl        :: RemoteHaxlApplicative q a -> RemoteHaxlMonad q a
   Bind        :: RemoteHaxlMonad q a -> (a -> RemoteHaxlMonad q b) -> RemoteHaxlMonad q b
   Ap'         :: RemoteHaxlMonad q (a -> b) -> RemoteHaxlMonad q a -> RemoteHaxlMonad q b

instance  Functor (RemoteHaxlMonad q) where
  fmap f m = pure f <*> m

instance  Applicative (RemoteHaxlMonad q) where
  pure a                = Appl (pure a)
  Appl f   <*> Appl g   = Appl (f <*> g)
  f        <*> g        = Ap' f g

instance Monad (RemoteHaxlMonad q) where
  return      = pure
  m >>= k     = Bind m k
  m1 >> m2    = m1 *> m2 -- This improves our bundling opportunities
