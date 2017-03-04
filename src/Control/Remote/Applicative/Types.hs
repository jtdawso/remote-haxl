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

module Control.Remote.Applicative.Types 
  ( 
   RemoteHaxlApplicative(..)
  ) where


import            Control.Natural
import            Control.Applicative
import            Data.Typeable
import            Control.Monad.Trans.Class


-- | 'RemoteHaxlApplicative' is our applicative that can be executed in a remote location.
data RemoteHaxlApplicative (q:: * -> *) a where 
   Query :: q a -> RemoteHaxlApplicative q a
   Ap        :: RemoteHaxlApplicative q (a -> b) -> RemoteHaxlApplicative q a -> RemoteHaxlApplicative q b
   Pure      :: a   -> RemoteHaxlApplicative q a  

instance Functor (RemoteHaxlApplicative q) where
  fmap f g = pure f <*> g

instance Applicative (RemoteHaxlApplicative q) where   -- may need m to be restricted to Monad here
  pure a = Pure a
  (<*>) = Ap
