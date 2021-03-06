{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module:      Control.Remote.Applicative
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Applicative
  ( -- * The remote applicative
    RemoteHaxlApplicative
    -- * The primitive lift functions
  , query
    -- * The run functions
  , RunHaxlApplicative(runHaxlApplicative)
  , runHaxlWeakApplicative
  , runHaxlApplicativeApplicative
  ) where


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Identity
import Control.Category ((>>>))

import           Control.Remote.Packet.Applicative as A
import qualified Control.Remote.Packet.Weak as Weak
import           Control.Remote.Packet.Weak (WeakPacket)
import           Control.Remote.Applicative.Types as T
import           Control.Natural
import           Control.Applicative


-- | promote a query into the applicative
query :: q a -> RemoteHaxlApplicative  q a
query q = T.Query q


-- | 'RunHaxlApplicative' is the overloading for choosing the appropriate bundling strategy for applicative.
class RunHaxlApplicative f where
  -- | This overloaded function chooses the appropriate bundling strategy
  --   based on the type of the handler your provide.
  runHaxlApplicative ::  (Applicative m) => (f q :~> m) -> (RemoteHaxlApplicative q:~> m)

instance RunHaxlApplicative WeakPacket where
  runHaxlApplicative = runHaxlWeakApplicative

instance RunHaxlApplicative ApplicativePacket where
  runHaxlApplicative = runHaxlApplicativeApplicative

-- | The weak remote applicative, that sends commands and procedures piecemeal.
runHaxlWeakApplicative :: forall m q . (Applicative m) =>(WeakPacket q :~> m) -> (RemoteHaxlApplicative q :~> m)
runHaxlWeakApplicative (NT rf) = wrapNT $ go
  where
    go :: forall a . RemoteHaxlApplicative q a ->  m a
    go (T.Query q)  = rf (Weak.Query q)
    go (T.Ap g h)   = go g <*> go h
    go (T.Pure   a) = pure a


-- | The applicative remote applicative, that is the identity function.
runHaxlApplicativeApplicative :: forall m q . (ApplicativePacket q :~> m) -> (RemoteHaxlApplicative q :~> m)
runHaxlApplicativeApplicative (NT rf) = wrapNT (rf . go3)
  where
    go3 :: forall a . RemoteHaxlApplicative q a -> ApplicativePacket q a
    go3 (T.Pure a)      = pure a
    go3 (T.Query q)     = (A.Query q)
    go3 (T.Ap g h)      = (go3 g) <*> (go3 h)
