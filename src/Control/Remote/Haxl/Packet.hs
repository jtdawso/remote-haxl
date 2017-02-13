{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module:      Control.Remote.Monad.Packet
Copyright:   (C) 2016, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Haxl.Packet
  (
    Promote(..)
  , promoteToApplicative
  ) where
import qualified Control.Remote.Haxl.Packet.Weak as Weak
import qualified Control.Remote.Haxl.Packet.Applicative as A
import           Control.Natural
import           Control.Applicative

class Promote f where
    promote :: (Applicative m) => (Weak.WeakPacket q :~> m) -> (f q :~> m)

instance Promote A.ApplicativePacket where
   promote f =  promoteToApplicative f


-- | promotes a function that can work over WeakPackets to a function that can work over Applicative Packets
promoteToApplicative :: forall q m . (Applicative m) => (Weak.WeakPacket q :~> m) -> (A.ApplicativePacket q :~> m)
promoteToApplicative (NT f) =  NT $ applicativeFunc
                    where
                        applicativeFunc :: (Applicative m) => (A.ApplicativePacket q a -> m a)
                        applicativeFunc (A.Query q) = f (Weak.Query q)
                        applicativeFunc (A.Zip f1 a b) =  f1 <$> applicativeFunc a <*> applicativeFunc b
                        applicativeFunc (A.Pure a) = pure a


