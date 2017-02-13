{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module:      Control.Remote.Haxl.Packet.Weak
Copyright:   (C) 2017, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Justin Dawson (jdawson@ku.edu)
Stability:   Alpha
Portability: GHC
-}

module Control.Remote.Haxl.Packet.Weak where

-- | A Weak Packet, that can encode a command or a procedure.

data WeakPacket (q :: * -> *) (a :: *) where
   Query :: q a -> WeakPacket q a

deriving instance (Show (q a)) => Show (WeakPacket q a)
