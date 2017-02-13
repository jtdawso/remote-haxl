{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Natural
import Control.Remote.Haxl
import Control.Remote.Haxl.Packet.Weak as WP
import Control.Remote.Haxl.Packet.Applicative as AP
import Control.Applicative

data Procedure :: * -> * where
   Say :: String -> Procedure ()
   Temperature :: Procedure Int

say :: String -> RemoteHaxlMonad Procedure ()
say s = query (Say s)

temperature :: RemoteHaxlMonad Procedure Int
temperature = query Temperature


--Server Side Functions
---------------------------------------------------------
runWP ::  WeakPacket Procedure a -> IO a
runWP (WP.Query Temperature) = do
                                 putStrLn "Temp Call"
                                 return 42
runWP (WP.Query (Say s))     = print s
---------------------------------------------------------

runAP :: ApplicativePacket Procedure a -> IO a
runAP (AP.Query (Say s)) = print s
runAP (AP.Query Temperature) =do
                                 putStrLn "Temp Call"
                                 return 42
runAP (AP.Zip f g h) = do
             f <$> runAP g <*> runAP h
runAP (AP.Pure a) = do
                       putStrLn "Pure"
                       return a
---------------------------------------------------------

sendWeak :: RemoteHaxlMonad Procedure a -> IO a
sendWeak = unwrapNT $ runHaxlMonad $ wrapNT (\pkt -> do putStrLn "-----"; runWP pkt)

--sendApp :: RemoteHaxlMonad Procedure a -> IO a
--sendApp = unwrapNT $ runHaxlMonad $ wrapNT (\pkt -> do putStrLn "-----"; runAP pkt)

---------------------------------------------------------

main :: IO ()
main = do

        putStrLn "WeakSend\n"
        runTest $ wrapNT sendWeak

--        putStrLn "\nAppSend\n"
--        runTest $ wrapNT sendApp

--Run Test Suite
runTest :: (RemoteHaxlMonad Procedure :~> IO)-> IO()
runTest (NT f) = do
               f test
               f testBind
               f testApp



-- Original test case
test :: RemoteHaxlMonad Procedure ()
test = do
         say "Howdy doodly do"
         say "How about a muffin?"
         t <- temperature
         say (show t ++ "F")

-- Test bind
testBind :: RemoteHaxlMonad Procedure ()
testBind = say "one" >> say "two" >> temperature >>= say . ("Temperature: " ++) .show


testApp :: RemoteHaxlMonad Procedure ()
testApp = do
          r<- add <$> temperature<*>temperature <*> temperature
          say (show r)
     where
            add :: Int -> Int -> Int -> Int
            add x y z= x + y + z

