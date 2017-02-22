{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Natural
import           Control.Remote.Haxl             as M
import           Control.Remote.Haxl.Applicative as A
import           Control.Remote.Haxl.Packet.Weak as WP
import           Control.Remote.Haxl.Packet.Applicative as AP


data Procedure :: * -> * where
   Say          :: String -> Procedure ()
   Temperature  :: Procedure Int
   Temperature2 :: Procedure Int 

sayA :: String -> RemoteHaxlApplicative Procedure ()
sayA  s = A.query (Say s)

temperatureA :: RemoteHaxlApplicative Procedure Int
temperatureA  = A.query Temperature

temperature2A :: RemoteHaxlApplicative Procedure Int
temperature2A  = A.query Temperature2

sayM :: String -> RemoteHaxlMonad Procedure ()
sayM s = M.query (Say s)

temperatureM :: RemoteHaxlMonad Procedure Int
temperatureM = M.query Temperature

temperature2M :: RemoteHaxlMonad Procedure Int
temperature2M = M.query Temperature2


--Server Side Functions
---------------------------------------------------------

runWP ::  WeakPacket Procedure a -> IO a
runWP (WP.Query Temperature) = do
                                 putStrLn "Temp Call"
                                 return 42

runWP (WP.Query Temperature2) = do
                                 putStrLn "Temp2 Call"
                                 return 52
                                 
runWP (WP.Query (Say s))     = print s
---------------------------------------------------------

runAP :: ApplicativePacket Procedure a -> IO a
runAP (AP.Query (Say s)) = print s
runAP (AP.Query Temperature) =do
                                 putStrLn "Temp Call"
                                 return 42
runAP (AP.Query Temperature2) = do
                                 putStrLn "Temp2 Call"
                                 return 52

runAP (AP.Zip f g h) = do
             f <$> runAP g <*> runAP h
runAP (AP.Pure a) = do
                       putStrLn "Pure"
                       return a
---------------------------------------------------------

sendWeakM :: RemoteHaxlMonad Procedure a -> IO a
sendWeakM = unwrapNT $ runHaxlMonad $ wrapNT (\pkt -> do putStrLn "-----"; runWP pkt)

sendAppM :: RemoteHaxlMonad Procedure a -> IO a
sendAppM = unwrapNT $ runHaxlMonad $ wrapNT (\pkt -> do putStrLn "-----"; runAP pkt)


sendWeakA :: RemoteHaxlApplicative Procedure a -> IO a
sendWeakA = unwrapNT $ runHaxlApplicative $ wrapNT (\pkt -> do putStrLn "-----"; runWP pkt)

sendAppA :: RemoteHaxlApplicative Procedure a -> IO a
sendAppA = unwrapNT $ runHaxlApplicative $ wrapNT (\pkt -> do putStrLn "-----"; runAP pkt)

---------------------------------------------------------

main :: IO ()
main = do

        putStrLn "WeakSendM\n"
        runTestM $ wrapNT sendWeakM

        putStrLn "\nAppSendM\n"
        runTestM $ wrapNT sendAppM

        putStrLn "WeakSendA\n"
        runTestA $ wrapNT sendWeakA

        putStrLn "\nAppSendA\n"
        runTestA $ wrapNT sendAppA

--Run Test Suite
runTestM :: (RemoteHaxlMonad Procedure :~> IO)-> IO ()
runTestM (NT f) = do
               f testM
               f testBind
               f testAppM
               r <- f haxlExample
               print r

runTestA :: (RemoteHaxlApplicative Procedure :~> IO) -> IO ()
runTestA (NT f) = do
               f testA
               t <- f testAppA
               print t
-- Original test case
testM :: RemoteHaxlMonad Procedure ()
testM = do
         sayM "Howdy doodly do"
         sayM "How about a muffin?"
         t <- temperatureM
         sayM (show t ++ "F")

-- Test bind
testBind :: RemoteHaxlMonad Procedure ()
testBind = sayM "one" >> sayM "two" >> temperatureM >>= sayM . ("Temperature: " ++) .show


testAppM :: RemoteHaxlMonad Procedure ()
testAppM = do
          r<- add <$> temperatureM<*>temperatureM <*> temperatureM
          sayM (show r)
     where
            add :: Int -> Int -> Int -> Int
            add x y z= x + y + z


testA :: RemoteHaxlApplicative Procedure ()
testA =     sayA "Howdy doodly do" *> sayA "How about a muffin?"
         
testAppA :: RemoteHaxlApplicative Procedure Int
testAppA = (add <$> temperatureA <*> temperatureA <*> temperatureA)
    where
            add :: Int -> Int -> Int -> Int
            add x y z = x + y + z


haxlExample :: RemoteHaxlMonad Procedure (Int,Int)
haxlExample = do
               (,) <$>  (temperature2M >>= f) <*> (temperatureM >>= f)

    where
         f a = if a > 50 then
                    return 1
                 else
                    return 0
        
