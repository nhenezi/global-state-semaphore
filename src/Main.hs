{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Random
import Control.Concurrent
import Control.Monad
import Data.IORef
import System.Environment
import qualified Network.AMQP as MQ
import qualified Data.ByteString.Lazy.Char8 as BL

import Lib


-- How long to pause between traffic light changes
changePause :: Double
changePause = 3.0

-- value after which crossroad will consider changing state
trafficDensityLimit :: TrafficDensity
trafficDensityLimit = 1.5

-- simulates traffic sensor by generating random value
generateRandomDensity :: IO TrafficDensity
generateRandomDensity = getStdRandom (randomR (0, 2))


main :: IO ()
main = do
  crossroadLocation <- getEnv "CROSSROAD_LOCATION"
  cIO <- newIORef (Crossroad HPass crossroadLocation) -- keeps track of crossroad
  changingIO <- newIORef False -- is state of the crossroad changing
  changeToIO <- newIORef HPass -- if yes, to what does it have to change
  timeSinceLastChangeIO <- newIORef 0.0 -- when was last change initiated

  -- Establish Connection with the RabbitMQ
  conn <- MQ.openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- MQ.openChannel conn
  forever $ do
    timeSinceLastChange <- readIORef timeSinceLastChangeIO -- get value since last change
    modifyIORef timeSinceLastChangeIO (const (timeSinceLastChange + 1)) -- update time since last change
    changing <- readIORef changingIO -- get changing value
    c <- readIORef cIO -- get crossroad value
    let ss = if changing then Crossroad Changing (location c) else c
    MQ.publishMsg chan "SemaphoreExchange" "exchangeKey"
      MQ.newMsg { MQ.msgBody = BL.pack $ show ss, MQ.msgDeliveryMode = Just MQ.Persistent}
    -- if crossroad is changing and time since last change is sufficient, then update crossroad
    -- to a new state, reset timer and set changing to False
    when (changing && timeSinceLastChange > changePause) $ do
      putStr "Changing! "
      changeTo <- readIORef changeToIO
      modifyIORef cIO (const $ c { state = changeTo })
      modifyIORef timeSinceLastChangeIO (const 0.0)
      modifyIORef changingIO (const False)
    -- if crossroad is not changing state generate random densities and check if state
    -- has to be changed
    let currentState = state c
    unless changing $ do
      putStr "Not changing! "
      newD1 <- generateRandomDensity
      newD2 <- generateRandomDensity
      let newState = changeState timeSinceLastChange newD1 newD2 c
      putStrLn $ show timeSinceLastChange ++ " D1: " ++ show newD1 ++ " D2: " ++ show newD2 ++ " " ++ show currentState ++ " " ++ show newState
      -- if state has changed set that state of the crossroad is changing, save that value and
      -- reset the timer
      unless (newState == currentState) $ do
        print $ "Changing state to " ++ show newState
        modifyIORef changingIO (const True)
        modifyIORef changeToIO (const newState)
        modifyIORef timeSinceLastChangeIO (const 0.0)
    -- send state information to SemaphoreExchange exchange on RabbitMQ
    threadDelay 1000000 -- throttle everything so above happens every ~1 sec

