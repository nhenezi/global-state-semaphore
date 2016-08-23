module Main where

import System.Random
import Control.Concurrent
import Control.Monad
import Data.IORef

type TrafficDensity = Double
-- Crossroad can either allow horizontal pass, vertical pass or neither. If it's neither,
-- then we mark that state as changing (e.g. it's changing from vertical to horizontal pass)
data CrossroadState = HPass | VPass | Changing deriving (Eq, Show)
data Crossroad = Crossroad CrossroadState deriving (Eq, Show)

-- How long to pause between traffic light changesj
changePause :: Double
changePause = 3.0

-- how long does a crossroad have to be in a state before changing it
minimumCrossroadState :: Double
minimumCrossroadState = 5.0


-- value after which crossroad will consider changing state
trafficDensityLimit :: TrafficDensity
trafficDensityLimit = 1.5

-- default crossroad state
initializeCrossroad :: Crossroad
initializeCrossroad = Crossroad HPass

-- simulates traffic sensor by generating random value
generateRandomDensity :: IO TrafficDensity
generateRandomDensity = getStdRandom (randomR (0, 2))

-- hackish way to retrieve crossroad state
getCrossroadState :: Crossroad -> CrossroadState
getCrossroadState (Crossroad HPass) = HPass
getCrossroadState (Crossroad VPass) = VPass
getCrossroadState (Crossroad Changing) = Changing

-- determines when a state of a crossroad has to be changed
changeState :: Double -> TrafficDensity -> TrafficDensity -> Crossroad -> CrossroadState
changeState _ _ _ (Crossroad Changing) = Changing
changeState n nd1 nd2 (Crossroad HPass)
  | n < minimumCrossroadState = HPass
  | nd1 < nd2 = VPass
  | otherwise = HPass
changeState n nd1 nd2 (Crossroad VPass)
  | n < minimumCrossroadState = VPass
  | nd1 >= nd2 = HPass
  | otherwise = VPass

main :: IO ()
main = do
  cIO <- newIORef initializeCrossroad -- keeps track of crossroad
  changingIO <- newIORef False -- is state of the crossroad changing
  changeToIO <- newIORef HPass -- if yes, to what does it have to change
  timeSinceLastChangeIO <- newIORef 0.0 -- when was last change initiated
  forever $ do
    c <- readIORef cIO -- get crossroad value
    timeSinceLastChange <- readIORef timeSinceLastChangeIO -- get value since last change
    modifyIORef timeSinceLastChangeIO (const (timeSinceLastChange + 1)) -- update time since last change
    changing <- readIORef changingIO -- get changing value
    -- if crossroad is changing and time since last change is sufficient, then update crossroad
    -- to a new state, reset timer and set changing to False
    when (changing && timeSinceLastChange > changePause) $ do
      putStr "Changing! "
      changeTo <- readIORef changeToIO
      modifyIORef cIO (const $ Crossroad changeTo)
      modifyIORef timeSinceLastChangeIO (const 0.0)
      modifyIORef changingIO (const False)
    -- if crossroad is not changing state generate random densities and check if state
    -- has to be changed
    unless changing $ do
      putStr "Not changing! "
      newD1 <- generateRandomDensity
      newD2 <- generateRandomDensity
      let currentState = getCrossroadState c
      let newState = changeState timeSinceLastChange newD1 newD2 c
      putStrLn $ show timeSinceLastChange ++ " D1: " ++ show newD1 ++ " D2: " ++ show newD2 ++ " " ++ show currentState ++ " " ++ show newState
      -- if state has changed set that state of the crossroad is changing, save that value and
      -- reset the timer
      unless (newState == currentState) $ do
        print $ "Changing state to " ++ show newState
        modifyIORef changingIO (const True)
        modifyIORef changeToIO (const newState)
        modifyIORef timeSinceLastChangeIO (const 0.0)
    threadDelay 1000000 -- throttle everything so above happens every ~1 sec

