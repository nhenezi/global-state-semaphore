{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies#-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Monad
import qualified Control.Monad.State as S
import Data.Functor.Identity

type TrafficDensity = Double
-- Crossroad can either allow horizontal pass, vertical pass or neither. If it's neither,
-- then we mark that state as changing (e.g. it's changing from vertical to horizontal pass)
type Location = String
data CrossroadState = HPass | VPass | Changing deriving (Eq, Show, Read)
data Crossroad = Crossroad { state ::CrossroadState, location :: Location} deriving (Eq, Show, Read)

-- how long does a crossroad have to be in a state before changing it
minimumCrossroadState :: Double
minimumCrossroadState = 5.0

-- Negates crossroad state
negateCState :: CrossroadState -> CrossroadState
negateCState Changing = Changing
negateCState HPass = VPass
negateCState VPass = HPass

-- determines when a state of a crossroad has to be changed
changeState :: Double -> TrafficDensity -> TrafficDensity -> Crossroad -> CrossroadState
changeState n nd1 nd2 (Crossroad {..})
  | n < minimumCrossroadState = state
  | nd1 >= nd2 = negateCState state -- @todo fix this
  | otherwise = state

-- Global state of the system
data GlobalState = GlobalState [Crossroad] deriving (Eq, Show)


-- we have to use custom `show` function because we used default implementations
-- for parsing strings from rabbitMQ messages to Haskell types
compactShow :: Crossroad -> String
compactShow c = location c ++ ": " ++ show (state c)

-- Prettifies output for stdout
pprint :: GlobalState -> String
pprint (GlobalState [] ) = ""
pprint (GlobalState (s:as)) = compactShow s ++ "\n" ++ pprint (GlobalState as)

-- Adds a crossroads to State
addCrossroad :: Crossroad -> S.State [Crossroad] Int
addCrossroad c = do
  s <- S.get
  S.put $ c : s
  return 1

-- checks if crossroad is in list of crossroads
inState :: Crossroad -> [Crossroad] -> Bool
inState c s = any ((== (location c)).location) s

-- replaces a crossroad
-- if a crossroad on that location doesn't exists, it does nothing.
replaceCrossroad :: Crossroad -> S.State [Crossroad] Int
replaceCrossroad c = do
  s <- S.get
  S.put $ c : (filter ((/= (location c)).location ) s)
  return 1

-- Updates Crossroad from State
-- if crossroad already exists it replaces it
-- otherwise it just adds a new crossroad
updateCrossroad :: Crossroad -> S.State [Crossroad] Int
updateCrossroad c = do
  s <- S.get
  if inState c s then replaceCrossroad c else addCrossroad c

-- Removes Crossroad from State
removeCrossroad :: Crossroad -> S.State [Crossroad] Int
removeCrossroad c = do
  s <- S.get
  S.put $ filter ((/= (location c)).location) s
  return 1
