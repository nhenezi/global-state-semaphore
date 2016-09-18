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

data GlobalState = GlobalState [Crossroad] deriving (Eq, Show)

compactShow :: Crossroad -> String
compactShow c = location c ++ ": " ++ show (state c)

pprint :: GlobalState -> String
pprint (GlobalState [] ) = ""
pprint (GlobalState (s:as)) = compactShow s ++ "\n" ++ pprint (GlobalState as)

addCrossroad :: Crossroad -> S.State [Crossroad] Int
addCrossroad c = do
  s <- S.get
  S.put $ c : s
  return 1

inState :: Crossroad -> [Crossroad] -> Bool
inState c s = any ((== (location c)).location) s

replaceCrossroad :: Crossroad -> S.State [Crossroad] Int
replaceCrossroad c = do
  s <- S.get
  S.put $ c : (filter ((/= (location c)).location ) s)
  return 1

updateCrossroad :: Crossroad -> S.State [Crossroad] Int
updateCrossroad c = do
  s <- S.get
  if inState c s then replaceCrossroad c else addCrossroad c

removeCrossroad :: Crossroad -> S.State [Crossroad] Int
removeCrossroad c = do
  s <- S.get
  S.put $ filter ((/= (location c)).location) s
  return 1
