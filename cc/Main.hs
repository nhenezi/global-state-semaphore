{-# Language OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.State
import Data.IORef
import qualified Network.AMQP as MQ
import qualified Data.ByteString.Lazy.Char8 as BL

import Lib

main :: IO ()
main = do
  conn <- MQ.openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- MQ.openChannel conn
  MQ.declareExchange chan MQ.newExchange { MQ.exchangeName = "SemaphoreExchange"
                                         , MQ.exchangeType = "direct" }
  (queue, _, _) <- MQ.declareQueue chan MQ.newQueue
                   { MQ.queueName = "semaphoreStatus" }
  MQ.bindQueue chan queue "SemaphoreExchange" "exchangeKey"
  stateIO <- newIORef ([] :: [Crossroad]) -- global state of the system
  forever $ do
    s <- readIORef stateIO
    mMsg <- liftIO $ MQ.getMsg chan MQ.Ack queue
    case mMsg of
      Nothing  -> liftIO $ threadDelay (10 * 100000) -- if there are no messages throttle ~100 msec
      Just envelope -> do
        let newS = execState (updateStatus chan envelope) s
        putStrLn $ pprint (GlobalState newS)
        modifyIORef stateIO (const newS)
        liftIO $ MQ.ackEnv (snd envelope)
        return ()

updateStatus :: MQ.Channel -> (MQ.Message, MQ.Envelope) -> State [Crossroad] Int
updateStatus _ (msg, _)= do
    updateCrossroad crossroad
    return 1
  where crossroad = read $ BL.unpack $ MQ.msgBody msg :: Crossroad
