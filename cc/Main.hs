{-# Language OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import qualified Network.AMQP as MQ
import qualified Data.ByteString.Lazy.Char8 as BL

main :: IO ()
main = do
  conn <- MQ.openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- MQ.openChannel conn
  MQ.declareExchange chan MQ.newExchange { MQ.exchangeName = "SemaphoreExchange"
                                         , MQ.exchangeType = "direct" }
  (queue, _, _) <- MQ.declareQueue chan MQ.newQueue
                   { MQ.queueName = "semaphoreStatus" }
  MQ.bindQueue chan queue "SemaphoreExchange" "exchangeKey"
  forever $ do
    mMsg <- liftIO $ MQ.getMsg chan MQ.Ack queue
    case mMsg of
      Nothing  -> liftIO $ threadDelay (10 * 100000) -- if there are no messages throttle ~100 msec
      Just envelope -> do
        saveStatus chan envelope
        _ <- liftIO $ forkIO (MQ.ackEnv (snd envelope))
        return ()

saveStatus :: MQ.Channel -> (MQ.Message, MQ.Envelope) -> IO ()
saveStatus _ (msg, _)= do
  let res = BL.unpack $ MQ.msgBody msg
  print res




