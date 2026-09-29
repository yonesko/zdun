{-# LANGUAGE ScopedTypeVariables #-}

module Probes
  ( worker,
  )
where

import Control.Concurrent (threadDelay)
import System.Timeout (timeout)

seconds :: Int -> Int
seconds n = n * 1000000

worker :: (String -> IO ()) -> String -> IO Bool -> Int -> IO Bool
worker logMsg name action timeoutSec
  | timeoutSec <= 0 = workerLoop logMsg name action -- 0 или меньше = ждать бесконечно
  | otherwise = do
      res <- timeout (seconds timeoutSec) (workerLoop logMsg name action)
      case res of
        Nothing -> pure False
        Just ok -> pure ok

workerLoop :: (String -> IO ()) -> String -> IO Bool -> IO Bool
workerLoop logMsg name action = do
  logMsg $ "[zdun] Running " <> name
  stop <- action
  if stop
    then pure True
    else do
      threadDelay (seconds 1)
      workerLoop logMsg name action