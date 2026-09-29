{-# LANGUAGE ScopedTypeVariables #-}

module Probes
  ( worker,
  )
where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (NominalDiffTime)
import System.Timeout (timeout)

seconds :: Int -> Int
seconds n = n * 1000000

diffToMicroseconds :: NominalDiffTime -> Int
diffToMicroseconds d = round (d * 1000000)

worker :: (String -> IO ()) -> String -> IO Bool -> NominalDiffTime -> IO Bool
worker logMsg name action timeoutDiff
  | timeoutDiff <= 0 = workerLoop logMsg name action -- 0 или меньше = ждать бесконечно
  | otherwise = do
      res <- timeout (diffToMicroseconds timeoutDiff) (workerLoop logMsg name action)
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