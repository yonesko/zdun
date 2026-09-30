module Probes
  ( worker,
    CheckResult (..),
    seconds,
  )
where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (NominalDiffTime)
import System.Timeout (timeout)

data CheckResult = Ok | Err String

seconds :: Int -> Int
seconds n = n * 1000000

diffToMicroseconds :: NominalDiffTime -> Int
diffToMicroseconds d = round (d * 1000000)

worker :: (String -> IO ()) -> String -> IO CheckResult -> NominalDiffTime -> IO CheckResult
worker logMsg name action timeoutDiff
  | timeoutDiff <= 0 = workerLoop logMsg name action -- 0 или меньше = ждать бесконечно
  | otherwise = do
      res <- timeout (diffToMicroseconds timeoutDiff) $ workerLoop logMsg name action
      case res of
        Nothing -> do
          logMsg $ "[zdun] " <> name <> " timeout"
          pure $ Err "timeout"
        Just r -> pure r

workerLoop :: (String -> IO ()) -> String -> IO CheckResult -> IO CheckResult
workerLoop logMsg name action = do
  logMsg $ "[zdun] Running " <> name
  res <- action
  case res of
    Ok -> do
      logMsg $ "[zdun] " <> name <> " OK"
      pure Ok
    Err err -> do
      logMsg $ "[zdun] " <> name <> " error: " <> err
      threadDelay (seconds 1)
      workerLoop logMsg name action