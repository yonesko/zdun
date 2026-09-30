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

worker :: (String -> IO ()) -> String -> IO (Either String ()) -> NominalDiffTime -> IO (Either String ())
worker logMsg name action timeoutDiff
  | timeoutDiff <= 0 = workerLoop logMsg name action -- 0 или меньше = ждать бесконечно
  | otherwise = do
      res <- timeout (diffToMicroseconds timeoutDiff) (workerLoop logMsg name action)
      case res of
        Nothing -> do
          logMsg $ "[zdun] " <> name <> " timeout"
          pure (Left "timeout")
        Just r -> pure r

workerLoop :: (String -> IO ()) -> String -> IO (Either String ()) -> IO (Either String ())
workerLoop logMsg name action = do
  logMsg $ "[zdun] Running " <> name
  res <- action
  case res of
    Right () -> do
      logMsg $ "[zdun] " <> name <> " OK"
      pure (Right ())
    Left err -> do
      logMsg $ "[zdun] " <> name <> " error: " <> err
      threadDelay (seconds 1)
      workerLoop logMsg name action