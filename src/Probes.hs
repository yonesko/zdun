module Probes
  ( runLoop,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Bitraversable (bitraverse)
import Data.Time.Clock
  ( NominalDiffTime,
    addUTCTime,
    getCurrentTime,
  )
import Http (checkHttp)
import Tcp (checkTcp)
import Types

runSingle :: Env -> Probe -> IO (Either String ())
runSingle _ (Probe (Tcp host port) check) = checkTcp host port check
runSingle env (Probe (Http url) check) = checkHttp env url check

runLoop :: Env -> NominalDiffTime -> Probe -> IO (Either String ())
runLoop env t p = do
  deadline <- if t <= 0 then pure Nothing else Just . addUTCTime t <$> getCurrentTime
  let loop = withLogs env (formatProbe p) (runSingle env p) >>= either onFail (pure . Right)
      onFail :: String -> IO (Either String ())
      onFail err = do
        now <- getCurrentTime
        if maybe False (now >=) deadline
          then
            pure $ Left $ "done trying, last error: " <> err
          else threadDelay (seconds 1) *> loop
  loop

withLogs :: Env -> String -> IO (Either String ()) -> IO (Either String ())
withLogs (Env _ logger) probeName action =
  logger ("Calling probe " <> probeName)
    *> (action >>= bitraverse onFail onSuccess)
  where
    onFail err = err <$ logger ("Failed probe " <> probeName <> ": " <> err)
    onSuccess () = void (logger ("Succeeded probe " <> probeName))

seconds :: Int -> Int
seconds n = n * 1000000
