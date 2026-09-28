{-# LANGUAGE ScopedTypeVariables #-}

module Probes
  ( worker,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, displayException, try)
import Data.List (isInfixOf)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType),
    HostName,
    ServiceName,
    SocketType (Stream),
    close,
    connect,
    defaultHints,
    getAddrInfo,
    socket,
  )
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

seconds :: Int -> Int
seconds n = n * 1000000

worker :: IO Bool -> Int -> IO Bool
worker action timeoutSec
  | timeoutSec <= 0 = workerLoop action -- 0 или меньше = ждать бесконечно
  | otherwise = do
      res <- timeout (seconds timeoutSec) (workerLoop action)
      case res of
        Nothing -> pure False
        Just ok -> pure ok

workerLoop :: IO Bool -> IO Bool
workerLoop action = do
  stop <- action
  if stop
    then pure True
    else do
      threadDelay (seconds 1)
      workerLoop action