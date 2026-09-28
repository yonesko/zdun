{-# LANGUAGE ScopedTypeVariables #-}

module Probes
  ( isPortOpen,
    parseTarget,
    worker,
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
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

seconds :: Int -> Int
seconds n = n * 1000000

-- Разбирает строки вида "ya.ru:80" или "tcp://ya.ru:8080"
parseTarget :: String -> Maybe (HostName, ServiceName)
parseTarget raw =
  let withoutScheme =
        if "://" `isInfixOf` raw
          then drop 3 (dropWhile (/= ':') raw)
          else raw
   in case break (== ':') withoutScheme of
        (host, ':' : port) | not (null host) && not (null port) -> Just (host, port)
        _ -> Nothing

isPortOpen :: String -> IO Bool
isPortOpen target = case parseTarget target of
  Nothing -> do
    hPutStrLn stderr $ "[zdun] Invalid target format: " ++ target ++ " (expected host:port or tcp://host:port)"
    pure False
  Just (host, port) -> do
    res <- timeout (seconds 2) check
    case res of
      Nothing -> do
        hPutStrLn stderr $ "[zdun] Timeout connecting to " ++ host ++ ":" ++ port
        pure False
      Just ok -> pure ok
    where
      hints = defaultHints {addrSocketType = Stream}

      check = do
        result <- try $ do
          addrs <- getAddrInfo (Just hints) (Just host) (Just port)
          case addrs of
            [] -> do
              hPutStrLn stderr $ "[zdun] Host not found: " ++ host
              pure False
            (serverAddr : _) ->
              bracket
                (socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr))
                close
                (\sock -> connect sock (addrAddress serverAddr) >> pure True)

        case result of
          Left (err :: IOException) -> do
            hPutStrLn stderr $ "[zdun] " ++ host ++ ":" ++ port ++ " error: " ++ displayException err
            pure False
          Right ok -> pure ok

worker :: IO Bool -> Int -> IO ()
worker action timeoutSec
  | timeoutSec <= 0 = workerLoop action -- 0 или меньше = ждать бесконечно
  | otherwise = do
      res <- timeout (seconds timeoutSec) (workerLoop action)
      case res of
        Nothing -> do
          hPutStrLn stderr "[zdun] probe timeout exceeded"
          exitFailure
        Just () -> pure ()

workerLoop :: IO Bool -> IO ()
workerLoop action = do
  stop <- action
  if stop
    then pure ()
    else do
      threadDelay (seconds 1)
      workerLoop action