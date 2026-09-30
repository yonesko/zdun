module Tcp
  ( isPortOpen,
    parseTarget,
  )
where

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
import Probes (CheckResult (..))
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

isPortOpen :: String -> IO CheckResult
isPortOpen target = case parseTarget target of
  Nothing -> pure $ Err ("Invalid target format: " ++ target ++ " (expected host:port or tcp://host:port)")
  Just (host, port) -> do
    res <- timeout (seconds 2) check
    pure $ case res of
      Nothing -> Err ("Timeout connecting to " ++ host ++ ":" ++ port)
      Just r -> r
    where
      hints = defaultHints {addrSocketType = Stream}

      check = do
        result <- try $ do
          addrs <- getAddrInfo (Just hints) (Just host) (Just port)
          case addrs of
            [] -> pure (Left ("Host not found: " ++ host))
            (serverAddr : _) ->
              bracket
                (socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr))
                close
                (\sock -> connect sock (addrAddress serverAddr) >> pure (Right ()))

        case result of
          Left (err :: IOException) -> pure (Err (unwords (lines (displayException err))))
          Right _ -> pure Ok