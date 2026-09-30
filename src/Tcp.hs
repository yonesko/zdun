module Tcp
  ( isPortOpen,
    parseTarget,
    shortSocketError,
  )
where

import Control.Exception (IOException, bracket, displayException, try)
import Data.List (isInfixOf)
import GHC.IO.Exception (ioe_description)
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
import Probes (CheckResult (..), seconds)
import System.Timeout (timeout)

-- Разбирает строки вида "ya.ru:80" или "tcp://ya.ru:8080"
parseTarget :: String -> Maybe (HostName, ServiceName)
parseTarget raw =
  let withoutScheme =
        if "://" `isInfixOf` raw
          then drop 3 (dropWhile (/= ':') raw)
          else raw
   in case break (== ':') withoutScheme of
        (host, ':' : port) | not (null host), not (null port) -> Just (host, port)
        _ -> Nothing

-- | Extracts a concise, human-readable error description from an IOException.
shortSocketError :: IOException -> String
shortSocketError err =
  case ioe_description err of
    "" -> unwords . lines . displayException $ err
    desc -> desc

isPortOpen :: String -> IO CheckResult
isPortOpen target = case parseTarget target of
  Nothing -> pure $ Err $ "Invalid target format: " ++ target ++ " (expected host:port or tcp://host:port)"
  Just (host, port) -> do
    res <- timeout (seconds 2) check
    pure $ maybe (Err $ "Timeout connecting to " ++ host ++ ":" ++ port) id res
    where
      hints = defaultHints {addrSocketType = Stream}

      check = do
        result <- try $ do
          addrs <- getAddrInfo (Just hints) (Just host) (Just port)
          case addrs of
            [] -> pure $ Err $ "Host not found: " ++ host
            serverAddr : _ ->
              bracket
                (socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr))
                close
                (\sock -> connect sock (addrAddress serverAddr) >> pure Ok)
        pure $ either (Err . shortSocketError) id result