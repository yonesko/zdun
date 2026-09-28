{-# LANGUAGE ScopedTypeVariables #-}

module Probes
  ( isPortOpen,
    worker,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forever)
import Network.Socket
import System.Exit (exitFailure)
import System.Timeout (timeout)

data TcpProbe = TcpProbe String Int

seconds, minutes, milliseconds :: Int -> Int
seconds n = n * 1000000
minutes n = n * seconds 60
milliseconds n = n * 1000

isPortOpen :: HostName -> ServiceName -> Int -> IO Bool
isPortOpen host port timeoutUs = do
  -- Оборачиваем ВСЮ операцию целиком в таймаут:
  res <- timeout timeoutUs check
  pure (res == Just True)
  where
    hints = defaultHints {addrSocketType = Stream}

    check = do
      -- Перехватываем ошибки и DNS-резолвинга, и соединения
      result <- try $ do
        addrs <- getAddrInfo (Just hints) (Just host) (Just port)
        case addrs of
          [] -> pure False
          (serverAddr : _) ->
            -- Вот здесь сокет ОБЯЗАТЕЛЬНО в bracket, чтобы не утекал дескриптор:
            bracket
              (socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr))
              close
              (\sock -> connect sock (addrAddress serverAddr) >> pure True)

      case result of
        Left (_ :: IOException) -> pure False -- DNS не отрезолвился или порт закрыт
        Right ok -> pure ok

worker :: IO Bool -> Int -> IO ()
worker action timeoutSec
  | timeoutSec <= 0 = workerLoop action -- 0 или меньше = ждать бесконечно
  | otherwise = do
      res <- timeout (seconds timeoutSec) (workerLoop action)
      case res of
        Nothing -> do
          putStrLn "zdun: probe timeout exceeded!"
          exitFailure
        Just () -> pure ()

workerLoop :: IO Bool -> IO ()
workerLoop action = do
  stop <- action
  if stop
    then pure ()
    else do
      threadDelay (seconds 5)
      workerLoop action