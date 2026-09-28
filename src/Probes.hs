module Probes
  (isPortOpen
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link)
import Control.Exception (IOException, bracket, try)
import Control.Monad (forever)
import Network.Socket
import System.Timeout (timeout)

data TcpProbe = TcpProbe String Int

seconds, minutes, milliseconds :: Int -> Int
seconds      n = n * 1_000_000
minutes      n = n * seconds 60
milliseconds n = n * 1_000

isPortOpen :: HostName -> ServiceName -> Int -> IO Bool
isPortOpen host port timeoutUs = do
  let hints = defaultHints {addrSocketType = Stream}
  addrs <- getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> pure False
    (serverAddr : _) -> do
      result <- timeout timeoutUs $ do
        bracket
          (socket (addrFamily serverAddr) (addrSocketType serverAddr) (addrProtocol serverAddr))
          close
          ( \sock -> do
              res <- try (connect sock (addrAddress serverAddr)) :: IO (Either IOException ())
              pure (either (const False) (const True) res)
          )
      pure (result == Just True)

worker :: IO Bool -> IO ()
worker action = do
    stop <- action
    if stop then
        pure ()
    else $ do
        threadDelay (seconds 5)
        worker action

    --   -- Запускает worker в отдельном потоке внутри скоупа
    -- withAsync worker $ \_asyncHandle -> do
    --     putStrLn "Основной поток работает параллельно..."
    --     -- Основная логика программы
    --     threadDelay 16000000
    --     putStrLn "Завершение программы (фоновый тред автоматически отменится)"
