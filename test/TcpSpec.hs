module TcpSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (forM_, forever, replicateM, void)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Network.Socket
  ( Family (AF_INET),
    ServiceName,
    SockAddr (SockAddrInet),
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    connect,
    defaultProtocol,
    listen,
    setSocketOption,
    socket,
    socketPort,
    tupleToHostAddress,
  )
import Network.Socket.ByteString (sendAll)
import Tcp (checkTcp)
import Test.Hspec
  ( Spec,
    describe,
    expectationFailure,
    it,
    shouldBe,
    shouldStartWith,
  )
import Types (Check (..))

spec :: Spec
spec = do
  describe "checkTcp" $ do
    describe "success" $ do
      it "connects to open TCP port" $ do
        withOpenPort $ \port -> do
          res <- checkTcp "127.0.0.1" port Nothing
          res `shouldBe` Right ()

    describe "timeout" $ do
      it "times out after 2s connecting to unresponsive port" $ do
        withUnresponsivePort $ \port -> do
          res <- checkTcp "127.0.0.1" port Nothing
          res `shouldBe` Left ("Timeout(2s) connecting to 127.0.0.1:" <> port)

    describe "failure" $ do
      it "fails on invalid service / port name" $ do
        res <- checkTcp "127.0.0.1" "invalid-port" Nothing
        case res of
          Left err -> err `shouldStartWith` "127.0.0.1:invalid-port: "
          Right () -> expectationFailure "expected connection to fail, but succeeded"

      it "fails on invalid service / port name with IPv6 host" $ do
        res <- checkTcp "::1" "invalid-port" Nothing
        case res of
          Left err -> err `shouldStartWith` "[::1]:invalid-port: "
          Right () -> expectationFailure "expected connection to fail, but succeeded"

      it "fails on non-existent host" $ do
        res <- checkTcp "nonexistent.example.invalid" "80" Nothing
        case res of
          Left err -> err `shouldStartWith` "nonexistent.example.invalid:80: "
          Right () -> expectationFailure "expected connection to fail, but succeeded"

    describe "content check (contains)" $ do
      it "succeeds when banner contains expected substring" $ do
        withBannerServer (TE.encodeUtf8 "220 mail.example.com ESMTP Postfix\r\n") $ \port -> do
          res <- checkTcp "127.0.0.1" port (Just (Contains "220"))
          res `shouldBe` Right ()

      it "succeeds when banner contains UTF-8 substring" $ do
        withBannerServer (TE.encodeUtf8 "Привет мир\r\n") $ \port -> do
          res <- checkTcp "127.0.0.1" port (Just (Contains "Привет"))
          res `shouldBe` Right ()

      it "fails when banner doesn't contain expected substring" $ do
        withBannerServer (TE.encodeUtf8 "220 mail.example.com ESMTP Postfix\r\n") $ \port -> do
          res <- checkTcp "127.0.0.1" port (Just (Contains "PONG"))
          res `shouldBe` Left "response body doesn't contain substring"

    describe "content check (matches)" $ do
      it "succeeds when banner matches regex" $ do
        withBannerServer (TE.encodeUtf8 "220 mail.example.com ESMTP Postfix\r\n") $ \port -> do
          res <- checkTcp "127.0.0.1" port (Just (Matches "^220.*ESMTP"))
          res `shouldBe` Right ()

      it "fails when banner doesn't match regex" $ do
        withBannerServer (TE.encodeUtf8 "220 mail.example.com ESMTP Postfix\r\n") $ \port -> do
          res <- checkTcp "127.0.0.1" port (Just (Matches "^[0-9]{3} PONG"))
          res `shouldBe` Left "response body doesn't match re"

withBannerServer :: BS.ByteString -> (ServiceName -> IO a) -> IO a
withBannerServer banner action =
  bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    listen sock 5
    port <- socketPort sock
    tid <- forkIO $ forever $ do
      (conn, _) <- accept sock
      void (sendAll conn banner) `finally` close conn
    action (show port) `finally` killThread tid

withOpenPort :: (ServiceName -> IO a) -> IO a
withOpenPort action = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 5
  port <- socketPort sock
  action (show port) `finally` close sock

withUnresponsivePort :: (ServiceName -> IO a) -> IO a
withUnresponsivePort action = bracket setup cleanup (\(_, _, port) -> action (show port))
  where
    setup = do
      sock <- socket AF_INET Stream defaultProtocol
      setSocketOption sock ReuseAddr 1
      bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      listen sock 1
      port <- socketPort sock
      pairs <-
        replicateM
          (30 :: Int)
          ( do
              c <- socket AF_INET Stream defaultProtocol
              tid <- forkIO $ catch (connect c (SockAddrInet port (tupleToHostAddress (127, 0, 0, 1)))) (\(_ :: SomeException) -> pure ())
              pure (tid, c)
          )
      threadDelay 50000
      pure (sock, pairs, port)
    cleanup (srv, pairs, _) = do
      forM_ pairs $ \(tid, _) -> killThread tid
      forM_ pairs $ \(_, c) -> catch (close c) (\(_ :: SomeException) -> pure ())
      close srv
