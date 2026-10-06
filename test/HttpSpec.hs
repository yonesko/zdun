module HttpSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, catch, finally)
import Control.Monad (forM_, forever, void)
import qualified Data.ByteString.Char8 as BS
import Http (checkHttp)
import Network.HTTP.Client (managerSetProxy, newManager, noProxy)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    listen,
    setSocketOption,
    socket,
    socketPort,
    tupleToHostAddress,
  )
import Network.Socket.ByteString (recv, sendAll)
import Test.Hspec
  ( Spec,
    aroundAll,
    describe,
    it,
    shouldBe,
  )
import Types (Check (Contains, Matches), Env (Env))

spec :: Spec
spec = do
  describe "checkHttp" $ do
    aroundAll withMockServer $ do
      describe "success" $ do
        it "succeeds on 200 OK without check" $ \(env, baseUrl) -> do
          res <- checkHttp env (baseUrl <> "/status/200") Nothing
          res `shouldBe` Right ()

        it "succeeds on 201 Created" $ \(env, baseUrl) -> do
          res <- checkHttp env (baseUrl <> "/status/201") Nothing
          res `shouldBe` Right ()

        it "succeeds on 204 No Content" $ \(env, baseUrl) -> do
          res <- checkHttp env (baseUrl <> "/status/204") Nothing
          res `shouldBe` Right ()

        it "succeeds when body contains expected substring" $ \(env, baseUrl) -> do
          res <- checkHttp env (baseUrl <> "/get") (Just (Contains "origin"))
          res `shouldBe` Right ()

        it "succeeds when body matches regex" $ \(env, baseUrl) -> do
          res <- checkHttp env (baseUrl <> "/get") (Just (Matches (regexEscape (baseUrl <> "/get"))))
          res `shouldBe` Right ()

      describe "status failure" $ do
        let failedStatuses :: [(Int, String)]
            failedStatuses =
              [ (400, "Bad Request"),
                (401, "Unauthorized"),
                (403, "Forbidden"),
                (404, "Not Found"),
                (500, "Internal Server Error"),
                (502, "Bad Gateway"),
                (503, "Service Unavailable")
              ]
        forM_ failedStatuses $ \(code, name) ->
          it ("fails on " <> show code <> " " <> name) $ \(env, baseUrl) -> do
            res <- checkHttp env (baseUrl <> "/status/" <> show code) Nothing
            res `shouldBe` Left ("response status is not successful: " <> show code)

      describe "content check failure" $ do
        it "fails when substring is not contained in response body" $ \(env, baseUrl) -> do
          res <- checkHttp env (baseUrl <> "/get") (Just (Contains "nonexistent_substring_12345"))
          res `shouldBe` Left "response body doesn't contain substring"

        it "fails when regex does not match response body" $ \(env, baseUrl) -> do
          res <- checkHttp env (baseUrl <> "/get") (Just (Matches "^[0-9]+$"))
          res `shouldBe` Left "response body doesn't match re"

      describe "timeout" $ do
        it "times out when response takes longer than 2 seconds" $ \(env, baseUrl) -> do
          res <- checkHttp env (baseUrl <> "/delay/3") Nothing
          res `shouldBe` Left "Response timeout"

      describe "invalid url" $ do
        it "fails on invalid URL" $ \(env, _) -> do
          res <- checkHttp env "invalid-url" Nothing
          res `shouldBe` Left "Invalid URL: Invalid URL"

withMockServer :: ((Env, String) -> IO ()) -> IO ()
withMockServer action =
  bracket (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    listen sock 128
    port <- socketPort sock
    let baseUrl = "http://127.0.0.1:" <> show port
    mgr <- newManager (managerSetProxy noProxy tlsManagerSettings)
    let env = Env mgr (\_ -> pure ())
    tid <- forkIO $ forever $ do
      (conn, _) <- accept sock
      void $ forkIO $ handleConn baseUrl conn `finally` close conn
    action (env, baseUrl) `finally` killThread tid

handleConn :: String -> Socket -> IO ()
handleConn baseUrl conn = (`catch` (\(_ :: SomeException) -> pure ())) $ do
  req <- recv conn 4096
  case BS.words req of
    (_method : path : _) -> respond baseUrl conn path
    _ -> pure ()

respond :: String -> Socket -> BS.ByteString -> IO ()
respond baseUrl conn path
  | path == "/status/200" =
      sendAll conn "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
  | path == "/status/201" =
      sendAll conn "HTTP/1.1 201 Created\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
  | path == "/status/204" =
      sendAll conn "HTTP/1.1 204 No Content\r\nConnection: close\r\n\r\n"
  | path == "/get" = do
      let body = "{\"origin\": \"127.0.0.1\", \"url\": \"" <> BS.pack baseUrl <> "/get\"}"
      sendAll conn $
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: "
          <> BS.pack (show (BS.length body))
          <> "\r\nConnection: close\r\n\r\n"
          <> body
  | BS.isPrefixOf "/status/" path = do
      let codeStr = BS.drop (BS.length "/status/") path
      sendAll conn $
        "HTTP/1.1 " <> codeStr <> " Status\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
  | BS.isPrefixOf "/delay/" path = do
      threadDelay 2200000
      sendAll conn "HTTP/1.1 200 OK\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
  | otherwise =
      sendAll conn "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"

regexEscape :: String -> String
regexEscape = concatMap (\c -> if c == '.' then "\\." else [c])
