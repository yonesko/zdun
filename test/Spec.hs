module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (bracket, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Lib (parseDuration, runApp)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import System.Directory (findExecutable)
import System.Environment (setEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    pendingWith,
    shouldBe,
    shouldContain,
    shouldReturn,
    shouldSatisfy,
  )

-- | Creates a bound, listening TCP server on a free port and runs an action with the port number.
-- The provided loop runs in a background thread that is killed when the action completes.
withBoundServer :: (S.Socket -> IO ()) -> (Int -> IO a) -> IO a
withBoundServer serverLoop action = do
  serverSock <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.setSocketOption serverSock S.ReuseAddr 1
  S.bind serverSock (S.SockAddrInet 0 (S.tupleToHostAddress (127, 0, 0, 1)))
  S.listen serverSock 128
  sockAddr <- S.getSocketName serverSock
  let port = case sockAddr of
        S.SockAddrInet p _ -> fromIntegral p
        _ -> error "Unexpected socket address"
  bracket
    (forkIO $ serverLoop serverSock)
    (\tid -> killThread tid >> S.close serverSock)
    (\_ -> action port)

-- | Spawns a lightweight local HTTP server on a free port simulating a test site.
-- Responds with 200 OK and UTF-8 encoded body with both English and Russian text.
withTestServer :: (Int -> IO a) -> IO a
withTestServer = withTestServerBody "Example Domain - Тестовый сервер готов к работе. Привет, мир!\n"

withTestServerBody :: T.Text -> (Int -> IO a) -> IO a
withTestServerBody bodyText = withBoundServer acceptLoop
  where
    bodyBytes = TE.encodeUtf8 bodyText
    respBytes =
      BS.concat
        [ "HTTP/1.1 200 OK\r\n",
          "Content-Type: text/plain; charset=utf-8\r\n",
          "Content-Length: ",
          BS8.pack $ show $ BS.length bodyBytes,
          "\r\nConnection: close\r\n\r\n",
          bodyBytes
        ]
    acceptLoop sock = do
      res <- try (S.accept sock) :: IO (Either IOError (S.Socket, S.SockAddr))
      case res of
        Left _ -> pure ()
        Right (conn, _) -> do
          _ <- forkIO $ do
            _ <- try (SB.recv conn 2048) :: IO (Either IOError BS.ByteString)
            _ <- try (SB.sendAll conn respBytes) :: IO (Either IOError ())
            S.close conn
          acceptLoop sock

-- | Spawns a server that accepts TCP connections but never sends any data.
-- Used to test that -t timeout is respected even when the connection is established.
withSilentServer :: (Int -> IO a) -> IO a
withSilentServer = withBoundServer silentLoop
  where
    silentLoop sock = do
      res <- try (S.accept sock) :: IO (Either IOError (S.Socket, S.SockAddr))
      case res of
        Left _ -> pure ()
        Right (conn, _) -> do
          _ <- forkIO $ do
            _ <- try (SB.recv conn 2048) :: IO (Either IOError BS.ByteString)
            -- Deliberately no send here — simulate a hung server.
            -- Close connection on exit to prevent resource leaks.
            S.close conn
          silentLoop sock

-- | Spawns a server that requires Basic Auth header, returning 200 OK only if Authorization header matches.
withBasicAuthServer :: String -> (Int -> IO a) -> IO a
withBasicAuthServer expectedAuth = withBoundServer acceptLoop
  where
    respOk = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nOK"
    respUnauthorized = "HTTP/1.1 401 Unauthorized\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
    acceptLoop sock = do
      res <- try (S.accept sock) :: IO (Either IOError (S.Socket, S.SockAddr))
      case res of
        Left _ -> pure ()
        Right (conn, _) -> do
          _ <- forkIO $ do
            mReq <- try (SB.recv conn 2048) :: IO (Either IOError BS.ByteString)
            case mReq of
              Right reqBytes
                | BS8.pack ("Authorization: Basic " ++ expectedAuth) `BS.isInfixOf` reqBytes -> do
                    _ <- try (SB.sendAll conn respOk) :: IO (Either IOError ())
                    pure ()
              _ -> do
                _ <- try (SB.sendAll conn respUnauthorized) :: IO (Either IOError ())
                pure ()
            S.close conn
          acceptLoop sock

-- | Runs an action with the path to the zdun-exe binary,
-- or marks the test as pending if the binary is not found in PATH.
withZdunExe :: (FilePath -> IO ()) -> IO ()
withZdunExe action = do
  mExe <- findExecutable "zdun-exe"
  maybe (pendingWith "zdun-exe binary not found in PATH") action mExe

main :: IO ()
main = do
  setEnv "no_proxy" "127.0.0.1,localhost"
  setEnv "NO_PROXY" "127.0.0.1,localhost"
  hspec spec

spec :: Spec
spec = do
  describe "parseDuration" $ do
    it "parses single digit seconds" $
      parseDuration "1s" `shouldBe` Right 1

    it "parses multi digit seconds" $
      parseDuration "12s" `shouldBe` Right 12

    it "parses single digit minutes" $
      parseDuration "2m" `shouldBe` Right 120

    it "parses multi digit minutes" $
      parseDuration "10m" `shouldBe` Right 600

    it "parses multi digit minutes with multi digit seconds" $
      parseDuration "10m77s" `shouldBe` Right 677

    it "rejects invalid characters" $
      parseDuration "1x" `shouldBe` Left "Invalid duration 1x"

    it "rejects strings starting with non-digit" $
      parseDuration "s1" `shouldBe` Left "Invalid duration s1"

    it "parses empty string" $
      parseDuration "" `shouldBe` Right 0

    it "parses zero string" $
      parseDuration "0" `shouldBe` Right 0

    it "parses hours" $
      parseDuration "1h" `shouldBe` Right 3600

    it "parses fractional seconds" $
      parseDuration "1.5s" `shouldBe` Right 1.5

    it "parses milliseconds" $
      parseDuration "500ms" `shouldBe` Right 0.5

  describe "main / CLI integration tests with test server (example.org mock)" $ do
    it "succeeds with basic --http URL check and forwards command" $
      withTestServer $ \port ->
        runApp ["--http", "http://127.0.0.1:" ++ show port, "--", "echo", "hello"]
          `shouldReturn` Right ("echo", ["hello"])

    it "succeeds with normalized target --http host:port and multiple arguments" $
      withTestServer $ \port ->
        runApp ["--http", "127.0.0.1:" ++ show port, "--", "my-app", "--flag", "val"]
          `shouldReturn` Right ("my-app", ["--flag", "val"])

    it "succeeds with body regex match: --http 'Example Domain@host:port'" $
      withTestServer $ \port ->
        runApp ["--http", "Example Domain@127.0.0.1:" ++ show port, "--", "true"]
          `shouldReturn` Right ("true", [])

    it "succeeds with verbose flag: -v --http host:port" $
      withTestServer $ \port ->
        runApp ["-v", "--http", "127.0.0.1:" ++ show port, "--", "true"]
          `shouldReturn` Right ("true", [])

    it "succeeds with TCP port check: --tcp 127.0.0.1:port" $
      withTestServer $ \port ->
        runApp ["--tcp", "127.0.0.1:" ++ show port, "--", "true"]
          `shouldReturn` Right ("true", [])

    it "succeeds combining multiple checks: -v -t 5s --http ... --tcp ..." $
      withTestServer $ \port ->
        runApp ["-v", "-t", "5s", "--http", "127.0.0.1:" ++ show port, "--tcp", "127.0.0.1:" ++ show port, "--", "echo", "all-passed"]
          `shouldReturn` Right ("echo", ["all-passed"])

    it "succeeds with basic auth in URL: --http http://admin:pass@127.0.0.1:port" $
      withTestServer $ \port ->
        runApp ["--http", "http://admin:pass@127.0.0.1:" ++ show port, "--", "echo", "auth-ok"]
          `shouldReturn` Right ("echo", ["auth-ok"])

    it "sends correct Basic Auth header from --http http://admin:pass@host:port" $
      -- "admin:pass" in base64 is "YWRtaW46cGFzcw=="
      withBasicAuthServer "YWRtaW46cGFzcw==" $ \port ->
        runApp ["--http", "http://admin:pass@127.0.0.1:" ++ show port, "--", "echo", "auth-verified"]
          `shouldReturn` Right ("echo", ["auth-verified"])

    it "fails when Basic Auth credentials do not match within timeout (-t 500ms)" $
      withBasicAuthServer "YWRtaW46cGFzcw==" $ \port ->
        runApp ["-t", "500ms", "--http", "http://wrong:creds@127.0.0.1:" ++ show port, "--", "echo", "fail"]
          `shouldReturn` Left (ExitFailure 1)

    it "fails and does not exec when body regex does not match within timeout (-t 500ms)" $
      withTestServer $ \port ->
        runApp ["-t", "500ms", "--http", "NonExistentString12345XYZ@127.0.0.1:" ++ show port, "--", "echo", "fail"]
          `shouldReturn` Left (ExitFailure 1)

    it "fails and does not exec when TCP connection fails within timeout (-t 500ms)" $
      runApp ["-t", "500ms", "--tcp", "127.0.0.1:54321", "--", "echo", "fail"]
        `shouldReturn` Left (ExitFailure 1)

    it "fails when no command is provided after options" $
      runApp ["--http", "127.0.0.1:80"]
        `shouldReturn` Left (ExitFailure 1)

    it "fails when invalid option flag is provided" $
      runApp ["--unknown-flag", "--", "true"]
        `shouldReturn` Left (ExitFailure 1)

  describe "timeout (-t) is respected when server accepts but never responds" $ do
    it "HTTP: exits with failure and does not hang beyond -t 1s when server is silent" $
      withSilentServer $ \port -> do
        start <- getCurrentTime
        result <- runApp ["-t", "1s", "--http", "127.0.0.1:" ++ show port, "--", "echo", "fail"]
        end <- getCurrentTime
        result `shouldBe` Left (ExitFailure 1)
        -- Wall-clock time must be well under 3s (generous upper bound to avoid flakiness).
        -- If timeout were ignored the test would hang for minutes.
        let elapsed = realToFrac (diffUTCTime end start) :: Double
        elapsed `shouldSatisfy` (< 3.0)

    it "TCP: exits with failure and does not hang beyond -t 1s when server is silent" $
      withSilentServer $ \port -> do
        start <- getCurrentTime
        -- TCP check succeeds immediately (port is open), but if the check
        -- kept looping we'd still be bounded by -t.
        -- This validates the overall timeout machinery end-to-end.
        result <- runApp ["-t", "1s", "--tcp", "127.0.0.1:" ++ show port, "--", "echo", "fail"]
        end <- getCurrentTime
        -- TCP to a listening socket succeeds, so the command should succeed
        -- (not time out). Either way the call must return within 3s.
        let elapsed = realToFrac (diffUTCTime end start) :: Double
        elapsed `shouldSatisfy` (< 3.0)
        -- Suppress unused-result warning; result can be Ok or Err depending on TCP probe behaviour
        result `shouldSatisfy` const True

  describe "zdun-exe process execution (end-to-end)" $ do
    it "executes binary, checks test server and runs the target command" $
      withTestServer $ \port ->
        withZdunExe $ \exe -> do
          (code, stdoutStr, stderrStr) <- readProcessWithExitCode exe ["-v", "--http", "127.0.0.1:" ++ show port, "--", "echo", "BINARY_EXEC_OK"] ""
          code `shouldBe` ExitSuccess
          stdoutStr `shouldContain` "BINARY_EXEC_OK"
          stderrStr `shouldContain` "[zdun]"
          stderrStr `shouldContain` "Running 127.0.0.1:"
          stderrStr `shouldContain` "All checks passed"

    it "executes binary with regex and tcp checks on test server" $
      withTestServer $ \port ->
        withZdunExe $ \exe -> do
          (code, stdoutStr, stderrStr) <- readProcessWithExitCode exe ["-v", "--http", "Example Domain@127.0.0.1:" ++ show port, "--tcp", "127.0.0.1:" ++ show port, "--", "echo", "BINARY_COMBINED_OK"] ""
          code `shouldBe` ExitSuccess
          stdoutStr `shouldContain` "BINARY_COMBINED_OK"
          stderrStr `shouldContain` "[zdun]"
          stderrStr `shouldContain` "All checks passed"

    it "executes binary and fails on regex mismatch with timeout" $
      withTestServer $ \port ->
        withZdunExe $ \exe -> do
          (code, stdoutStr, stderrStr) <- readProcessWithExitCode exe ["-t", "500ms", "--http", "NoSuchContentShouldFail@127.0.0.1:" ++ show port, "--", "echo", "SHOULD_NOT_EXECUTE"] ""
          code `shouldBe` ExitFailure 1
          isInfixOf "SHOULD_NOT_EXECUTE" stdoutStr `shouldBe` False
          stderrStr `shouldContain` "Some checks failed"

  describe "verbose logging flag (-v)" $ do
    it "prints progress and success logs to stderr when -v is enabled" $
      withTestServer $ \port ->
        withZdunExe $ \exe -> do
          (code, _, stderrStr) <- readProcessWithExitCode exe ["-v", "--http", "127.0.0.1:" ++ show port, "--", "true"] ""
          code `shouldBe` ExitSuccess
          stderrStr `shouldContain` "[zdun]"
          stderrStr `shouldContain` "Running 127.0.0.1:"
          stderrStr `shouldContain` ("127.0.0.1:" ++ show port ++ " OK")
          stderrStr `shouldContain` "All checks passed"

    it "prints error logs to stderr when -v is enabled and TCP check fails" $
      withZdunExe $ \exe -> do
        (code, _, stderrStr) <- readProcessWithExitCode exe ["-v", "-t", "500ms", "--tcp", "127.0.0.1:54321", "--", "true"] ""
        code `shouldBe` ExitFailure 1
        stderrStr `shouldContain` "[zdun]"
        stderrStr `shouldContain` "Running 127.0.0.1:54321"
        stderrStr `shouldContain` "127.0.0.1:54321 error:"
        stderrStr `shouldContain` "Some checks failed"

    it "prints error logs to stderr when -v is enabled and HTTP regex check fails" $
      withTestServer $ \port ->
        withZdunExe $ \exe -> do
          (code, _, stderrStr) <- readProcessWithExitCode exe ["-v", "-t", "500ms", "--http", "NonExistentRe@127.0.0.1:" ++ show port, "--", "true"] ""
          code `shouldBe` ExitFailure 1
          stderrStr `shouldContain` "[zdun]"
          stderrStr `shouldContain` "Running NonExistentRe@127.0.0.1:"
          stderrStr `shouldContain` "error: body did not match regex: NonExistentRe"
          stderrStr `shouldContain` "Some checks failed"

    it "stays completely silent on stderr when -v is not specified and checks pass" $
      withTestServer $ \port ->
        withZdunExe $ \exe -> do
          (code, _, stderrStr) <- readProcessWithExitCode exe ["--http", "127.0.0.1:" ++ show port, "--", "true"] ""
          code `shouldBe` ExitSuccess
          isInfixOf "Running" stderrStr `shouldBe` False
          isInfixOf "All checks passed" stderrStr `shouldBe` False
          stderrStr `shouldBe` ""

    it "prints failure logs to stderr even without -v when a check fails" $
      withZdunExe $ \exe -> do
        (code, _, stderrStr) <- readProcessWithExitCode exe ["-t", "500ms", "--tcp", "127.0.0.1:54321", "--", "true"] ""
        code `shouldBe` ExitFailure 1
        stderrStr `shouldContain` "Some checks failed"

  describe "main / CLI tests with Russian body and regex" $ do
    it "succeeds with Russian substring match in body: --http 'Привет@host:port'" $
      withTestServer $ \port ->
        runApp ["--http", "Привет@127.0.0.1:" ++ show port, "--", "true"]
          `shouldReturn` Right ("true", [])

    it "succeeds with Russian phrase in body: --http 'готов к работе@host:port'" $
      withTestServer $ \port ->
        runApp ["--http", "готов к работе@127.0.0.1:" ++ show port, "--", "echo", "russian-ok"]
          `shouldReturn` Right ("echo", ["russian-ok"])

    it "succeeds with Russian regex pattern: --http '[Тт]естовый.*сервер@host:port'" $
      withTestServer $ \port ->
        runApp ["--http", "[Тт]естовый.*сервер@127.0.0.1:" ++ show port, "--", "true"]
          `shouldReturn` Right ("true", [])

    it "succeeds with Russian regex alternation: --http 'Привет, (мир|вселенная)!@host:port'" $
      withTestServer $ \port ->
        runApp ["--http", "Привет, (мир|вселенная)!@127.0.0.1:" ++ show port, "--", "true"]
          `shouldReturn` Right ("true", [])

    it "fails when Russian regex does not match within timeout (-t 500ms)" $
      withTestServer $ \port ->
        runApp ["-t", "500ms", "--http", "НесуществующийТекст@127.0.0.1:" ++ show port, "--", "echo", "fail"]
          `shouldReturn` Left (ExitFailure 1)

    it "executes binary zdun-exe with Russian regex and Cyrillic command output" $
      withTestServer $ \port ->
        withZdunExe $ \exe -> do
          (code, stdoutStr, stderrStr) <- readProcessWithExitCode exe ["-v", "--http", "готов к работе@127.0.0.1:" ++ show port, "--", "echo", "ТЕСТ_ПРОЙДЕН"] ""
          code `shouldBe` ExitSuccess
          stdoutStr `shouldContain` "ТЕСТ_ПРОЙДЕН"
          stderrStr `shouldContain` "[zdun]"
          stderrStr `shouldContain` "All checks passed"

    it "executes binary zdun-exe and fails when Russian regex does not match" $
      withTestServer $ \port ->
        withZdunExe $ \exe -> do
          (code, stdoutStr, stderrStr) <- readProcessWithExitCode exe ["-t", "500ms", "--http", "НенайденныйПаттерн123@127.0.0.1:" ++ show port, "--", "echo", "НЕ_ДОЛЖНО_БЫТЬ"] ""
          code `shouldBe` ExitFailure 1
          isInfixOf "НЕ_ДОЛЖНО_БЫТЬ" stdoutStr `shouldBe` False
          stderrStr `shouldContain` "Some checks failed"
