module Main (main) where

import Control.Applicative (many, some, (<**>))
import Control.Concurrent (withMVar)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (newMVar)
import Control.Monad (when)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import Http (checkHttp)
import Lib (parseDuration)
import Options.Applicative (Parser, ParserInfo, ReadM, eitherReader, execParser, fullDesc, help, helper, info, infoOption, long, metavar, option, progDesc, short, showDefault, strArgument, strOption, switch, value)
import Paths_zdun (version)
import Probes (worker)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import Tcp (isPortOpen)

data Options = Options
  { optTimeout :: NominalDiffTime,
    optVerbose :: Bool,
    optTcp :: [String],
    optHttp :: [String],
    optRest :: [String]
  }

optsInfo :: ParserInfo Options
optsInfo =
  info
    ( opts
        <**> helper
        <**> infoOption
          (showVersion version)
          ( long "version"
              <> help "Show version information"
          )
    )
    ( fullDesc
        <> progDesc "Zdun - utility to exec a command after waiting for rediness probes to success or timeout"
    )

opts :: Parser Options
opts =
  Options
    <$> option durationParser (short 't' <> value 0 <> showDefault <> help "Timeout")
    <*> switch (short 'v' <> help "Verbose")
    <*> many (strOption (long "tcp" <> help "TCP connection check"))
    <*> many (strOption (long "http" <> help "HTTP check: URL (for 200 OK) or regex@URL"))
    <*> some (strArgument (metavar "--- CMD"))

durationParser :: ReadM NominalDiffTime
durationParser = eitherReader parseDuration

main :: IO ()
main = do
  options <- execParser optsInfo
  case optRest options of
    [] -> do
      putStrLn "[zdun] command after -- is not specified"
      exitFailure
    (cmd : args) -> do
      -- spawn checks concurrently
      let tcpChecks = [(tcp, isPortOpen tcp) | tcp <- optTcp options]
      let httpChecks = [(httpTarget, checkHttp httpTarget) | httpTarget <- optHttp options]
      let allChecks = tcpChecks ++ httpChecks
      logLock <- newMVar ()
      let logMsg = if optVerbose options then \msg -> withMVar logLock $ \_ -> hPutStrLn stderr msg else const (pure ())
      start <- getCurrentTime
      checkResults <-
        mapConcurrently
          ( \(name, action) -> do
              ok <- worker logMsg name action (optTimeout options)
              pure (name, ok)
          )
          allChecks
      end <- getCurrentTime
      let diff = diffUTCTime end start
      -- assert results
      when (optVerbose options) (hPutStrLn stderr $ "[zdun] All checks passed in " <> show diff)
      let failedChecks = [name | (name, False) <- checkResults]
      if not (null failedChecks)
        then do
          hPutStrLn stderr $ "[zdun] Some checks failed: " ++ unwords failedChecks
          exitFailure
        else executeFile cmd True args Nothing
