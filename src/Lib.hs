module Lib
  ( parseDuration,
    Options (..),
    optsInfo,
    opts,
    durationParser,
    runWithOptions,
    runApp,
    defaultMain,
    main,
  )
where

import Control.Applicative (many, some, (<**>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime)
import Data.Version (showVersion)
import GHC.Base (Alternative ((<|>)))
import Http (checkHttp)
import qualified Options.Applicative as OA
import Paths_zdun (version)
import Probes (worker)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import Tcp (isPortOpen)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readMaybe)

-- | Parse duration strings like "1s", "2m", "10m77s", "1h30m", "500ms", "0.5s", "0", ""
parseDuration :: String -> Either String NominalDiffTime
parseDuration "" = Right 0
parseDuration "0" = Right 0
parseDuration s =
  case [val | (val, "") <- ReadP.readP_to_S (durationP <* ReadP.eof) s] of
    (val : _) -> Right val
    [] -> Left ("Invalid duration " <> s)
  where
    durationP :: ReadP.ReadP NominalDiffTime
    durationP = sum <$> ReadP.many1 componentP

    componentP :: ReadP.ReadP NominalDiffTime
    componentP = (*) <$> numberP <*> unitP
      where
        numberP = do
          digits <- ReadP.munch1 isDigit
          mFrac <- ReadP.option "" (ReadP.char '.' *> ReadP.munch1 isDigit)
          pure $ case mFrac of
            "" -> fromInteger (read digits)
            f -> case readMaybe (digits ++ "." ++ f) of
              Just (d :: Double) -> realToFrac d
              Nothing -> 0

        unitP =
          3600 <$ ReadP.char 'h'
            <|> 0.001 <$ ReadP.string "ms"
            <|> 60 <$ ReadP.char 'm'
            <|> 1 <$ ReadP.char 's'

data Options = Options
  { optTimeout :: NominalDiffTime,
    optVerbose :: Bool,
    optTcp :: [String],
    optHttp :: [String],
    optRest :: [String]
  }
  deriving (Show, Eq)

optsInfo :: OA.ParserInfo Options
optsInfo =
  OA.info
    ( opts
        <**> OA.helper
        <**> OA.infoOption
          (showVersion version)
          (OA.long "version" <> OA.help "Show version information")
    )
    ( OA.fullDesc
        <> OA.progDesc "Zdun - utility to exec a command after waiting for rediness probes to success or timeout"
    )

opts :: OA.Parser Options
opts =
  Options
    <$> OA.option durationParser (OA.short 't' <> OA.value 0 <> OA.showDefault <> OA.help "Timeout")
    <*> OA.switch (OA.short 'v' <> OA.help "Verbose")
    <*> many (OA.strOption (OA.long "tcp" <> OA.help "TCP connection check"))
    <*> many (OA.strOption (OA.long "http" <> OA.help "HTTP check: URL (for 200 OK) or regex@URL"))
    <*> some (OA.strArgument (OA.metavar "--- CMD"))

durationParser :: OA.ReadM NominalDiffTime
durationParser = OA.eitherReader parseDuration

-- | Runs readiness checks for parsed options.
-- Returns Right (cmd, args) if all checks pass.
-- Returns Left (ExitFailure 1) if checks fail or command is missing.
runWithOptions :: Options -> IO (Either ExitCode (FilePath, [String]))
runWithOptions options =
  case optRest options of
    [] -> do
      hPutStrLn stderr "[zdun] command after -- is not specified"
      pure (Left (ExitFailure 1))
    (cmd : args) -> do
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
      let failedChecks = [name | (name, False) <- checkResults]
      if not (null failedChecks)
        then do
          hPutStrLn stderr $ unwords ["[zdun] Some checks failed in", show diff ++ ":", unwords failedChecks]
          pure (Left (ExitFailure 1))
        else do
          when (optVerbose options) (hPutStrLn stderr $ "[zdun] All checks passed in " <> show diff)
          pure (Right (cmd, args))

-- | Parses command-line arguments and runs checks, returning either an ExitCode or the target command and args.
runApp :: [String] -> IO (Either ExitCode (FilePath, [String]))
runApp args =
  case OA.execParserPure OA.defaultPrefs optsInfo args of
    OA.Success options -> runWithOptions options
    OA.Failure failure -> do
      let (msg, exitCode) = OA.renderFailure failure "zdun"
      hPutStrLn stderr msg
      pure (Left exitCode)
    OA.CompletionInvoked compl -> do
      msg <- OA.execCompletion compl "zdun"
      hPutStrLn stderr msg
      pure (Left ExitSuccess)

-- | Default main implementation which executes the command on success or exits with the error code.
defaultMain :: IO ()
defaultMain = do
  args <- getArgs
  res <- runApp args
  case res of
    Left code -> exitWith code
    Right (cmd, cmdArgs) -> executeFile cmd True cmdArgs Nothing

-- | Main entry point.
main :: IO ()
main = defaultMain
