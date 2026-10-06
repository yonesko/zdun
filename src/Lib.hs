module Lib
  ( parseDuration,
    Options (..),
    optsInfo,
    opts,
    cliPrefs,
    probeHelpDoc,
    footerDoc,
    runWithOptions,
    runApp,
    defaultMain,
    main,
  )
where

import Control.Applicative (Alternative ((<|>)), many, some, (<**>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (newMVar, withMVar)
import Control.Exception (catch)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Data.Version (showVersion)
import GHC.Exception.Type (displayException)
import GHC.IO.Exception (ioe_description)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Options.Applicative as OA
import qualified Options.Applicative.Help.Pretty as P
import Paths_zdun (version)
import Probes (runLoop)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Posix.Process (executeFile)
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (readMaybe)
import Types

-- | Parse duration strings like "1s", "2m", "10m77s", "1h30m", "500ms", "0.5s", "0", ""
parseDuration :: String -> Either String NominalDiffTime
parseDuration "" = Right 0
parseDuration "0" = Right 0
parseDuration s =
  case [val | (val, "") <- ReadP.readP_to_S (durationP <* ReadP.eof) s] of
    val : _ -> Right val
    [] -> Left ("Invalid duration " <> s)
  where
    durationP = sum <$> ReadP.many1 componentP
    componentP :: ReadP.ReadP NominalDiffTime
    componentP = (*) <$> numberP <*> unitP
      where
        numberP = do
          digits <- ReadP.munch1 isDigit
          mFrac <- ReadP.option "" (ReadP.char '.' *> ReadP.munch1 isDigit)
          let str = if null mFrac then digits else digits <> ('.' : mFrac)
          pure $ maybe 0 realToFrac (readMaybe str :: Maybe Double)

        unitP =
          3600 <$ ReadP.char 'h'
            <|> 0.001 <$ ReadP.string "ms"
            <|> 60 <$ ReadP.char 'm'
            <|> 1 <$ ReadP.char 's'

data Options = Options
  { optTimeout :: NominalDiffTime,
    optVerbose :: Bool,
    optProbes :: [Probe],
    optRest :: [String]
  }

probeHelpDoc :: P.Doc
probeHelpDoc =
  P.vsep
    [ "Readiness probe specification. Syntax:",
      P.indent 2 $
        P.annotate (P.color P.Cyan) "tcp://"
          <> P.annotate P.bold "<host>:<port>"
          P.<+> checkDoc,
      P.indent 2 $
        P.annotate (P.color P.Cyan) "http[s]://"
          <> P.annotate P.bold "<url>"
          P.<+> checkDoc
    ]
  where
    checkDoc =
      P.brackets
        ( P.annotate (P.color P.Yellow) "contains"
            P.<+> P.annotate (P.color P.Magenta) "<pattern>"
            P.<+> P.pipe
            P.<+> P.annotate (P.color P.Yellow) "matches"
            P.<+> P.annotate (P.color P.Magenta) "<regex>"
        )

footerDoc :: P.Doc
footerDoc =
  P.vsep
    [ P.annotate P.bold "Examples:",
      P.indent 2 $
        P.annotate (P.color P.Green) "zdun -p tcp://postgres:5432 -t 30s -- ./run.sh",
      P.indent 2 $
        P.annotate (P.color P.Green) "zdun -p \"tcp://smtp:25 contains 220\" -t 10s -- ./mail-client",
      P.indent 2 $
        P.annotate (P.color P.Green) "zdun -p \"http://api:8080/health contains UP\" -t 1m -- npm start",
      P.indent 2 $
        P.annotate (P.color P.Green) "zdun -p \"https://service:8443/status matches [0-9]+\" -- ./start-billing.sh"
    ]

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
        <> OA.progDesc "Zdun - utility to exec a command after waiting for readiness probes to succeed or timeout"
        <> OA.footerDoc (Just footerDoc)
    )

opts :: OA.Parser Options
opts =
  Options
    <$> OA.option
      (OA.eitherReader parseDuration)
      ( OA.short 't'
          <> OA.long "timeout"
          <> OA.value 0
          <> OA.showDefault
          <> OA.metavar "DURATION"
          <> OA.help "Timeout"
      )
    <*> OA.switch (OA.short 'v' <> OA.long "verbose" <> OA.help "Verbose")
    <*> many
      ( OA.option
          (OA.eitherReader parseProbe)
          ( OA.long "probe"
              <> OA.short 'p'
              <> OA.metavar "PROBE"
              <> OA.helpDoc (Just probeHelpDoc)
          )
      )
    <*> some (OA.strArgument (OA.metavar "-- CMD"))

printLog :: String -> IO ()
printLog msg = do
  ts <- formatTime defaultTimeLocale "%H:%M:%S" <$> getZonedTime
  hPutStrLn stderr $ "[zdun] [" <> ts <> "] " <> msg

-- | Runs readiness checks for parsed options.
-- Returns Right (cmd, args) if all checks pass.
-- Returns Left (ExitFailure 1) if checks fail or command is missing.
runWithOptions :: Options -> IO (Either ExitCode (FilePath, [String]))
runWithOptions options =
  case optRest options of
    [] -> do
      hPutStrLn stderr "[zdun] command after -- is not specified"
      pure $ Left $ ExitFailure 1
    cmd : args -> do
      manager <- newManager tlsManagerSettings
      logLock <- newMVar ()
      let logMsg msg = when (optVerbose options) $ withMVar logLock $ const $ printLog msg
      let env = Env {envManager = manager, envLogger = logMsg}
      probeResults <- mapConcurrently (\p -> (,) p <$> runLoop env (optTimeout options) p) (optProbes options)
      let failedChecks = [formatProbe probe <> ": " <> str | (probe, Left str) <- probeResults]
      if null failedChecks
        then do
          when (optVerbose options) $ logMsg "All checks passed"
          pure $ Right (cmd, args)
        else do
          printLog $ unwords ["Some checks failed:", unwords failedChecks]
          pure $ Left $ ExitFailure 1

-- | Parser preferences: show full help when invoked without arguments.
cliPrefs :: OA.ParserPrefs
cliPrefs = OA.prefs OA.showHelpOnEmpty

-- | Parses command-line arguments and runs checks, returning either an ExitCode or the target command and args.
runApp :: [String] -> IO (Either ExitCode (FilePath, [String]))
runApp args =
  case OA.execParserPure cliPrefs optsInfo args of
    OA.Success options -> runWithOptions options
    OA.Failure failure -> do
      let (msg, exitCode) = OA.renderFailure failure "zdun"
      hPutStrLn stderr msg
      pure (Left exitCode)
    OA.CompletionInvoked compl -> do
      msg <- OA.execCompletion compl "zdun"
      putStrLn msg
      pure (Left ExitSuccess)

-- | Default main implementation which executes the command on success or exits with the error code.
defaultMain :: IO ()
defaultMain = do
  args <- getArgs
  res <- runApp args
  case res of
    Left code -> exitWith code
    Right (cmd, cmdArgs) -> exec cmd cmdArgs

-- | Main entry point.
main :: IO ()
main = defaultMain

exec :: FilePath -> [String] -> IO ()
exec cmd args = catch (executeFile cmd True args Nothing) handleException
  where
    handleException :: IOError -> IO ()
    handleException e = do
      let desc = if null (ioe_description e) then displayException e else ioe_description e
      printLog $ "Failed to execute '" <> cmd <> "': " <> desc
      exitWith $ ExitFailure $ status e

    status e
      | isPermissionError e = 126
      | isDoesNotExistError e = 127
      | otherwise = 127
