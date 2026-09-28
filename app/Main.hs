module Main (main) where

import Control.Applicative (some, many, (<**>))
import Control.Concurrent.Async (mapConcurrently)
import Data.Version (showVersion)
import Lib (parseDuration)
import Options.Applicative (Parser, ParserInfo, ReadM, eitherReader, execParser, fullDesc, help, helper, info, infoOption, long, metavar, option, progDesc, short, showDefault, strArgument, strOption, value)
import Paths_zdun (version)
import Probes (worker)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import Tcp (isPortOpen)

data Options = Options
  { optTimeout :: Int,
    optTcp :: [String],
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
    <*> many (strOption (long "tcp" <> help "TCP connection check"))
    <*> some (strArgument (metavar "--- CMD"))

durationParser :: ReadM Int
durationParser = eitherReader parseDuration

main :: IO ()
main = do
  options <- execParser optsInfo
  case optRest options of
    [] -> do
      putStrLn "[zdun]: command after -- is not specified"
      exitFailure
    (cmd : args) -> do
      checkResults <- mapConcurrently (\tcp -> worker (isPortOpen tcp) (optTimeout options)) (optTcp options)
      if elem False checkResults
        then
          hPutStrLn stderr $ "[zdun]: Some checks failed"
        else executeFile cmd True args Nothing
