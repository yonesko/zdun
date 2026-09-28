module Main (main) where

import Control.Applicative (many, (<**>))
import Control.Concurrent.Async (forConcurrently_)
import Data.List.Split (splitOn)
import Data.Version (showVersion)
import Lib (parseDuration)
import Options.Applicative (Parser, ParserInfo, ReadM, eitherReader, execParser, fullDesc, help, helper, info, infoOption, long, metavar, option, progDesc, short, showDefault, strArgument, strOption, value)
import Paths_zdun (version)
import Probes (isPortOpen, worker)
import System.Exit (exitFailure)
import System.Posix.Process (executeFile)

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
    <*> many (strArgument (metavar "ARGS..."))

durationParser :: ReadM Int
durationParser = eitherReader parseDuration

main :: IO ()
main = do
  options <- execParser optsInfo
  case optRest options of
    [] -> do
      putStrLn "zdun: не указана команда для выполнения после --"
      exitFailure
    (cmd : args) -> do
      -- wait here
      forConcurrently_ (optTcp options) (\tcp -> worker (isPortOpen "ya.ru" "80") (optTimeout options))
      executeFile cmd True args Nothing

--  ()