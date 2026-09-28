module Main (main) where

import Control.Applicative (many, (<**>))
import Data.Version (showVersion)
import Lib (parseDuration)
import Options.Applicative (Parser, ParserInfo, ReadM, eitherReader, execParser, fullDesc, help, helper, info, infoOption, long, metavar, option, progDesc, short, strArgument)
import Paths_zdun (version)

data Options = Options
  { optTimeout :: Int,
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
    <$> option durationParser (short 't' <> help "Таймаут ожидания успеха")
    <*> many (strArgument (metavar "ARGS..."))

durationParser :: ReadM Int
durationParser = eitherReader parseDuration

main :: IO ()
main = do
  options <- execParser optsInfo

  let arguments = optRest options
  putStrLn $ "Полученные аргументы после -- или позиционные: " ++ show arguments
