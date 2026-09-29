module Lib
  ( parseDuration,
  )
where

import Data.Char (isDigit)
import Data.Time.Clock (NominalDiffTime)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    choice,
    eof,
    many1,
    munch1,
    option,
    readP_to_S,
    string,
  )
import Text.Read (readMaybe)

-- | Parse duration strings like "1s", "2m", "10m77s", "1h30m", "500ms", "0.5s", "0", ""
parseDuration :: String -> Either String NominalDiffTime
parseDuration "" = Right 0
parseDuration "0" = Right 0
parseDuration s =
  case [val | (val, "") <- readP_to_S (durationP <* eof) s] of
    (val : _) -> Right val
    [] -> Left ("Invalid duration " <> s)
  where
    durationP :: ReadP NominalDiffTime
    durationP = sum <$> many1 componentP

    componentP :: ReadP NominalDiffTime
    componentP = do
      digits <- munch1 isDigit
      mFrac <- option "" (char '.' >> munch1 isDigit)
      let n :: NominalDiffTime
          n = case mFrac of
            "" -> fromInteger (read digits)
            f -> case readMaybe (digits ++ "." ++ f) of
              Just (d :: Double) -> realToFrac d
              Nothing -> 0
      unit <-
        choice
          [ 3600 <$ char 'h',
            0.001 <$ string "ms",
            60 <$ char 'm',
            1 <$ char 's'
          ]
      pure (n * unit)
