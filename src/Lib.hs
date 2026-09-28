module Lib
  ( someFunc,
    parseDuration,
  )
where

import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Parse value like 1s or 5m or 5m1s
parseDuration :: String -> Either String Int
parseDuration "" = Right 0
parseDuration s =
  let (numStr, rest) = span isDigit s
   in case (readMaybe numStr, listToMaybe rest) of
        (Nothing, _) -> Left ("Invalid duration " <> s)
        (_, Nothing) -> Left ("Invalid duration " <> s)
        (Just num, Just u) ->
          let under = parseDuration (drop 1 rest)
           in case (under, u) of
                (Left _, _) -> under
                (Right underSum, 'm') -> Right $ underSum + num * 60
                (Right underSum, 's') -> Right $ underSum + num
                _ -> Left ("Invalid duration " <> s)
