module Types
  ( Env (..),
    Check (..),
    Probe (..),
    Source (..),
    parseProbe,
    formatSource,
    formatCheck,
    formatProbe,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char
import Network.HTTP.Client (Manager)
import Network.Socket (HostName, ServiceName)
import Text.ParserCombinators.ReadP
import Text.Regex.TDFA
  ( defaultCompOpt,
    defaultExecOpt,
  )
import Text.Regex.TDFA.String (compile)

data Env = Env
  { envManager :: Manager,
    envLogger :: String -> IO ()
  }

data Check = Matches String | Contains String deriving (Show, Eq)

data Probe = Probe
  { probeSource :: Source,
    probeCheck :: Maybe Check
  }
  deriving (Show, Eq)

data Source = Tcp HostName ServiceName | Http String deriving (Show, Eq)

parseProbe :: String -> Either String Probe
parseProbe s = case readP_to_S (skipSpaces *> parse <* skipSpaces <* eof) s of
  (p, _) : _ -> Right p
  [] -> Left $ "invalid probe " <> s
  where
    parse = Probe <$> parseSource <*> parseCheck

    parseSource = parseTcp <|> parseHttp

    parseCheck =
      pure Nothing
        <|> (munch1 isSpace *> (Just <$> (parseMatches <|> parseContains)))

    parseMatches = do
      re <- parseArg "matches"
      guard (isValidRegex re)
      pure (Matches re)

    parseContains = Contains <$> parseArg "contains"

    parseArg keyword = string keyword *> munch1 isSpace *> munch1 (const True)

    isValidRegex re = either (const False) (const True) (compile defaultCompOpt defaultExecOpt re)

    parseTcp = Tcp <$ string "tcp://" <*> parseHost <* char ':' <*> munch1 isDigit
      where
        parseHost = parseIpv6 <|> parseRegName
        parseIpv6 = between (char '[') (char ']') (munch1 (\c -> c /= ']' && not (isSpace c)))
        parseRegName = munch1 (\c -> c /= ':' && c /= '[' && c /= ']' && not (isSpace c))

    parseHttp = Http . fst <$> gather ((string "http://" <|> string "https://") >> munch1 (not . isSpace))

formatSource :: Source -> String
formatSource (Tcp h p)
  | ':' `elem` h = "tcp://[" <> h <> "]:" <> p
  | otherwise    = "tcp://" <> h <> ":" <> p
formatSource (Http u) = u

formatCheck :: Check -> String
formatCheck (Contains s) = " contains " <> s
formatCheck (Matches r) = " matches " <> r

formatProbe :: Probe -> String
formatProbe (Probe src Nothing) = formatSource src
formatProbe (Probe src (Just c)) = formatSource src <> formatCheck c
