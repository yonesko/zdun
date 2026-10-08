module Tcp
  ( checkTcp,
  )
where

import Control.Exception (IOException, bracket, displayException, try)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.IO.Exception (ioe_description)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType),
    HostName,
    ServiceName,
    SocketType (Stream),
    close,
    connect,
    defaultHints,
    getAddrInfo,
    socket,
  )
import Network.Socket.ByteString (recv)
import System.Timeout (timeout)
import Text.Regex.TDFA ((=~))
import Types (Check (..))

shortSocketError :: IOException -> String
shortSocketError err =
  case ioe_description err of
    "" -> unwords . lines . displayException $ err
    desc -> desc

checkTcp :: HostName -> ServiceName -> Maybe Check -> IO (Either String ())
checkTcp host port check = fromMaybe timeoutMsg <$> timeout 2_000_000 checkTcp'
  where
    checkTcp' = do
      res <- try $ getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just host) (Just port)
      either (pure . Left . formatErr) run res

    run [] = pure (Left "Empty address list")
    run [a] = withAddr a
    run (a : as) = withAddr a >>= either (\_ -> run as) (pure . Right)

    withAddr addr =
      either (Left . formatErr) id
        <$> try
          ( bracket
              (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
              close
              ( \sock -> do
                  connect sock (addrAddress addr)
                  maybe (pure $ Right ()) (\c -> (`checkBody` c) <$> recv sock 4096) check
              )
          )

    checkBody body (Contains substr) = if (TE.encodeUtf8 . T.pack) substr `BS.isInfixOf` body then Right () else Left "response body doesn't contain substring"
    checkBody body (Matches re) = if body =~ re then Right () else Left "response body doesn't match re"
    timeoutMsg = Left $ "Timeout(2s) connecting to " <> formatTarget host port
    formatTarget h p
      | ':' `elem` h = "[" <> h <> "]:" <> p
      | otherwise = h <> ":" <> p

    formatErr err = formatTarget host port <> ": " <> shortSocketError err
