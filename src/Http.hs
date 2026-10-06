module Http
  ( checkHttp,
  )
where

import Control.Exception (IOException, displayException, fromException, try)
import Control.Monad (forM_)
import Control.Monad.Except (liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import GHC.IO.Exception (ioe_description)
import Network.HTTP.Client
  ( BodyReader,
    HttpException (..),
    HttpExceptionContent
      ( ConnectionClosed,
        ConnectionFailure,
        ConnectionTimeout,
        InternalException,
        InvalidHeader,
        InvalidStatusLine,
        NoResponseDataReceived,
        ResponseTimeout,
        StatusCodeException,
        TlsNotSupported
      ),
    Request (responseTimeout),
    Response (responseBody, responseStatus),
    brConsume,
    parseRequest,
    responseTimeoutMicro,
    withResponse,
  )
import Network.HTTP.Types
  ( Status (statusCode),
    statusIsSuccessful,
  )
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()
import Types

checkHttp :: Env -> String -> Maybe Check -> IO (Either String ())
checkHttp env url check = do
  res <- try $ do
    initialReq <- parseRequest url
    let request = initialReq {responseTimeout = responseTimeoutMicro (seconds 2)}
    withResponse request (envManager env) $ \response -> runExceptT $ do
      liftEither $ checkStatus response
      forM_ check $ \c -> do
        body <- liftIO $ readBody response
        liftEither $ checkContent c body
  pure $ either (Left . shortHttpError) id res
  where
    checkStatus :: Response a -> Either String ()
    checkStatus resp
      | statusIsSuccessful st = Right ()
      | otherwise = Left $ "response status is not successful: " <> show (statusCode st)
      where
        st = responseStatus resp

    checkContent :: Check -> T.Text -> Either String ()
    checkContent (Contains substr) body
      | T.pack substr `T.isInfixOf` body = Right ()
      | otherwise = Left "response body doesn't contain substring"
    checkContent (Matches re) body
      | body =~ re = Right ()
      | otherwise = Left "response body doesn't match re"

    readBody :: Response BodyReader -> IO T.Text
    readBody = fmap (TE.decodeUtf8With TE.lenientDecode . mconcat) . brConsume . responseBody

seconds :: Int -> Int
seconds n = n * 1000000

-- | Extracts a concise, human-readable error description from an IOException.
shortSocketError :: IOException -> String
shortSocketError err =
  case ioe_description err of
    "" -> unwords . lines . displayException $ err
    desc -> desc

-- | Extracts a concise, single-line error description from an HttpException.
shortHttpError :: HttpException -> String
shortHttpError (InvalidUrlException _ reason) = "Invalid URL: " <> reason
shortHttpError (HttpExceptionRequest _ content) = case content of
  StatusCodeException resp _ -> "HTTP status " <> show (statusCode $ responseStatus resp)
  ResponseTimeout -> "Response timeout"
  ConnectionTimeout -> "Connection timeout"
  ConnectionFailure e -> handleSomeException e
  ConnectionClosed -> "Connection closed"
  InvalidStatusLine bs -> "Invalid status line: " <> show bs
  InvalidHeader bs -> "Invalid header: " <> show bs
  InternalException e -> handleSomeException e
  NoResponseDataReceived -> "No response data received"
  TlsNotSupported -> "TLS not supported"
  other -> unwords . lines . show $ other
  where
    handleSomeException e = case fromException e of
      Just (ioe :: IOException) -> shortSocketError ioe
      Nothing -> unwords . lines . displayException $ e