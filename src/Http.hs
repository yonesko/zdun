{-# LANGUAGE ScopedTypeVariables #-}

module Http
  ( checkHttp,
    shortHttpError,
  )
where

import Control.Exception (IOException, displayException, fromException, try)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client
  ( HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (..),
    Request (method, responseTimeout),
    Response (responseStatus),
    brConsume,
    newManager,
    parseRequest,
    responseBody,
    responseTimeoutMicro,
    withResponse,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Probes (CheckResult (..))
import Tcp (shortSocketError)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

-- | Разбирает строку вида "regex@url" или просто "url".
parseHttpTarget :: String -> (Maybe String, String)
parseHttpTarget raw =
  let (mRe, urlPart) = case break (== '@') raw of
        (re, '@' : u) | not (null re) -> (Just re, u)
        _ -> (Nothing, raw)
      normalizedUrl
        | "http://" `isPrefixOf` urlPart || "https://" `isPrefixOf` urlPart = urlPart
        | otherwise = "http://" ++ urlPart
   in (mRe, normalizedUrl)

-- | Extracts a concise, single-line error description from an HttpException.
shortHttpError :: HttpException -> String
shortHttpError (InvalidUrlException _ reason) = "Invalid URL: " ++ reason
shortHttpError (HttpExceptionRequest _ content) = case content of
  StatusCodeException resp _ -> "HTTP status " ++ show (statusCode (responseStatus resp))
  ResponseTimeout -> "Response timeout"
  ConnectionTimeout -> "Connection timeout"
  ConnectionFailure e -> case fromException e of
    Just (ioe :: IOException) -> shortSocketError ioe
    Nothing -> unwords (lines (displayException e))
  ConnectionClosed -> "Connection closed"
  InvalidStatusLine bs -> "Invalid status line: " ++ show bs
  InvalidHeader bs -> "Invalid header: " ++ show bs
  InternalException e -> case fromException e of
    Just (ioe :: IOException) -> shortSocketError ioe
    Nothing -> unwords (lines (displayException e))
  NoResponseDataReceived -> "No response data received"
  TlsNotSupported -> "TLS not supported"
  other -> unwords (lines (show other))

-- | Единый метод для всех HTTP проверок:
-- 1. Если передано "url" — проверяет статус 200 OK (тело не читается).
-- 2. Если передано "regex@url" — проверяет статус 200 OK и совпадение всего тела ответа с регуляркой.
checkHttp :: String -> IO CheckResult
checkHttp rawTarget = do
  let (mRegex, url) = parseHttpTarget rawTarget
  manager <- newManager tlsManagerSettings

  -- Ловим только сетевые/HTTP ошибки, НЕ перехватывая асинхронный Timeout и Ctrl+C!
  mReq <- try (parseRequest url) :: IO (Either HttpException Request)
  case mReq of
    Left err -> pure $ Err (shortHttpError err)
    Right initialReq -> do
      let req =
            initialReq
              { method = "GET",
                responseTimeout = responseTimeoutMicro (seconds 2)
              }
      res <-
        try
          ( withResponse req manager $ \resp -> do
              let code = statusCode (responseStatus resp)
              if code /= 200
                then pure $ Err ("status " ++ show code ++ " (expected 200)")
                else case mRegex of
                  Nothing -> pure Ok
                  Just regexPat -> do
                    chunks <- brConsume (responseBody resp)
                    let bodyText = TE.decodeUtf8With TE.lenientDecode (BS.concat chunks)
                    let matched = (bodyText =~ regexPat) :: Bool
                    if matched
                      then pure Ok
                      else pure $ Err ("body did not match regex: " ++ regexPat)
          ) ::
          IO (Either HttpException CheckResult)

      pure $ case res of
        Left err -> Err (shortHttpError err)
        Right outcome -> outcome

seconds :: Int -> Int
seconds n = n * 1000000