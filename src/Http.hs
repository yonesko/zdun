{-# LANGUAGE ScopedTypeVariables #-}

module Http
  ( checkHttp,
    parseHttpTarget,
  )
where

import Control.Exception (displayException, try)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client
  ( HttpException (HttpExceptionRequest, InvalidUrlException),
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
import System.IO (hPutStrLn, stderr)
import Text.Regex.TDFA ((=~))

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

-- | Единый метод для всех HTTP проверок:
-- 1. Если передано "url" — проверяет статус 200 OK (тело не читается).
-- 2. Если передано "regex@url" — проверяет статус 200 OK и совпадение всего тела ответа с регуляркой.
checkHttp :: String -> IO Bool
checkHttp rawTarget = do
  let (mRegex, url) = parseHttpTarget rawTarget
  manager <- newManager tlsManagerSettings

  -- Ловим только сетевые/HTTP ошибки, НЕ перехватывая асинхронный Timeout и Ctrl+C!
  mReq <- try (parseRequest url) :: IO (Either HttpException Request)
  case mReq of
    Left err -> do
      hPutStrLn stderr $ "[zdun] Invalid URL: " ++ url ++ " (" ++ displayException err ++ ")"
      pure False
    Right initialReq -> do
      let req =
            initialReq
              { method = "GET",
                responseTimeout = responseTimeoutMicro (2 * 1000000)
              }
      res <-
        try
          ( withResponse req manager $ \resp -> do
              let code = statusCode (responseStatus resp)
              if code /= 200
                then do
                  hPutStrLn stderr $ "[zdun] " ++ url ++ " returned status " ++ show code ++ " (expected 200)"
                  pure False
                else case mRegex of
                  -- Вариант 1: регулярка не указана — 200 OK достаточно, тело не качаем
                  Nothing -> pure True
                  -- Вариант 2: регулярка указана — читаем весь ответ и проверяем
                  Just regexPat -> do
                    chunks <- brConsume (responseBody resp)
                    let bodyText = T.unpack (TE.decodeUtf8With TE.lenientDecode (BS.concat chunks))
                    let matched = (bodyText =~ regexPat) :: Bool
                    if matched
                      then pure True
                      else do
                        hPutStrLn stderr $ "[zdun] " ++ url ++ " body did not match regex: " ++ regexPat
                        pure False
          ) ::
          IO (Either HttpException Bool)

      case res of
        Left (HttpExceptionRequest _ content) -> do
          hPutStrLn stderr $ "[zdun] HTTP error for " ++ url ++ ": " ++ unwords (lines (show content))
          pure False
        Left (InvalidUrlException _ reason) -> do
          hPutStrLn stderr $ "[zdun] HTTP error for " ++ url ++ ": " ++ reason
          pure False
        Right ok -> pure ok