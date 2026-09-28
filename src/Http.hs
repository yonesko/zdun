{-# LANGUAGE ScopedTypeVariables #-}

module Http
  ( isHttpOk,
    isHttpMatch,
    checkHttp,
    parseHttpTarget,
  )
where

import Control.Exception (SomeException, displayException, try)
import qualified Data.ByteString.Lazy as L
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Client
  ( Request (method, responseTimeout),
    Response (responseStatus),
    brReadSome,
    httpNoBody,
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
        _                             -> (Nothing, raw)
      normalizedUrl
        | "http://" `isPrefixOf` urlPart || "https://" `isPrefixOf` urlPart = urlPart
        | otherwise = "http://" ++ urlPart
   in (mRe, normalizedUrl)

-- | Вариант 1: Просто проверяет, что HTTP-ответ вернул статус 200 OK.
-- Тело ответа вообще не скачивается (httpNoBody).
isHttpOk :: String -> IO Bool
isHttpOk rawUrl = do
  let (_, url) = parseHttpTarget rawUrl
  manager <- newManager tlsManagerSettings
  mReq <- try (parseRequest url) :: IO (Either SomeException Request)
  case mReq of
    Left err -> do
      hPutStrLn stderr $ "[zdun] Invalid URL: " ++ url ++ " (" ++ displayException err ++ ")"
      pure False
    Right initialReq -> do
      let req = initialReq
            { method = "GET",
              responseTimeout = responseTimeoutMicro (2 * 1000000)
            }
      res <- try (httpNoBody req manager) :: IO (Either SomeException (Response ()))
      case res of
        Left err -> do
          hPutStrLn stderr $ "[zdun] HTTP error for " ++ url ++ ": " ++ displayException err
          pure False
        Right resp -> do
          let code = statusCode (responseStatus resp)
          if code == 200
            then pure True
            else do
              hPutStrLn stderr $ "[zdun] " ++ url ++ " returned status " ++ show code ++ " (expected 200)"
              pure False

-- | Вариант 2: Проверяет, что статус 200 OK И тело ответа матчится с регулярным выражением.
-- Скачивает не больше 64 КБ и декодирует UTF-8.
isHttpMatch :: String -> String -> IO Bool
isHttpMatch regexPat rawUrl = do
  let (_, url) = parseHttpTarget rawUrl
  manager <- newManager tlsManagerSettings
  mReq <- try (parseRequest url) :: IO (Either SomeException Request)
  case mReq of
    Left err -> do
      hPutStrLn stderr $ "[zdun] Invalid URL: " ++ url ++ " (" ++ displayException err ++ ")"
      pure False
    Right initialReq -> do
      let req = initialReq
            { method = "GET",
              responseTimeout = responseTimeoutMicro (2 * 1000000)
            }
      -- withResponse открывает поток и гарантированно закрывает сокет при выходе
      res <- try (withResponse req manager $ \resp -> do
        let code = statusCode (responseStatus resp)
        if code /= 200
          then pure (Left code)
          else do
            -- Читаем максимум 64 КБ, не скачивая лишний трафик
            chunk <- brReadSome (responseBody resp) (64 * 1024)
            pure (Right chunk)
        ) :: IO (Either SomeException (Either Int L.ByteString))
      case res of
        Left err -> do
          hPutStrLn stderr $ "[zdun] HTTP error for " ++ url ++ ": " ++ displayException err
          pure False
        Right (Left code) -> do
          hPutStrLn stderr $ "[zdun] " ++ url ++ " returned status " ++ show code ++ " (expected 200)"
          pure False
        Right (Right bodyBytes) -> do
          -- Корректно декодируем UTF-8 текст
          let bodyText = T.unpack (TE.decodeUtf8With TE.lenientDecode (L.toStrict bodyBytes))
          let matched = (bodyText =~ regexPat) :: Bool
          if matched
            then pure True
            else do
              hPutStrLn stderr $ "[zdun] " ++ url ++ " body did not match regex: " ++ regexPat
              pure False

checkHttp :: String -> IO Bool
checkHttp raw =
  case parseHttpTarget raw of
    (Just re, url) -> isHttpMatch re url
    (Nothing, url) -> isHttpOk url