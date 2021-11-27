{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AoC.Input
  ( getInput,
  )
where

import AoC (aocYear)
import Control.Applicative (liftA2, (<|>))
import Control.Exception
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.CaseInsensitive (mk)
import Data.Char (isDigit)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory (createDirectoryIfMissing)

isValid :: ByteString -> Bool
isValid bs =
  not $
    C8.isPrefixOf "Please don't repeatedly request this endpoint before it unlocks!" bs
      || C8.isPrefixOf "Puzzle inputs differ by user." bs

getInput :: String -> IO (Maybe String)
getInput dayStr = liftA2 (<|>) fromCache fromWeb
  where
    dayNumber :: Int
    dayNumber = read $ filter isDigit dayStr

    cacheFp :: Show a => a -> String
    cacheFp ms = ".cache/input-" <> show ms <> ".txt"

    adress :: String
    adress =
      "http://adventofcode.com/"
        <> show aocYear
        <> "/day/"
        <> show dayNumber
        <> "/input"

    fromWeb :: IO (Maybe String)
    fromWeb = do
      sessionKey <- fmap (head . lines) . readFile $ "sessionKey.txt"
      initRequest <- parseRequest adress
      let session' = "session=" <> sessionKey
          req =
            initRequest
              { requestHeaders = [(mk $ C8.pack "Cookie", C8.pack session')]
              }
      manager <- newTlsManager
      input <- C8.concat <$> withResponse req manager (brConsume . responseBody)

      if isValid input
        then do
          createDirectoryIfMissing False ".cache"
          C8.writeFile (cacheFp dayNumber) input
          pure $ Just $ C8.unpack input
        else pure Nothing

    fromCache :: IO (Maybe String)
    fromCache = do
      (Just . C8.unpack <$> C8.readFile (cacheFp dayNumber)) `catch` \(_ :: IOException) -> pure Nothing
