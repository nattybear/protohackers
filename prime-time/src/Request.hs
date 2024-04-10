{-# LANGUAGE OverloadedStrings #-}

module Request where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Text           (Text)

data Request = Request
  { method :: Text
  , number :: Int
  } deriving Show

instance FromJSON Request where
  parseJSON (Object v) = do
    String method <- v .: "method"
    Number number <- v .: "number"
    guard (method == "isPrime")
    number' <- parseJSON (Number number) <|> return (-1)
    return (Request method number')

  parseJSON _ = empty
