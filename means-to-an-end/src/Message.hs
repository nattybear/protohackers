{-# LANGUAGE OverloadedStrings #-}

module Message where

import           Data.Binary
import           Data.Binary.Get
import           Data.Int

data Message
  = Insert { timestamp :: Int32, price   :: Int32 }
  | Query  { mintime   :: Int32, maxtime :: Int32 }
  | Undefined
  deriving Show

instance Binary Message where
  put = undefined

  get = do
    t <- getByteString 1
    a <- getInt32be
    b <- getInt32be
    case t of
      "I" -> return (Insert a b)
      "Q" -> return (Query a b)

mean :: [(Int32, Int64)] -> Int32 -> Int32 -> Int32
mean [] _ _ = 0
mean samples mintime maxtime
  | mintime > maxtime = 0
  | otherwise =
      let
        samples' = filter (\(t, _) -> mintime <= t && t <= maxtime) samples
        total = foldr (\(_, p) acc -> p + acc) 0 samples'
        count = fromIntegral (length samples')
      in
        if count == 0
        then 0
        else fromIntegral (total `div` count)
