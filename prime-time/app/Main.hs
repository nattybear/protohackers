{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy  as L
import           Data.Prime
import           Network.Socket
import           Request
import           Response
import           System.IO

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  bind sock (SockAddrInet 4000 0)
  listen sock 5
  session sock

session :: Socket -> IO ()
session sock = do
  (conn, _) <- accept sock
  handle <- socketToHandle conn ReadWriteMode
  _ <- forkIO (process handle)
  session sock

process :: Handle -> IO ()
process handle = do
  result <- hIsEOF handle
  case result of
    True -> hClose handle
    False -> do
      line <- C.hGetLine handle
      case decode (C.fromStrict line) of
        Nothing -> do
          C.hPut handle line
          hClose handle
        Just (Request _ n) -> do
          let response = Response "isPrime" (isPrime n)
          L.hPut handle (encode response <> "\n")
          process handle
