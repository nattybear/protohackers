module Main where

import           Control.Concurrent
import           Data.Binary
import qualified Data.ByteString.Lazy as L
import           Data.Int
import           Message
import           Network.Socket
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
  _ <- forkIO (process handle [])
  session sock

process :: Handle -> [(Int32,Int64)] -> IO ()
process handle samples = do
  line <- L.hGet handle 9
  let message = decode line
  case message of
    Insert time price -> do
      let samples' = (time, fromIntegral price) : samples
      process handle samples'
    Query mintime maxtime -> do
      let result = mean samples mintime maxtime
      L.hPut handle (encode result)
      process handle samples
    Undefined ->
      process handle samples
