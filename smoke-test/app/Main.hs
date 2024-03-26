module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Network.Socket
import           System.IO

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  bind sock (SockAddrInet 4000 0)
  listen sock 5
  forever (session sock)

session :: Socket -> IO ()
session sock = do
  (conn, _) <- accept sock
  handle <- socketToHandle conn ReadWriteMode
  contents <- B.hGetContents handle
  B.hPut handle contents
