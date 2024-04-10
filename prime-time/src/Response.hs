{-# LANGUAGE DeriveGeneric #-}

module Response where

import           Data.Aeson
import           GHC.Generics

data Response = Response
  { method :: String
  , prime  :: Bool
  } deriving (Generic, Show)

instance ToJSON Response
