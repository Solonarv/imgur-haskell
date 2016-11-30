{-# LANGUAGE
    DeriveGeneric
    #-}

module Imgur.Types.Client where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text

import GHC.Generics

data Client = Client {
    clientID     :: Text,
    clientSecret :: Text
} deriving (Eq, Ord, Generic)

instance FromJSON Client
instance ToJSON Client


