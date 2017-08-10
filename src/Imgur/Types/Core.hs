module Imgur.Types.Core (
    module Imgur.Types.Core,
    module Data.Time.Clock
    ) where

import Data.Time.Clock (UTCTime)

import Data.Aeson
import Data.Text (Text)

type ID = Text

data Vote = Upvote | Downvote deriving (Show, Eq)

instance FromJSON Vote where
    parseJSON _ = pure Upvote -- dummy implementation

type URL = Text

type Tag = Text

data Advertisement = Advertisement {
    adType :: Int,
    adURL :: URL
    } deriving (Show, Eq)