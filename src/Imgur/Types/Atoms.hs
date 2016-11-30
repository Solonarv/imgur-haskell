module Imgur.Types.Atoms (module Imgur.Types.Atoms) where

import Data.Text

type UserID = Int
type UserName = Text
type User = (UserID, UserName)

type Email = Text

type Timestamp = Integer

data Privacy = Private | Hidden | Public

type ImageID = Text

type CommentID = Text

data ViewLayout = ViewLayout

data Points = Points {
    pointsUp :: Int,
    pointsDn :: Int,
    pointsUserVote :: Maybe Bool
}

type AlbumID = Text
type Section = Text

data Topic = Topic

data CoverImage = CoverImage {
    coverimgID          :: ImageID,
    coverimgWidth       :: Int,
    coverimgHeight      :: Int
}

type DeleteHash = Text

data MIME = MIME -- TODO stub