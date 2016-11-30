module Imgur.Types.Gallery where

import Imgur.Types.Atoms
import Imgur.Types.Image

import Data.Text

data CustomGallery = CustomGallery {
    cgCreatorName       :: UserName,
    cgURL               :: Text,
    cgTags              :: [Text],
    cgItemCount         :: Integer,
    cgItems             :: [GalleryItem]
}

type GalleryItem = Either GalleryAlbum GalleryImage

data GalleryAlbum = GalleryAlbum {
    gaID           :: AlbumID,
    gaTitle        :: Text,
    gaDescription  :: Text,
    gaUploadTime   :: Timestamp,
    gaCoverImage   :: CoverImage,
    gaUploader     :: Maybe User,
    gaPrivacy      :: Privacy,
    gaLayout       :: ViewLayout,
    gaViewCount    :: Integer,
    gaURL          :: Text,
    gaFavorited    :: Bool,
    gaNSFW         :: Maybe Bool,
    gaSection      :: Maybe Section,
    gaOrder        :: Int,
    gaDeleteHash   :: Maybe Text,
    gaInGallery    :: Bool,
    gaLink         :: Text,
    gaPoints       :: Points,
    gaScore        :: Int,
    gaIsAlbum      :: Bool,
    gaVote         :: Maybe Bool,
    gaCommentCount :: Int,
    gaTopic        :: Topic,
    gaImageCount   :: Int,
    gaImages       :: [Image]
}

data GalleryImage = GalleryImage -- TODO stub

data Comment = Comment {
    commentID           :: CommentID,
    commentImage        :: ImageID,
    commentBody         :: Text,
    commentAuthor       :: User,
    commentOnAlbum      :: Bool,
    commentAlbumCover   :: Maybe ImageID,
    commentPoints       :: Points,
    commentCreated      :: Timestamp,
    commentParent       :: Maybe CommentID,
    commendDeleted      :: Bool,
    commentVote         :: Maybe Bool,
    commentChildren     :: [Comment]
}
