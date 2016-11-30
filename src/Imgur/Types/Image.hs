module Imgur.Types.Image (module Imgur.Types.Image) where

import Imgur.Types.Atoms

import Data.Text

data Image = Image {
    imgID           :: ImageID,
    imgTitle        :: Text,
    imgDescription  :: Text,
    imgUploadTime   :: Timestamp,
    imgMIME         :: MIME,
    imgAnimated     :: Bool,
    imgWidth        :: Int,
    imgHeight       :: Int,
    imgSize         :: Int,
    imgViewCount    :: Int,
    imgBandwith     :: Int,
    imgDeleteHash   :: Maybe DeleteHash,
    imgOrigFileName :: Maybe Text,
    imgCategory     :: Maybe Section,
    imgLink         :: Text,
    imgGIFV         :: Maybe Text,
    imgMP4          :: Maybe Text,
    imgMP4Length    :: Maybe Double,
    imgLooping      :: Maybe Bool,
    imgFavorited    :: Bool,
    imgNSFW         :: Maybe Bool,
    imgUserVote     :: Maybe Bool,
    imgInGallery    :: Bool
}

data Album = Album {
    albumID             :: AlbumID,
    albumTitle          :: Text,
    albumDescription    :: Text,
    albumUploadTime     :: Timestamp,
    albumCoverImage     :: CoverImage,
    albumUploader       :: Maybe User,
    albumPrivacy        :: Privacy,
    albumLayout         :: ViewLayout,
    albumViewCount      :: Integer,
    albumURL            :: Text,
    albumFavorited      :: Bool,
    albumNSFW           :: Maybe Bool,
    albumSection        :: Maybe Section,
    albumOrder          :: Int,
    albumDeleteHash     :: Maybe Text,
    albumImageCount     :: Int,
    albumImages         :: Maybe [Image],
    albumInGallery      :: Bool
}