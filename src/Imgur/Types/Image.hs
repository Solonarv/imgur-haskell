{-# LANGUAGE
    FlexibleContexts,
    OverloadedStrings,
    ApplicativeDo,
    ScopedTypeVariables,
    RankNTypes,
    MultiParamTypeClasses,
    DataKinds
    #-}
module Imgur.Types.Image where

import Imgur.Types.Core
import Imgur.Internal.Imgur
import Imgur.Internal.Route

import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))

import qualified Data.HashMap.Lazy as HM

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Scientific

import Control.Monad.Reader
import Control.Monad.IO.Class

import Control.Lens

import Network.Wreq
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Sess

import Data.Time.Clock.POSIX

data Image = Image {
    imgID :: ID,
    imgTitle :: Maybe Text,
    imgDescription :: Maybe Text,
    imgUploadTime :: UTCTime,
    imgType :: Text,
    imgAnimated :: Bool,
    imgDimensions :: (Int, Int),
    imgSize :: Int,
    imgViews :: Int,
    imgBandwidth :: Int,
    imgVote :: Maybe Vote,
    imgFavorited :: Bool,
    imgNSFW :: Bool,
    --imgSection :: Section,
    imgAccount :: Maybe (URL, Int),
    imgAdvertisement :: Maybe Advertisement,
    imgInMostViral :: Bool,
    imgTags :: [Tag],
    imgInGallery :: Bool,
    imgDeleteHash :: Maybe ID,
    imgName :: Maybe Text,
    imgDirectLink :: URL
    } deriving (Show, Eq)

instance FromJSON Image where
    parseJSON = withObject "Image" $ \v -> Image
        <$> v .: "id"
        <*> v .: "title"
        <*> v .: "description"
        <*> do
            n <- v .: "datetime"
            floatingOrInteger n & either (const $ typeMismatch "Datetime" (v HM.! "datetime")) (pure . posixSecondsToUTCTime . fromInteger)
        <*> v .: "type"
        <*> v .: "animated"
        <*> ((,) <$> v .: "width" <*> v .: "height")
        <*> v .: "size"
        <*> v .: "views"
        <*> v .: "bandwidth"
        <*> v .: "vote"
        <*> v .: "favorite"
        <*> v .: "nsfw"
        -- <*> v .: "section"
        <*> do
            url <- v .:? "account_url"
            aid <- v .:? "account_id"
            pure $ (,) <$> url <*> aid
        <*> do
            isad <- v .: "is_ad"
            adtype <- v .:? "ad_type"
            adurl <- v .:? "ad_url"
            pure $ if isad then Advertisement <$> adtype <*> adurl else Nothing
        <*> v .: "in_most_viral"
        <*> v .: "tags"
        <*> v .: "in_gallery"
        <*> v .:? "deletehash"
        <*> v .:? "name"
        <*> v .: "link"

data GetImage = GetImage ID
instance Request GetImage GET Image where
    reqRoute (GetImage iid) = "image/" <> iid

getImage :: (MonadIO m, MonadReader ImgurConfig m, MonadSession m) => ID -> m (APIResponse Image)
getImage = fmap (view responseBody) . queryEndpoint . GetImage