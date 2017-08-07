{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverloadedStrings,
    TypeFamilies    
    #-}
module Imgur where

import Control.Lens
import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Monoid ((<>))

import Network.Wreq
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Sess

import Imgur.Internal.Imgur

type ID = Text

data Image = Image {
    imgID :: ID
    }

getImage :: {-(MonadIO m, MonadReader ImgurConfig m, MonadReader Session m{) => -}ID -> ImgurT IO Image
getImage iid = do
    sess <- getSession
    opts <- getOpts
    url <- apiEndpoint "image" iid
    liftIO $ Sess.getWith opts sess url >>= print
    pure $ Image iid

getOpts :: MonadReader ImgurConfig m => m Options
getOpts = do
    cfg <- ask
    pure $ defaults & header "Authorization" .~ ["Client-ID " <> clientID (cfgClientKey cfg)]

type URL = String
apiEndpoint :: MonadReader ImgurConfig m => Text -> Text -> m URL
apiEndpoint endpoint query = return $ Text.unpack $ "https://api.imgur.com/3/" <> endpoint <> "/" <> query