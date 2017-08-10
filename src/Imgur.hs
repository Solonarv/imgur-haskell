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