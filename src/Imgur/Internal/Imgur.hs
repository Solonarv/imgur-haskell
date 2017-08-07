{-# LANGUAGE
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverloadedStrings,
    TypeFamilies,
    DeriveFunctor
    #-}

module Imgur.Internal.Imgur where

import Control.Monad.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Sess

newtype ImgurT m a = ImgurT {_runImgurT :: ImgurConfig -> Session -> m a} deriving (Functor)
type Imgur = ImgurT IO

instance Monad m => Applicative (ImgurT m) where
    pure x = ImgurT $ \cfg sess -> pure x
    (<*>) = ap
instance Monad m => Monad (ImgurT m) where
    return = pure
    ImgurT act >>= k = ImgurT $ \cfg sess -> act cfg sess >>= (\it -> _runImgurT it cfg sess) . k
instance Monad m => MonadReader ImgurConfig (ImgurT m) where
    ask = ImgurT $ \cfg sess -> pure cfg
    local f act = ImgurT $ \cfg sess -> _runImgurT act (f cfg) sess
instance MonadTrans ImgurT where
    lift m = ImgurT $ \_ _ -> m
instance MonadIO m => MonadIO (ImgurT m) where
    liftIO io = ImgurT $ \_ _ -> liftIO io
instance MonadBase IO m => MonadBase IO (ImgurT m) where
    liftBase io = ImgurT $ \_ _ -> liftBase io
instance MonadBaseControl IO m => MonadBaseControl IO (ImgurT m) where
    type StM (ImgurT m) a = StM m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
instance MonadTransControl ImgurT where
    type StT ImgurT a = a
    liftWith f = ImgurT $ \cfg sess -> f $ \act -> _runImgurT act cfg sess
    restoreT = lift

class Monad m => MonadSession m where
    getSession :: m Session
instance Monad m => MonadSession (ImgurT m) where
    getSession = ImgurT $ \cfg sess -> pure sess

runImgurT :: MonadBaseControl IO m => ImgurConfig -> ImgurT m a -> m a
runImgurT cfg (ImgurT act) = liftBaseOp Sess.withSession $ \sess -> act cfg sess

data ImgurConfig = ImgurConfig {
    cfgCredentials :: Credentials,
    cfgClientKey :: ClientKey
    }

data Credentials = Anonymous

data ClientKey = ClientKey {
    clientID :: ByteString,
    clientSecret :: ByteString
    }