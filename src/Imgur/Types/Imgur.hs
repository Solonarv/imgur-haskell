{-# LANGUAGE
    GADTs,
    GeneralizedNewtypeDeriving
    #-}

module Imgur.Types.Imgur where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Free

type Imgur = ImgurT IO

data ImgurF a where
    FailWith :: String -> ImgurF a
    Request :: Route b -> (b -> a) ->  ImgurF a
    Login :: Credentials -> (Bool -> a) -> ImgurF a

instance Functor (RedditF m) where
    fmap _ (Fail err) = Fail err
    fmap f (Request r k) = Request r (f . k)
    fmap f (Login c k) = Login c (f . k)

newtype ImgurT m a = ImgurT (FreeT (ImgurF m) m a)
    deriving (Functor, Applicative, Monad)

instance MonadTrans ImgurT where
    lift = ImgurT . lift

instance MonadIO m => MonadIO (ImgurT m) where
    liftIO = RedditT . liftIO

instance MonadFail (ImgurT m) where
    fail = ImgurT . liftF . FailWith

request :: Route a -> ImgurT m a
request r = ImgurT . liftF $ Request r id

login :: Credentials -> ImgurT m Bool
login c = ImgurT . liftF $ Login c id
