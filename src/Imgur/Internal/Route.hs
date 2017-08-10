{-# LANGUAGE
    TypeFamilies,
    FlexibleContexts,
    MultiParamTypeClasses,
    FunctionalDependencies,
    OverloadedStrings,
    DataKinds,
    DefaultSignatures
    #-}
module Imgur.Internal.Route where

import Imgur.Types.Core
import Imgur.Internal.Imgur

import Network.Wreq
import Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as Sess

import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text as T
import Data.Semigroup ((<>))

import Data.Aeson

-- | Constructs the base API URL from the environment. Free applications use "https://api.imgur.com/3/", commercial ones
--   use "https://imgur-apiv3.p.mashape.com/". Currently always returns the free endpoint.
apiBaseURL :: MonadReader ImgurConfig m => m URL
apiBaseURL = return $ "https://api.imgur.com/3/"

-- | Enumeration of HTTP verbs, only contains verbs used by the imgur API.
data Verb = GET | DELETE | POST | PUT

type family HasPayload (v :: Verb) (p :: *) :: * where
    HasPayload GET p = ()
    HasPayload DELETE p = ()
    HasPayload POST p = p
    HasPayload PUT p = p

-- | The type of requests to the imgur API. If there is an instance @'Request' r a p@, then @r@
--   is the type of API requests that sends a payload of type @p@ and return an @a@, using the HTTP verb @v@.
class Request r (v :: Verb) a p | r -> v a p where
    --- | Constructs a URL (technically, a path) out of the given request.
    reqRoute :: r -> Text
    -- | Extract the payload from a request. Note that this method is pure; if you want to e.g. upload a file,
    -- reading the file must happen in your application code.
    reqPayload :: r -> HasPayload v p
    default reqPayload :: r -> ()
    reqPayload _ = ()


-- | The type of API responses: @'APISuccess' result statuscode@ or @'APIError' errormessage statuscode@.
data APIResponse a = APISuccess a Int | APIError Text Int

instance FromJSON a => FromJSON (APIResponse a) where
    parseJSON = withObject "Response" $ \o -> do
        success <- o .: "success"
        if success
        then APISuccess <$> o .: "data" <*> o .: "status"
        else do
            dObj <- o .: "data"
            flip (withObject "ErrorReason") dObj $ \d ->
                APIError <$> d .: "error" <*> o .: "status"

class Request r v a p => VerbImpl r (v :: Verb) where
    perform :: (Request r v a p, FromJSON a, MonadReader ImgurConfig m, MonadSession m) => r -> m (Response (APIResponse a))

instance Request r v a p => VerbImpl r GET where
    perform req = do
        baseURL <- apiBaseURL
        let url = T.unpack (baseURL <> reqRoute req)
        sess <- getSession
        opts <- getOpts
        liftIO $ Sess.getWith opts sess url >>= asJSON

instance Request r v a p => VerbImpl DELETE where
    perform req = do
        baseURL <- apiBaseURL
        let url = T.unpack (baseURL <> reqRoute req)
        sess <- getSession
        opts <- getOpts
        liftIO $ Sess.deleteWith opts sess url >>= asJSON

instance (Request r v a p, Postable p) => VerbImpl POST where
    perform req = do
        baseURL <- apiBaseURL
        let url = T.unpack (baseURL <> reqRoute req)
        sess <- getSession
        opts <- getOpts
        let payload = reqPayload req
        liftIO $ Sess.postWith opts sess url payload >>= asJSON

instance (Request r v a p, Putable p) => VerbImpl PUT where
    perform req = do
        baseURL <- apiBaseURL
        let url = T.unpack (baseURL <> reqRoute req)
        sess <- getSession
        opts <- getOpts
        let payload = reqPayload req
        liftIO $ Sess.putWith opts sess url payload >>= asJSON