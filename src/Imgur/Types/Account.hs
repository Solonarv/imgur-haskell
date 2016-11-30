module Imgur.Types.Account (module Imgur.Types.Account) where

import Imgur.Types.Atoms

import Data.Text

data Account = Account {
    accountUser    :: User,
    accountBio     :: Text,
    accountRep     :: Double,
    accountCreated :: Timestamp,
    accountPro     :: Maybe Timestamp
}

data AccountSettings = AccountSettings {
    settingsAccount      :: UserName,
    settingsEmail        :: Email,
    settingsHQCapable    :: Bool,
    settingsDefPrivacy   :: Privacy,
    settingsPro          :: Maybe Timestamp,
    settingsTOUAccepted  :: Bool,
    settingsActiveEmails :: [Email],
    settingsMsgEnabled   :: Bool,
    settingsBlockedUsers :: [User],
    settingsShowMature   :: Bool
}
data Conversation = Conversation {
    conversID           :: Int,
    conversLastMsgPrev  :: Text,
    conversLastMsgTime  :: Timestamp,
    conversPartner      :: User,
    conversMsgCount     :: Int,
    conversMessages     :: Maybe [Message],
    conversDone         :: Maybe Bool,
    conversNextPage     :: Maybe Int
}

data Message = Message {
    msgID       :: Int,
    msgSender   :: UserName,
    msgReceiver :: User,
    msgBody     :: Text,
    msgConvers  :: Int,
    msgTime     :: Timestamp
}