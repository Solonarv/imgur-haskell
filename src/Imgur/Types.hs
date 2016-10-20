module Imgur.Types where

data Client = Client {
    clientID     :: Text,
    clientSecret :: Text
}

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

type UserID = Int
type UserName = Text
type User = (UserID, UserName)

type Email = Text

type Timestamp = Integer

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
    albumSection        :: Maybe Section
    albumOrder          :: Int,
    albumDeleteHash     :: Maybe Text,
    albumImageCount     :: Int,
    albumImages         :: Maybe [Image]
    albumInGallery      :: Bool
}

type AlbumID = Text
type Section = Text

data CoverImage = CoverImage {
    coverimgID          :: ImageID,
    coverimgWidth       :: Int,
    coverimgHeight      :: Int
}

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

data Conversation = Conversation {
    conversID           :: Int,
    conversLastMsgPrev  :: Text,
    conversLastMsgTime  :: Timestamp,
    conversPartner      :: User,
    conversMsgCount     :: Int,
    conversMessages     :: Maybe [Message]
    conversDone         :: Maybe Bool,
    conversNextPage     :: Maybe Int,
}

data Message = Message {
    msgID       :: Int,
    msgSender   :: UserName,
    msgReceiver :: User,
    msgBody     :: Text,
    msgConvers  :: Int,
    msgTime     :: Timestamp
}

data CustomGallery = CustomGallery {
    cgCreatorName       :: UserName,
    cgURL               :: Text,
    cgTags              :: [Text],
    cgItemCount         :: Integer,
    cgItems             :: [GalleryItem]
}

type GalleryItem = Either GalleryAlbum GalleryImage

data GalleryAlbum = GalleryAlbum {
    gaAlbum     :: Album,
    gaLink      :: Text,
    gaPoints    :: Points,
    gaScore     :: Int,
    gaIsAlbum   :: Bool,
    gaVote      :: Maybe Bool,
    gaCommentCount :: Int,
    gaTopic     :: Topic,
    gaImageCount :: Int,
    gaImages    :: [Image]
}
