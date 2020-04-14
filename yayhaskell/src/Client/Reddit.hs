module Client.Reddit where

import Chronos (Time(..))
import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.Proxy
import GHC.Generics (Generic)
import Data.Text (Text)
import Servant.Client (ClientM, BaseUrl(..), Scheme(..))
import Servant.API (Header, JSON, Get, Post, ReqBody, QueryParam, (:>), (:<|>))
import Servant.API.BasicAuth (BasicAuth, BasicAuthData(..))

import qualified Data.ByteString.Internal as ByteString
import qualified Data.Text    as Text
import qualified Data.Text.IO as File
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Servant.Client as Client
import qualified Common
-- import qualified Servant.API.BasicAuth as Auth

-----------------
-- REDDIT AUTH --
-----------------

-- TODO: Remove "Reddit" from the beginning of names.

type RedditAuthApi = BasicAuth "" () :> "api" :> "v1" :> "access_token" :> QueryParam "grant_type" Text :> Post '[JSON] RedditAuth

data RedditAuth = RedditAuth
  { access_token :: Text,
    token_type   :: Text,
    expires_in   :: Int,
    scope        :: Text
  } deriving (Eq, Show, Generic, FromJSON, ToJSON)

data RedditError = AuthErr Text
  deriving (Eq, Show)

redditAuthApi :: Proxy RedditAuthApi
redditAuthApi = Proxy

redditAuthHandler :: BasicAuthData -> Maybe Text -> ClientM RedditAuth
redditAuthHandler = Client.client redditAuthApi

getRedditAuth :: IO (Either RedditError RedditAuth)
getRedditAuth = do
  text <- Text.lines <$> File.readFile "secret.txt"

  let user:pass:[] = ByteString.packChars . Text.unpack <$> text
  let basicAuthData = BasicAuthData user pass
  let grantType = Just "client_credentials"
  let baseUrl = BaseUrl Https "www.reddit.com" 443 ""

  manager <- TLS.newTlsManager
  res <- Client.runClientM (redditAuthHandler basicAuthData grantType) (Client.mkClientEnv manager baseUrl)

  case res of
    Left authErr  -> pure . Left . AuthErr . Text.pack . show $ authErr
    Right authObj -> pure . Right $ authObj

----------------
-- REDDIT API --
----------------

data Thing = Thing
  { _kind :: Text
  , _data :: ThingData
  } deriving (Eq, Show, Generic)

instance ToJSON Thing where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Thing where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

-- TODO: Making this into a sum type seems to mess the parsing of
-- the result up. ThingData can be may different values.
data ThingData = Link
  { _id           :: Text
  , _author       :: Text
  -- TODO: Add method for getting UTC time
  , _created_utc  :: Time
  , _num_comments :: Int
  , _permalink    :: Text
  , _score        :: Int
  , _thumbnail    :: Text
  , _title        :: Text
  , _url          :: Text
  , _stickied     :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON ThingData where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ThingData where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Listing = Listing
  { _kind :: Text
  , _data :: ListingData
  } deriving (Eq, Show, Generic)

instance ToJSON Listing where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Listing where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data ListingData = ListingData
  { _modhash  :: Text
  , _dist     :: Int
  , _children :: [Thing]
  , _after    :: Maybe Text
  , _before   :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON ListingData where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON ListingData where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

type RedditApi = "r"
               :> "haskell"
               :> "hot"
               :> Header "Authorization" Text
               :> Header "User-Agent" Text
               :> QueryParam "limit" Int
               :> Get '[JSON] Listing

redditApi :: Proxy RedditApi
redditApi = Proxy

listing :: Maybe Text -> Maybe Text -> Maybe Int -> ClientM Listing

listing = Client.client redditApi

getHaskellHot :: HTTP.Manager -> IO [Common.Post]
getHaskellHot man = do
  eitherAuth <- getRedditAuth

  case eitherAuth of
    Left authErr  -> do
      print authErr
      pure []
    Right authObj -> do

      let
        accessToken = Just $ "bearer " <> access_token authObj
        bearer = Just "yayhaskell"
        limit = Just 10
        baseUrl = BaseUrl Https "oauth.reddit.com" 443 ""

      -- manager <- TLS.newTlsManager
      res <- Client.runClientM
        (listing accessToken bearer limit)
        (Client.mkClientEnv man baseUrl)

      case res of
        Left err -> do
          print err
          pure []
        Right res -> pure $ listingToPosts res

listingToPosts :: Listing -> [Common.Post]
listingToPosts Listing{..} =
  case _data of
    ListingData{..} -> map thingToPost _children

-- TODO: Figure out how to work on generting ids for reddit posts
thingToPost :: Thing -> Common.Post
thingToPost Thing{..} = case _data of
  Link{..} -> Common.Post
      (Common.Id 0)
      _created_utc
      Common.Reddit
      _title
      (Common.Url _url)
      (Common.Url $ "https://www.reddit.com" <> _permalink)
      (Common.Lambdas 0)
