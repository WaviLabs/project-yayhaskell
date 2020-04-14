module Client.HackerNews where

import Chronos
import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.Proxy
import GHC.Generics (Generic)
import Data.Text (Text)
import Servant.Client (ClientM, BaseUrl(..), Scheme(..))
import Servant.API (Header, JSON, Get, Post, ReqBody, Capture, QueryParam, (:>), (:<|>)(..))
import Servant.API.BasicAuth (BasicAuth, BasicAuthData(..))

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Internal as ByteString
import qualified Data.Text    as Text
import qualified Data.Text.IO as File
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Servant.Client as Client
import qualified Common


----------------
-- REDDIT API --
----------------

-- Items can be other things as well, but we are only interested in stories.
data Hit = Hit
  { _objectID   :: Text
  , _created_at :: Text
  , _title      :: Text
  , _url        :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON Hit where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Hit where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Hits = Hits
  { _hits :: [Hit]
  , _processingTimeMS :: Int
  } deriving (Eq, Show, Generic)

instance ToJSON Hits where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON Hits where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

type HackerNewsApi = "api" :> "v1" :> "search_by_date" :> QueryParam "query" Text :> QueryParam "tags" Text :> Get '[JSON] Hits

hackerNewsApi :: Proxy HackerNewsApi
hackerNewsApi = Proxy

searchByDate :: Maybe Text -> Maybe Text -> ClientM Hits

searchByDate = Client.client hackerNewsApi

getHaskellStories :: HTTP.Manager -> IO [Common.Post]
getHaskellStories man = do
  let
    baseUrl = BaseUrl Http "hn.algolia.com" 80 ""
    query = Just "Haskell"
    tags  = Just "story"

  -- manager <- TLS.newTlsManager
  res <- Client.runClientM (searchByDate query tags) (Client.mkClientEnv man baseUrl)

  case res of
    Left err -> do
      print err
      pure []
    Right Hits{..} -> pure $ map hitToPost _hits

-- TODO: Force compiler to throw errors when fields are missing
hitToPost :: Hit -> Common.Post
hitToPost Hit{..} = Common.Post{..} where
  postUserId     = Common.Id 0
  -- TODO: Define a currentTime
  currentTime    = undefined
  datetimeFormat = DatetimeFormat (Just '-') (Just 'T') (Just ':')
  postCreatedAt  = maybe currentTime datetimeToTime (decode_YmdHMS datetimeFormat _created_at)
  postAuthor     = Nothing
  postSource     = Common.HackerNews
  postTitle      = _title

  postLink       = Common.Url $
    case _url of
      Nothing  -> "https://news.ycombinator.com/item?id=" <> _objectID
      Just url -> url

  postCommentsLink = Common.Url $ "https://news.ycombinator.com/item?id=" <> _objectID
  postLambdas = Common.Lambdas 0
