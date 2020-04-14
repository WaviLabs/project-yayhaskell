module Client.Arxiv where
    -- TODO: Figure out how to parse XML responses
    -- Can also get updated papers.

import Data.Aeson (FromJSON(..), ToJSON(..), defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON)
import Data.Proxy
import GHC.Generics (Generic)
import Data.Text (Text)
import Servant.Client (ClientM, BaseUrl(..), Scheme(..))
import Servant.API (Header, JSON, PlainText, Get, Post, ReqBody, Capture, QueryParam, (:>), (:<|>)(..))
import Servant.API.BasicAuth (BasicAuth, BasicAuthData(..))

import qualified Control.Concurrent.Async as Async
import qualified Data.ByteString.Internal as ByteString
import qualified Data.Text    as Text
import qualified Data.Text.IO as File
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import qualified Servant.Client as Client
-- import qualified Servant.XML
-- import qualified Xmlbf

import qualified Common

{-
  Add support for ARXIV later
-}
----------------
-- ARXIV API --
----------------

{-
data Entries = Entries
  { paperTitle :: Text
  , paperAbstract :: Text
  , paperLink :: Common.Url
  , paperPdfLink :: Common.Url
  } deriving (Show, Eq, Generic, Xmlbf.ToXml, Xmlbf.FromXml)

type ArxivApi = "api"
              :> "query"
              :> QueryParam "search_query" Text
              :> QueryParam "sortBy" Text
              :> QueryParam "sortOrder" Text
              :> QueryParam "max_results" Int
              :> Get '[PlainText] Text

arixvApi :: Proxy ArxivApi
arixvApi = Proxy

searchWithQuery :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int -> ClientM Text

searchWithQuery = Client.client arixvApi

getHaskellPapers :: IO () --[Common.Post]
getHaskellPapers = do
  let
    baseUrl = BaseUrl Http "export.arxiv.org" 80 ""
    query  = Just "%28ti:Haskell+OR+abs:Haskell%29+AND+%28cs.AI+OR+cs.AR+OR+cs.CC+OR+cs.CE+OR+cs.CG+OR+cs.CL+OR+cs.CR+OR+cs.CV+OR+cs.CY+OR+cs.DB+OR+cs.DC+OR+cs.DL+OR+cs.DM+OR+cs.DS+OR+cs.ET+OR+cs.FL+OR+cs.GL+OR+cs.GR+OR+cs.GT+OR+cs.HC+OR+cs.IR+OR+cs.IT+OR+cs.LG+OR+cs.LO+OR+cs.MA+OR+cs.MM+OR+cs.MS+OR+cs.NA+OR+cs.NE+OR+cs.NI+OR+cs.OH+OR+cs.OS+OR+cs.PF+OR+cs.PL+OR+cs.RO+OR+cs.SC+OR+cs.SD+OR+cs.SE+OR+cs.SI+OR+cs.SY%29"
    sortBy = Just "submittedDate"
    sortOrder = Just "descending"
    maxResults = Just 100

  manager <- TLS.newTlsManager
  res <- Client.runClientM (searchWithQuery query sortBy sortOrder maxResults) (Client.mkClientEnv manager baseUrl)

  case res of
    Left err -> print err
    Right res -> print res

paperToPost :: Entries -> [Common.Post]
paperToPost Entries{..} = [Common.Post{..}]
-}
