{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Common where

import Chronos
import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Proxy
import Database.PostgreSQL.Simple (FromRow, Only(..), ToRow, (:.)(..))
import Database.PostgreSQL.Simple.FromField (FieldParser, FromField, ResultError(..))
import Database.PostgreSQL.Simple.FromRow (FromRow(..), RowParser, field)
import Database.PostgreSQL.Simple.ToField (Action(..), ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import GHC.Generics (Generic)
import Template.Enum
import Servant (FromHttpApiData)

import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple.FromField as Field
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Network.HTTP.Client.TLS as TLS

-- TODO: Flesh out Post data type
-- It must represent posts from different sites

-- Reddit i s red, hackerNews is orange, Arixv is blue tags
-- Yay posts are in purple.
data Source = Arixv | HackerNews | Reddit | Yay
    deriving (Show, Eq, Generic, FromJSON, ToJSON)

derivePgEnum sourceInflector ''Source

{-
instance FromField Source where
    fromField :: FieldParser Source
    fromField f mdata = do
        n <- Field.typename f
        if n /= "source"
        then Field.returnError Incompatible f ""
        else case mdata of
            Nothing -> Field.returnError UnexpectedNull f ""
            Just bs -> case parseMyEnum bs of
                Nothing -> Field.returnError ConversionFailed f (show bs)
                Just x  -> return x

parseMyEnum :: Bytes.ByteString -> Maybe Source
parseMyEnum = \case
    "Arxiv" -> Just Arixv
    "HackerNews" -> Just HackerNews
    "Reddit" -> Just Reddit
    "Yay" -> Just Yay
    _     -> Nothing

instance ToField Source where
    toField :: Source -> Action
    toField = Plain . Builder.byteString . Char8.pack . show
-}

newtype Id a = Id { unId :: Int }
    deriving stock (Functor, Generic, Show)
    deriving newtype (Eq, FromJSON, ToJSON, FromField, ToField, FromHttpApiData)

newtype Url = Url { unUrl :: Text }
    deriving stock (Generic, Show)
    deriving newtype (Eq, FromJSON, ToJSON, FromField, ToField)

newtype Lambdas = Lambdas { unLambdas :: Int }
    deriving stock (Generic, Show)
    deriving newtype (Eq, FromJSON, ToJSON, FromField, ToField)
-- TODO: Can't comment unless you click on the link.

{-
    Represents the flow of the data.
    Used as a phantom type.
    Up means towards the client. (Must be client friendly*)
    Down means toward the DB. (Must be DB friendly*).
    Might add actual directions instead of Up/Down. Or
    maybe even GET/POST
    That looks quite reasonable!

Another change to consider: rather than have a phantom parameter that enforces data flow via abstraction (i.e., hiding the constructor), why not have that parameter actually attached to the contents it describes?

data DataB dA
  = { datas :: dA
    }
Then you can start with a DataB (), ensuring that that field contains nothing, because it cannot contain anything in the first place.

getDataBById :: Int -> IO (DataB ())
and the type changes once the field gets populated:

buildDataB :: DataB () -> Queried [DataASource] -> DataB [DataA]

-}
-- data Flow = Up | Down

data WithId a = WithId
    { getId :: !(Id a)
    , withoutId :: !a
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON)

-- TODO: Check if functor laws are preserved.
-- We want to preserve IDs.
instance Functor WithId where
    fmap :: (a -> b) -> WithId a -> WithId b
    fmap f WithId{..} = WithId (f <$> getId) $ f withoutId

instance FromRow a => FromRow (WithId a) where
    -- fromRow :: RowParser (WithId a)
    fromRow = WithId <$> field <*> fromRow
    {-# INLINE fromRow #-}

instance ToRow a => ToRow (WithId a) where
    -- toRow :: WithId a -> [Action]
    toRow WithId{..} = toRow (Only getId :. withoutId)
    {-# INLINE toRow #-}

data User = User
    { userEmail :: Text
    , userLambdas :: Lambdas
    } deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow, ToRow)

data UserAuth = UserAuth
    { userAuthEmail :: Text
    -- TODO: Maybe be better as bytestring
    , userAuthPassword :: Text
    } deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow, ToRow)

-- toUpUser :: User Down -> User Up
-- toUpUser User{..} = User userEmail
instance FromField Time where
    fromField :: FieldParser Time
    -- type FieldParser a = Field -> Maybe ByteString -> Conversion a
    fromField field m'data = Time <$> Field.fromField field m'data

instance ToField Time where
    toField (Time int64) = toField int64

data Post = Post
    { -- Case of Nothing: the post was pulled from Reddit, etc...
      -- TODO: Make a yayhaskell user bot that puts post and stuff.
      -- So a user could be the yayhaskell bot.
      postUserId       :: Id User
    -- The date the post was created. If it's an external post we use the time associated
    -- with it i.e. Reddit/HackerNews. If it's internal then use our time.
    , postCreatedAt    :: Time
    -- The type of post
    , postSource       :: Source
    -- The title of the article/research paper
    , postTitle        :: Text
    -- The link to the post
    , postLink         :: Url
    -- The link to the comment section.
    , postCommentsLink :: Url
    -- The number of likes the post has. Each posts score
    -- is 0 when the post is created.
    , postLambdas      :: Lambdas
    } deriving (Eq, Show, Generic, FromJSON, ToJSON, FromRow, ToRow)

-- For papers and user posts that
-- are not reddit or hackerNews posts.
data Comment = Comment
    { commentUserId   :: Id User
    , commentPostId   :: Id Post
    -- , commentDate    :: Time
    , commentEditedAt :: Maybe Time
    , commentLink     :: Url
    , commentContent  :: Text
    , commentLambdas  :: Lambdas
    } deriving (Eq, Show, Generic, FromJSON, ToJSON)

tShow :: Show a => a -> Text
tShow = Text.pack . show

