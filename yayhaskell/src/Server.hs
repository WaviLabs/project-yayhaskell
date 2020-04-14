module Server where

import Data.Proxy
import Data.Text (Text)
import Data.Time
import Data.Fixed
import Client
import Control.Monad.IO.Class
import Common
import Database
import Database.PostgreSQL.Simple (Connection)
import Hash
import JWT
import Network.Wai (Application)
import Servant.Server
import Servant.API -- (Header, Header', JSON, Get, Post, ReqBody, Capture, QueryParam, (:>), (:<|>)(..))
import Servant.Client (ClientM, BaseUrl(..), Scheme(..))
import Lucid
import Servant.HTML.Lucid

import qualified Client.Reddit as Reddit
import qualified Client.HackerNews as Hacker
import qualified Common as Common
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Html as Html
import qualified Lucid
import qualified Lucid.Base as Lucid
import qualified Network.HTTP.Client.TLS as TLS
import qualified Servant.Server.StaticFiles as Servant
import qualified Servant.API as Servant
import qualified Web.JWT as ExtJWT


type AuthAPI =
    (Header' '[Required, Strict] "Authorization" JWT :> "profile" :> Get '[JSON] Text) :<|>
    -- TODO: Add a comments GET endpoint to NoAuthAPI
    (Header' '[Required, Strict] "Authorization" JWT :> "comment" :> ReqBody '[JSON] Comment :> Servant.Post '[JSON] Text)

type NoAuthAPI =
    ("front" :> Get '[JSON] [Common.Post]) :<|>
    ("signup" :> ReqBody '[JSON] UserAuth :> Servant.Post '[JSON] Text) :<|>
    ("login" :> ReqBody '[JSON] UserAuth :> Get '[JSON] (Maybe JWT))

type API = NoAuthAPI
        :<|> AuthAPI
        :<|> Raw

api :: Proxy API
api = Proxy

frontHandler :: Connection -> Handler [Common.Post]
frontHandler conn = do
    posts <- liftIO (map withoutId <$> readPosts conn)
    liftIO $ print "This is the front"
    pure posts

signupHandler :: Connection -> UserAuth -> Handler Text
signupHandler conn auth@UserAuth{..} = do
    liftIO $ print "Signing up"
    hashedUser <- liftIO $ hashUser auth
    liftIO $ print "User hashed"
    {- -- TODO: Logic checking for same Email
    -}
    rowsAffected <- liftIO $ writeHashedUser conn hashedUser
    -- TODO: Fix naive check for write success
    pure $ if rowsAffected == 1
    then "You successfully signed up."
    else "Couldn't sign up. Please try again."

loginHandler :: Connection -> UserAuth -> Handler (Maybe JWT)
loginHandler conn auth@UserAuth{..} = do
    -- TODO: Make this function more concise
    m'hashedUserWithId <- liftIO $ readHashedUserWithIdByEmail conn userAuthEmail
    case m'hashedUserWithId of
        Nothing -> pure Nothing
        Just hashedUserWithId -> case isAuthorized auth $ withoutId hashedUserWithId of
            False -> pure Nothing
            -- TODO: Reroute to front page
            -- TODO: Redirect to /profile/[userId]
            True -> do
                -- TODO: Sign a JWT and send it.
                -- TODO: Update secret. Get it from config.
                let iss = "yayhaskell"
                    sub = hashedUserEmail $ withoutId hashedUserWithId
                    payload = Payload $
                    -- TODO: Change to using the user email instead of the id
                        Map.fromList [("user_id", Aeson.toJSON $ getId hashedUserWithId)]

                curr <- liftIO getCurrentTime

                -- TODO: Break this funtion up into reusable logic
                iat <- liftIO $ getMaybeCurrentNumericDate

                epoch <- liftIO getEpoch
                let expUTC = addUTCTime (secondsToNominalDiffTime $ (MkFixed 3600 :: Pico)) curr
                    exp = ExtJWT.numericDate $ diffUTCTime expUTC epoch
                -- TODO: Subject should be username
                pure . Just $ encodeJWT "yayhaskellAPI" "" exp iat payload "yayhaskell"

-- TODO: You should have to decode base64 JWT
-- Get secret from config
profileHandler :: Connection -> JWT -> Handler Text
profileHandler conn (JWT unJWT) = pure $ case decodeJWT "yayhaskell" unJWT of
    Nothing     -> "You are anonymous. Make a profile."
    Just claims -> tShow claims
        -- TODO: Add logic for reading from DB and generating HTML

-- TODO: Add logic for writing comment to DB
commentHandler :: Connection -> JWT -> Comment -> Handler Text
commentHandler conn (JWT unJWT) _comment = pure $ case decodeJWT "yayhaskell" unJWT of
    Nothing     -> "You can't comment."
    -- TODO: This may be changed in the future to accomondate changes to security
    Just ExtJWT.JWTClaimsSet{..} -> case Map.lookup "user_id" $ ExtJWT.unClaimsMap unregisteredClaims of
        Nothing     -> "You can't comment"
        Just userId -> "You would be able to comment but I didn't implement it. USER: " <> tShow userId

authServer conn =
    profileHandler conn :<|>
    commentHandler conn

-- noAuthServer :: _
noAuthServer conn =
    frontHandler conn :<|>
    signupHandler conn :<|>
    loginHandler conn

-- server :: Connection -> Server API
server conn = noAuthServer conn
         :<|> authServer conn
         :<|> Servant.serveDirectoryFileServer "assets/"

app :: Connection -> Application
app = serve api . server
