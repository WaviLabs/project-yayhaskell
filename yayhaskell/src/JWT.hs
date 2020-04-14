module JWT where

import Data.Aeson
import Data.Map
import Data.Text
import Data.Time
import Data.Time.Format
import GHC.Generics
import Servant.API
import Web.JWT (JOSEHeader(..), JWTClaimsSet(..), Algorithm(..))

import qualified Web.JWT as ExtJWT


newtype JWT = JWT { unJWT :: Text }
    deriving stock (Generic, Show)
    deriving newtype (Eq, FromJSON, ToJSON, FromHttpApiData)

newtype Payload = Payload { unPayload :: Map Text Value }

defJoseHeader = JOSEHeader
    { typ = Just "JWT"
    , cty = Nothing
    , alg = Just HS256
    , kid = Nothing
    }

getEpoch :: IO UTCTime
getEpoch = parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ") "1970-01-01T00:00:00Z"

getMaybeCurrentNumericDate :: IO (Maybe ExtJWT.NumericDate)
getMaybeCurrentNumericDate = do
    current <- getCurrentTime
    epoch   <- getEpoch
    pure . ExtJWT.numericDate $ diffUTCTime current epoch

-- TODO: Make sure JWT does baseEncoding as well. Or
-- handle it at the server handler level
encodeJWT :: Text
          -> Text
          -> Maybe ExtJWT.NumericDate
          -> Maybe ExtJWT.NumericDate
          -> Payload
          -> Text
          -> JWT
encodeJWT iss sub m'expTime m'iatTime (Payload unPayload) secret =
    JWT $ ExtJWT.encodeSigned (ExtJWT.hmacSecret secret) defJoseHeader claims
  where
    claims :: ExtJWT.JWTClaimsSet
    claims = ExtJWT.JWTClaimsSet
        -- TODO: Add values for iss time etc...
        (ExtJWT.stringOrURI iss)
        (ExtJWT.stringOrURI sub)
        Nothing
        m'expTime
        Nothing
        m'iatTime
        Nothing
        (ExtJWT.ClaimsMap unPayload)

-- TODO: Should decode base64 encoding
decodeJWT :: Text -> Text -> Maybe JWTClaimsSet
decodeJWT secret input = ExtJWT.claims <$> ExtJWT.decodeAndVerifySignature (ExtJWT.hmacSecret secret) input
