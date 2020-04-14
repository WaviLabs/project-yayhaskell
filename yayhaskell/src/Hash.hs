module Hash
    ( HashedUser(..)
    , hashUser
    , isAuthorized
    , testHashUser
    , unHashUser
    ) where

import Common (Id(..), Lambdas(..), UserAuth(..), User(..), tShow)
import Crypto.Hash
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8)
import Database.PostgreSQL.Simple.FromRow (FromRow(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Database.PostgreSQL.Simple.ToRow (ToRow(..))
import GHC.Generics
import System.Random

import qualified Data.ByteString as Bytes
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


instance Random ByteString where
    random :: RandomGen g => g -> (ByteString, g)
    random g = (randomBS, snd $ next g) where
        bounds :: (Int, Int)
        bounds = (0, 9)
        randomBS :: ByteString
        -- ATTENTION: randomRs generates infinite list
        randomBS = Text.encodeUtf8 . Text.concat . map tShow . take 32 $ randomRs bounds g
    -- TODO: Implement randomR

-- TODO: Don't expose hashed user constructor. Email
-- is not apart of hash.
-- Hashed User is used on the backend/DB side.
data HashedUser = HashedUser
    { hashedUserEmail :: Text
    , hashedUserHash :: ByteString
    , hashedUserSalt :: Salt
    , hashedUserLambdas :: Lambdas
    } deriving (Eq, Show, Generic, FromRow, ToRow)

testHashUser :: UserAuth -> IO HashedUser
testHashUser UserAuth{..} = pure $ HashedUser
    userAuthEmail
    (Text.encodeUtf8 userAuthPassword)
    (Salt "testSalt")
    (Lambdas 0)

-- TODO: Go back to this implementation when it is performant.
hashUser :: UserAuth -> IO HashedUser
hashUser UserAuth{..} = do
    hashedUserSalt <- makeSalt
    let
        hashInput = applySalt hashedUserSalt $
            Text.encodeUtf8 userAuthPassword
        hashedUserHash = Text.encodeUtf8 . Text.pack . show $
            hashWith SHA3_256 hashInput
        hashedUserEmail = userAuthEmail
        hashedUserLambdas = Lambdas 0

    pure HashedUser{..}

unHashUser :: HashedUser -> User
unHashUser HashedUser{..} = User hashedUserEmail hashedUserLambdas

-- | A ByteString used for salting data before it
--  is hashed.
newtype Salt = Salt { unSalt :: ByteString }
    deriving stock (Generic, Show)
    deriving newtype (Eq, Random, FromField, ToField)

-- TODO: Could be bad for performance
makeSalt :: IO Salt
makeSalt = randomIO

applySalt :: Salt -> ByteString -> ByteString
applySalt Salt{..} bs = unSalt <> bs

-- TODO: Check correctness of properties
isAuthorized :: UserAuth -> HashedUser -> Bool
isAuthorized auth@UserAuth{..} HashedUser{..} = and
    [ userAuthEmail == hashedUserEmail
    , (digestToBS $ hashWith SHA3_256 try) == hashedUserHash
    ]
  where
    try = applySalt hashedUserSalt $ Text.encodeUtf8 userAuthPassword

digestToBS :: Digest a -> ByteString
digestToBS = Text.encodeUtf8 . Text.pack . show
