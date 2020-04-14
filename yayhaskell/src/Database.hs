module Database where

import Data.ByteString
import Data.Int
import Data.Maybe
import Data.Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Common (Id(..), Post(..), User(..), WithId(..))
import Hash (HashedUser(..))

import qualified Hash
import qualified Database.PostgreSQL.Simple as Simple


readPosts :: Connection -> IO [WithId Post]
readPosts conn = query_ conn
    [sql|
        SELECT * FROM posts
    |]

writePosts :: Connection -> [Post] -> IO Int64
writePosts conn posts = executeMany
    conn
    [sql|
        INSERT INTO posts
            (userId, source, title, link, comments_link, lambdas)
        VALUES
            (?, ?, ?, ?, ?, ?)
    |]
    posts

writeHashedUser :: Connection -> HashedUser -> IO Int64
writeHashedUser conn user = execute
    conn
    [sql|
        INSERT INTO users
            (email, _hash, salt, lambdas)
        VALUES
            (?,?,?,?)
    |] user

instance ToRow Text where
    toRow text = [toField text]

readHashedUserWithIdByEmail :: Connection -> Text -> IO (Maybe (WithId HashedUser))
readHashedUserWithIdByEmail conn email = do
    -- TODO: Shoudln't even return a list. If it does that means error.
    hashedUserWithIdList <- query
        conn
        [sql|
            SELECT * FROM users WHERE email = ?
        |] email

    pure $ listToMaybe hashedUserWithIdList

instance ToRow (Id a) where
    toRow _id = [toField _id]

readUserWithIdById :: Connection -> Id User -> IO (Maybe (WithId User))
readUserWithIdById conn _id = do
    -- TODO: Shoudln't even return a list. If it does that means error.
    -- If Length longer than 1 than throw error
    hashedUserWithIdList <- query
        conn
        [sql|
            SELECT * FROM users WHERE id = ?
        |] _id

    pure $ case listToMaybe hashedUserWithIdList of
        Nothing -> Nothing
        Just hashedUserWithId -> Just $ Hash.unHashUser <$> hashedUserWithId
