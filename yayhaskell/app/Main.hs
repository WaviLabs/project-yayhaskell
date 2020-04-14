module Main where

import Client
import Database
import Database.PostgreSQL.Simple (ConnectInfo(..))
import Server

import qualified Client.HackerNews as Hacker
import qualified Client.Reddit as Reddit
import qualified Database.PostgreSQL.Simple as PGSQL
import qualified Html as Html
import qualified Lucid as Lucid
import qualified Server as Server
import qualified Network.Wai.Handler.Warp as Warp


main :: IO ()
main = do
    conn <- PGSQL.connect $
        ConnectInfo
        "localhost"
        5432
        "yayhaskell"
        "Haske!!et0n"
        "testdb"
    print "Connected to DB..."
    -- Put within when *certain time* block forkIO or something
    posts <- fetchPosts
    print "Fetched posts..."
    _     <- writePosts conn posts
    print "Wrote new posts to DB..."
    -- print posts
    print "Starting server..."
    Warp.run 9000 $ app conn

    {-
        Server starting logic
    -}

    {-
    Generate HTML...
    -}

    {-
    ...Serve HTML
    -}
