module Client where

import Client.HackerNews
import Client.Reddit
import Common
import qualified Network.HTTP.Client.TLS as TLS


fetchPosts :: IO [Post]
fetchPosts = do
    man <- TLS.newTlsManager
    -- TODO: Make it so that it sorts list or something
    -- based on command, etc...
    redditPosts <- take 10 <$> getHaskellHot man
    hackerPosts <- take 10 <$> getHaskellStories man
    pure $ concat [ redditPosts
                  , hackerPosts
                  ]
