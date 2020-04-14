module Html where

import Lucid ( Html, a_, body_, button_, content_, charset_
             , class_, doctypehtml_, h1_, head_
             , href_, link_, meta_, name_
             , rel_, span_, table_, tbody_, title_
             , td_, tr_, type_
             )
import Common (Post(..), Source(..), unLambdas, unUrl)

import qualified Lucid as Lucid

htmlBase :: Html () -> Html ()
htmlBase inner = doctypehtml_ $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width"]
        title_ [] "yayhaskell"
        link_ [href_ "style.css", rel_ "stylesheet", type_ "text/css"]
    body_ $ do
        h1_ [] "YayHaskell"
        inner

postsToHtml :: [Post] -> Html ()
postsToHtml posts = do
    table_ [] $ do
        tbody_ [] $ do
            mapM_ (uncurry postToTableRow) (zip [1..] posts)

postToTableRow :: Int -> Post -> Html ()
postToTableRow num Post{..} = do
    let bgClass = case postSource of
            HackerNews -> "hackerRow"
            Reddit     -> "redditRow"
            Yay        -> "yayRow"

    tr_ [class_ $ "tableRowTop " <> bgClass] $ do
        td_ [class_ "tableRowNum"] $ do
            span_ [] $ showHtml num <> "."
        td_ [class_ "tableRowTitle"] $ do
            a_ [href_ $ unUrl postLink, class_ "postTitle"] $ showHtml postTitle
        td_ [] ""
    tr_ [class_ $ "tableRowBottom " <> bgClass] $ do
        td_ [class_ "tableRowUpvoteCount"] $ do
            span_ [] $ showHtml (unLambdas postLambdas) <> "λ"
        td_ [class_ "tableRowComments"] $ do
            a_ [ href_ $ unUrl postCommentsLink
               , class_ "postCommentLink"
               ] $ "Comments"
        td_ [class_ "tableRowUpvote"] $ do
            button_ [] "λ"

showHtml :: Show a => a -> Html ()
showHtml = Lucid.toHtml . show

{-
writePostsHtmlToDocs :: IO ()
writePostsHtmlToDocs = do
    posts <- fetchPosts
    Lucid.renderToFile "../docs/posts.html" $
        htmlBase $ postsToHtml posts

fetchPostsHtml :: IO ()
fetchPosts = do
-}
