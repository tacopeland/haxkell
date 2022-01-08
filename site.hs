--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    }

main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "*.markdown" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title `mappend` listField "posts" (postCtxWithTags tags) (return posts) `mappend` defaultContext
            makeItem "" >>= loadAndApplyTemplate "templates/tag.html" ctx >>= loadAndApplyTemplate "templates/default.html" ctx >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    {-
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls
                -}

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
            let ogCtx =
                    defaultContext
                    <> constField "root" "https://tacopeland.github.io/haxkell"
                    <> constField "og-image" "https://tacopeland.github.io/haxkell/images/og-img.png"
                indexCtx =
                    ogCtx
                    <> listField "posts" (teaserCtxWithTags tags) (return posts)
                    <> openGraphField "opengraph" ogCtx
                    <> twitterCardField "twitter" ogCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    defaultContext
    <> constField "root" "https://tacopeland.github.io/haxkell/"
    <> constField "og-image" "https://tacopeland.github.io/haxkell/images/og-img.png"
    <> dateField "date" "%B %e, %Y"

teaserCtx :: Context String
teaserCtx = teaserField "teaser" "content" `mappend` postCtx

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
    postCtx
    <> tagsField "tags" tags
    <> openGraphField "opengraph" postCtx
    <> twitterCardField "twitter" postCtx

teaserCtxWithTags tags = 
    teaserCtx
    <> tagsField "tags" tags
