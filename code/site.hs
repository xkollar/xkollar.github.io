{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Function (on)
import Data.List (intersperse, intercalate)
import Data.Monoid ((<>))
import Data.Ord (Down(Down))

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import Hakyll

rules :: Rules ()
rules = do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    let menuTagsCtxPart = menuTagsField "menutags" (tagsByFrequency tags)
        finalize ctx =
            loadAndApplyTemplate "templates/default.html" (menuTagsCtxPart <> ctx)
            >=> relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
            >>= finalize postCtx
            -- >>= loadAndApplyTemplate "templates/default.html" (menuTagsCtx <> postCtx)
            -- >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" <> tag <> "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      <> listField "posts" postCtx (return posts)
                      <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= finalize ctx
                -- >>= loadAndApplyTemplate "templates/default.html" (menuTagsCtx <> ctx)
                -- >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts)
                    <> constField "title" "Archive"
                    <> defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= finalize archiveCtx
                -- >>= loadAndApplyTemplate "templates/default.html" (menuTagsCtx <> archiveCtx)
                -- >>= relativizeUrls

    create ["tags/index.html"] $ do
        route idRoute
        compile $ do
            renderedTags <- renderTagList $ tagsByFrequency tags
            let ctx = constField "title" "Tags" <> defaultContext
            makeItem (renderedTags)
                >>= finalize ctx
                -- >>= loadAndApplyTemplate "templates/default.html" (menuTagsCtx <> ctx)
                -- >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts)
                    <> constField "title" "Home"
                    <> defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= finalize indexCtx
                -- >>= loadAndApplyTemplate "templates/default.html" (menuTagsCtx <> indexCtx)
                -- >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


config :: Configuration
config = defaultConfiguration
    { providerDirectory = "data"
    }

main :: IO ()
main = hakyllWith config rules

-------------------------------------------------------------------------------
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

postCtx :: Context String
postCtx = dateField "date" "%Y-%m-%d"
    <> defaultContext

tagsByFrequency :: Tags -> Tags
tagsByFrequency = sortTagsBy cmp where
    cmp = compare `on` Down . length . snd

takeTags :: Int -> Tags -> Tags
takeTags n t = t {tagsMap = take n $ tagsMap t}

menuTagsField :: String -> Tags -> Context a
menuTagsField key tags = field key $ \_ -> renderTags makeLink (intercalate separator) tags where
    separator = renderHtml $ H.span H.! A.class_ "space" $ " "
    makeLink tag url _ _ _ = renderHtml $ H.li $ H.a H.! A.href (H.toValue url) $ H.toHtml tag
