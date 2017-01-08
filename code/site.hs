{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Function (on)
import Data.List (intersperse, intercalate)
import Data.Monoid ((<>))
import Data.Ord (Down(Down))
import System.IO

import Codec.Binary.Base64.String
import Hakyll
import System.Directory
import System.IO.Temp
import System.Process
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Definition
import Text.Pandoc.Walk


rules :: Rules ()
rules = do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    let menuTagsCtxPart = menuTagsField "menutags" . takeTags 3 $ tagsByFrequency tags
        finalize ctx =
            loadAndApplyTemplate "templates/default.html" (menuTagsCtxPart <> ctx)
            >=> relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ awesomeCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
            >>= finalize postCtx

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

    create ["tags/index.html"] $ do
        route idRoute
        compile $ do
            renderedTags <- renderTagList $ tagsByFrequency tags
            let ctx = constField "title" "Tags" <> defaultContext
            makeItem renderedTags
                >>= finalize ctx

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

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom myFeedConfiguration feedCtx posts

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

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "xkollar"
    , feedDescription = "…"
    , feedAuthorName  = "xkollar"
    , feedAuthorEmail = ""
    , feedRoot        = "https://xkollar.github.io"
    }

-------------------------------------------------------------------------------
-- {{{ Pandoc experimentation -------------------------------------------------
-------------------------------------------------------------------------------

transformativePandoc :: (Pandoc -> Compiler Pandoc) -> Compiler (Item String)
transformativePandoc = pandocCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions

runCmdIn
    :: FilePath
    -> String
    -> [String]
    -> String
    -> IO (String, [FilePath])
runCmdIn dir c opts i = withFile "/dev/null" ReadWriteMode $ \ h -> do
    v <- readCreateProcess (cp h) i
    s <- filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
    return (v,s)
    where
        cp h = CreateProcess
            { cmdspec = RawCommand c opts
            , std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = UseHandle h
            , cwd = Just dir
            , env = Nothing
            , close_fds = True
            , create_group = False
            , delegate_ctlc = False
            }

dotProc :: String -> IO String
dotProc i = withSystemTempDirectory "dot-processor" $ \ tmp ->
    fst <$> runCmdIn tmp
        "dot" ["-Tsvg"] i

dotToImage :: Block -> IO Block
dotToImage cb@(CodeBlock (i, cs, a) s)
    | "dot-render" `elem` cs = do
        s <- dotProc s
        return (Para [Image nullAttr [] ("data:image/svg+xml;base64," <> encode s, "fig:")])
dotToImage x = return x

pass :: Functor f => (b -> f a) -> b -> f b
pass f x = const x <$> f x

awesomeCompiler :: Compiler (Item String)
awesomeCompiler = transformativePandoc $ unsafeCompiler . walkM dotToImage
