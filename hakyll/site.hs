{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M
import Data.Monoid ((<>), mconcat)

import Text.Pandoc
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match ("images/**" .||. "js/**" .||. "fonts/**") $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "product.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
            >>= relativizeUrls
    
    tags <- buildTags "posts/*.md" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> postCtx tags)
            >>= relativizeUrls
    
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let listCtx =
                    field "posts" (\_ -> postList tags "posts/*.md" recentFirst)
                    <> constField "title" "Archive"
                    <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" listCtx
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> listCtx)
                >>= relativizeUrls

    --Copied from https://github.com/tanakh/tanakh.jp/blob/master/site.hs
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
               >>= loadAndApplyTemplate "templates/archive.html"
                       (constField "title" title <>
                        constField "posts" list  <>
                        defaultContext)
               >>= loadAndApplyTemplate "templates/default.html" defaultContext
               >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field "posts" $ \_ ->
                           postList tags "posts/*.md" $ fmap (take 5) . recentFirst

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> postCtx tags)
                >>= relativizeUrls

    match "sweets/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/sweet.html"   sweetCtx
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> sweetCtx)
            >>= relativizeUrls
    
    create ["cafe.html"] $ do
        route idRoute
        compile $ do
            let menuCtx = constField "title" "Cafe"
                          <> field "menu" (\_ -> sweetList "sweets/*.md" recentFirst)
                          <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/cafe.html"    menuCtx
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> menuCtx)
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx ts = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" ts
    , defaultContext
    ]

--------------------------------------------------------------------------------
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern sortFilter = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-list.html"
    applyTemplateList itemTpl (postCtx tags) posts
            
--------------------------------------------------------------------------------
mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
        metadata <- getMetadata $ itemIdentifier item
        return $ if "mathjax" `M.member` metadata
                    then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
                    else ""

--------------------------------------------------------------------------------
sweetCtx :: Context String
sweetCtx = mconcat
    [ dateField "date" "%B %e, %Y"
    , defaultContext
    ]
    
--------------------------------------------------------------------------------
sweetList :: Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
sweetList pattern sortFilter = do
    sweets  <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/sweet-list.html"
    applyTemplateList itemTpl sweetCtx sweets
            
--------------------------------------------------------------------------------
pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions
    {
        writerHTMLMathMethod = MathJax ""
    }

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration{ deployCommand = deploy }
    where
        deploy = "rm -rf ../*.html ../posts ../tags ../sweets ../images; cp -r _site/* .."
