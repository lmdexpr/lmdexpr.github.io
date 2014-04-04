--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map       as M
import           Data.Monoid    (mappend, mconcat)
import           Data.Char
import           Text.Pandoc
import           Hakyll

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
            >>= loadAndApplyTemplate "templates/default.html" (mappend mathCtx defaultContext)
            >>= relativizeUrls

    tags <- buildTags "posts/*.md" (fromCapture "tags/*.html")

    mapM_ (\(target, ctx, title, lists) -> do
                match (fromRegex $ target ++ "/*.md") $ do
                    route $ setExtension "html"
                    compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
                        >>= saveSnapshot "content"
                        >>= return . fmap demoteHeaders
                        >>= loadAndApplyTemplate 
                                (fromFilePath $ "templates/" ++ (target ++ "s") ++ ".html") ctx
                        >>= loadAndApplyTemplate "templates/default.html" (mappend mathCtx ctx)
                        >>= relativizeUrls
                
                create [fromFilePath $ title ++ ".html"] $ do
                    route idRoute
                    compile $ do
                        let listCtx =
                                field (target ++ "s")
                                    (\_ -> lists (fromRegex $ target ++ "/*.md") recentFirst)
                                `mappend` constField "title" (headUpper title)
                                `mappend` defaultContext
                        makeItem ""
                            >>= loadAndApplyTemplate 
                                    (fromFilePath $ "templates/" ++ title ++ ".html") listCtx
                            >>= loadAndApplyTemplate 
                                    "templates/default.html" (mappend mathCtx listCtx)
                            >>= relativizeUrls
          ) [ ("post" , postCtx tags, "archive", postList tags)
            , ("sweet", sweetCtx    , "cafe"   , sweetList)
            ]

    --Copied from https://github.com/tanakh/tanakh.jp/blob/master/site.hs
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
               >>= loadAndApplyTemplate "templates/archive.html"
                       (constField "title" title `mappend`
                        constField "posts" list  `mappend`
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
                >>= loadAndApplyTemplate "templates/default.html" 
                    (mappend mathCtx (postCtx tags))
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
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , urlField "image"
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
        deploy = "cp -r _site/* .. && ./lmdexpr.github.io clean"

--------------------------------------------------------------------------------
headUpper :: String -> String
headUpper (c:cs) = toUpper c : cs
