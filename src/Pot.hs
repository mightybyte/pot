{-# LANGUAGE OverloadedStrings #-}
module Pot where

import           AppState

import           Control.Applicative
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import           Data.List
import           Snap.Heist
import           Snap.Types
import           System.Directory.Tree hiding (name)
import           Text.Templating.Heist
import qualified Text.XML.Expat.Tree as X

editFile :: String -> String -> B.ByteString -> StateSnap ()
editFile d ext redir =
    method GET get <|>
    method POST post
  where
    get = do
        ts <- asks templateState
        fname <- withRequest (return . rqPathInfo)
        let fpath = d++(B.unpack fname)++ext
        c <- liftIO $ readFile fpath
        let ctag = X.Element "textarea" [ ("name","contents")
                                        , ("rows","30")
                                        , ("cols","80")] [X.Text $ B.pack c]
        let ts' = bindSplices [ ("filename", return [X.Text $ B.pack fpath])
                              , ("fileEditControl", return [ctag])
                              ] ts
        renderHtml ts' "editor/edit"
    post = do
        fname <- withRequest (return . rqPathInfo)
        let fpath = d++(B.unpack fname)++ext
        mc <- getParam "contents"
        let c = maybe "" id mc
        liftIO $ B.writeFile fpath c
        redirect redir


handleTemplates :: StateSnap ()
handleTemplates = do
    ts <- asks templateState
    handleAllTemplates ts


------------------------------------------------------------------------------
-- | A splice showing a nested list of directories and templates
tagsSplice :: FilePath -> Splice StateSnap
tagsSplice =
    dirTreeSplice
    (X.Element "ul" [("class", "tagList")])
    (\n cs -> X.Element "li" [("class", "tagDir")] $
              (X.Text (B.append (B.pack n) "/")) : cs)
    (\p n -> X.Element "li" [("class", "tagFile")] $
           [X.Element "a" [("href", B.append "/tag/" (showName (dircat p n)))] $
            [X.Text (showName n)]
           ])
    (isSuffixOf ".hs")
  where
    showName str = B.pack $ take (length str - 3) str


------------------------------------------------------------------------------
-- | A splice showing a nested list of directories and templates
templatesSplice :: FilePath -> Splice StateSnap
templatesSplice =
    dirTreeSplice
    (X.Element "ul" [("class", "templateList")])
    (\n cs -> X.Element "li" [("class", "templatedir")] $
              (X.Text (B.append (B.pack n) "/")) : cs)
    (\p n -> X.Element "li" [("class", "templatefile")] $
           [X.Element "a" [("href", B.append "/template/" (showName (dircat p n)))] $
            [X.Text (showName n)]
           ])
    (isSuffixOf ".tpl")
  where
    showName str = B.pack $ take (length str - 4) str


dircat :: FilePath -> FilePath -> FilePath
dircat p n = if not (null p) then p++"/"++n else n


------------------------------------------------------------------------------
-- | General splice that traverses a directory tree building a document.
dirTreeSplice :: ([Node] -> Node) -- ^ Function to apply to each whole directory
              -- ^ Function applied to each directory node
              -> (FileName -> [Node] -> Node)
              -- ^ Function applied to each file node
              -> (FileName -> FileName -> Node)
              -- ^ Predicate determining which files to display
              -> (FileName -> Bool)
              -- ^ Root directory to traverse
              -> FilePath
              -> Splice StateSnap
dirTreeSplice listFunc dirFunc fileFunc filePred d = do
    (_:/dt) <- liftIO $ readDirectory d
    return $ worker "" (contents dt)
  where
    worker prefix ds =
        [listFunc $ foldl' single [] ds]
      where
        single ns (Dir n c) = dirFunc n (worker (dircat prefix n) c) : ns
        single ns (File n _) = if filePred n
            then fileFunc prefix n : ns
            else ns
        single ns (Failed _ _) = ns

