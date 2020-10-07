{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                      ( Text
                                                , append
                                                , unpack
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Codec.Archive.Zip              ( getEntry
                                                , withArchive
                                                , mkEntrySelector
                                                )
import           Text.XML                       ( parseText_
                                                , def
                                                , Document
                                                )
import           Text.XML.Cursor
import           Data.Text.Lazy                 ( fromStrict )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromJust )


data SpineEntry = SpineEntry {
  id' :: Text,
  href :: Maybe Text
} deriving Show

data Spine = Spine {
  epubLocation :: FilePath,
  entries :: [SpineEntry]
}


pathPrefix :: String
pathPrefix = "/home/endi/epub-store/res/"

parseChapters :: Spine -> IO [Text]
parseChapters s = mapM (parseChapter (epubLocation s)) (entries s)

parseChapter :: FilePath -> SpineEntry -> IO Text
parseChapter archivePath spineEntry = do
  let pageHref = fromJust $ href spineEntry
  chapterContents <- getFileContentsFromArchive archivePath $ unpack pageHref
  return $ Prelude.foldr append " " (c (document chapterContents) $// content)
 where
  document :: Text -> Document
  document chapterContents = parseText_ def $ fromStrict chapterContents

  c :: Document -> Cursor
  c doc = fromDocument doc

parseSpine :: FilePath -> Text -> Map.Map Text Text -> Spine
parseSpine archiveLocation opfFileContents manifest =
  let document = parseText_ def $ fromStrict opfFileContents
      c        = fromDocument document
      idrefs =
          c
            $// laxElement "spine"
            >=> child
            >=> laxElement "itemref"
            >=> attribute "idref"
  in  Spine
        { epubLocation = archiveLocation
        , entries      = Prelude.map
          (\idref ->
            SpineEntry { id' = idref, href = Map.lookup idref manifest }
          )
          idrefs
        }

parseManifest :: Text -> Map.Map Text Text
parseManifest opfFileContents =
  let document = parseText_ def $ fromStrict opfFileContents
      c        = fromDocument document
      items    = c $// laxElement "manifest" >=> child >=> laxElement "item"
      ids      = concatMap (attribute "id") items
      hrefs    = concatMap (attribute "href") items
      zipped   = Prelude.zip ids hrefs
  in  Map.fromList zipped


getOpfPath :: Text -> Maybe Text
getOpfPath containerFileContents =
  let document = parseText_ def $ fromStrict containerFileContents
      c        = fromDocument document
      res =
          c
            $// attributeIs "media-type" "application/oebps-package+xml"
            >=> attribute "full-path"
  in  case res of
        [a] -> Just a
        []  -> Nothing

getContainerContents :: FilePath -> IO Text
getContainerContents archivePath =
  getFileContentsFromArchive archivePath "META-INF/container.xml"

getFileContentsFromArchive :: FilePath -> FilePath -> IO Text
getFileContentsFromArchive archivePath fileName = do
  entrySelector <- mkEntrySelector fileName
  decodeUtf8 <$> withArchive archivePath (getEntry entrySelector)


main :: IO ()
main = do
  putStrLn "hello world"
