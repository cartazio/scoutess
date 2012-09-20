{-# LANGUAGE OverloadedStrings #-}
-- | HTTP utility functions
module Scoutess.Utils.HTTP (downloadFile, updateFile) where

import Network.HTTP
import Network.HTTP.Base (defaultGETRequest_)
import Network.URI       (parseURI, URI)
import System.Directory  (createDirectoryIfMissing, doesFileExist, getModificationTime)
import System.FilePath   (dropFileName)
import System.Locale     (defaultTimeLocale, rfc822DateFormat)
import Data.Time         (ctTZName, formatCalendarTime, toUTCTime)

import qualified Data.ByteString as BS

-- | download a file over @http:\/\/@
--
-- 'Nothing' on failure
--
-- @Just filepath@ on success
downloadFile :: String   -- ^ remote url
             -> FilePath -- ^ local file name
             -> IO (Maybe FilePath)
downloadFile url dest =
    maybe (return Nothing)
          (flip downloadFile' dest)
          (parseURI url)

downloadFile' :: URI -> FilePath -> IO (Maybe FilePath)
downloadFile' uri dest = do
  ersp <- simpleHTTP (defaultGETRequest_ uri)
  case ersp of
    Left err  -> return Nothing -- SourceErrorOther "downloadFile: connection error"
    Right rsp -> case rspCode rsp == (2, 0, 0) of
      True  -> BS.writeFile dest (rspBody rsp) >> return (Just dest)
      False -> return Nothing

-- | Make sure a file is up to date using @http:\/\/@.
--   Sends a GET request with if-modified-since. If the file
--   is up-to-date return @Just filepath@, if the file is
--   outdated or doesn't exist, download the new version,
--   replace the file and return @Just filepath@.
--   If there is no old version and the download fails then
--   return 'Nothing'.

updateFile :: String    -- ^ remote url
            -> FilePath  -- ^ local file name
            -> IO (Maybe FilePath)
updateFile url dest = do
    maybe (return Nothing) (flip updateFile' dest) (parseURI url)

updateFile' :: URI -> FilePath -> IO (Maybe FilePath)
updateFile' uri dest = do
    fileExists <- doesFileExist dest
    if fileExists
      then do
        modifiedDate <- lastModified dest
        let setHeader = insertHeader HdrIfModifiedSince modifiedDate
        ersp <- simpleHTTP . setHeader . defaultGETRequest_ $ uri
        case ersp of
            Right rsp | rspCode rsp == (2,0,0)
                -> createDirectoryIfMissing True (dropFileName dest) >> BS.writeFile dest (rspBody rsp)
            _   -> return ()
        return (Just dest)
      else downloadFile' uri dest

-- | returns the last modifed date of the file in RFC 822 format
lastModified :: FilePath -> IO String
lastModified file = do
    modTime <- getModificationTime file
    let gmt = (toUTCTime modTime){ctTZName = "GMT"}
    return $ formatCalendarTime defaultTimeLocale rfc822DateFormat gmt
