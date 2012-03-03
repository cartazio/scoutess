{-# LANGUAGE OverloadedStrings #-}
module Scoutess.Utils.HTTP (downloadFile) where

import Network.HTTP
import Network.HTTP.Base (defaultGETRequest_)
import Network.URI (parseURI)

import qualified Data.ByteString as BS

downloadFile :: String -> FilePath -> IO (Maybe FilePath)
downloadFile url dest = do
  let (Just uri) = parseURI url 
  ersp <- simpleHTTP (defaultGETRequest_ uri)
  case ersp of
    Left err  -> return Nothing -- SourceErrorOther "downloadFile: connection error"
    Right rsp -> case rspCode rsp == (2, 0, 0) of
      True  -> BS.writeFile dest (rspBody rsp) >> return (Just dest)
      False -> return Nothing
