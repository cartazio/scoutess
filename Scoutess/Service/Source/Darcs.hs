{-# LANGUAGE OverloadedStrings #-}
-- | fetch (cabal) source using @darcs@
--
-- <http://darcs.net>
module Scoutess.Service.Source.Darcs where

import Control.Monad.Trans                           (MonadIO(..), liftIO)
import Data.Text                                     (Text)
import qualified Data.Text as Text                   (dropWhileEnd, pack, unpack, breakOnEnd, append)
import Distribution.PackageDescription               (specVersion)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parse         (readPackageDescription)
import Distribution.Verbosity                        (silent)
import Distribution.Version                          (Version)
import System.Cmd                                    (system)
import System.Exit                                   (ExitCode(..))
import System.FilePath                               ((</>))

import Scoutess.Service.Source.Core (SourceConfig(..), SourceException(..), SourceInfo(..))

-- | fetch source using @darcs@
fetchDarcs :: (MonadIO m) =>
              SourceConfig -- ^ 'SourceConfig'
           -> Text         -- ^ location of darcs repo (e.g. @http:\/\/example.org\/repo@, @ssh:\/\/user\@example.org/srv/darcs/repo@)
           -> Maybe Text   -- ^ optional darcs tag
           -> m (Either SourceException SourceInfo)
fetchDarcs sourceConfig location maybeTag = do
    -- escape the command line arguments?
    exitCode <- liftIO . system . Text.unpack $
            "darcs get "    ++. location
        ++. " --repo-name=" ++. "\"" ++. Text.pack destDir ++. "\""
        ++. case maybeTag of
            Nothing  -> ""
            Just tag -> "--tag =" ++. "\"" ++. tag ++. "\""
    liftIO $ case exitCode of
        ExitFailure _ -> return . Left $ SourceErrorOther "Couldn't download the package. Please check your connection and the repo location."
        ExitSuccess   -> do
            genericPkgDesc <- readPackageDescription silent (destDir </> pkgName ++ ".cabal")
            let pkgDescr    = flattenPackageDescription genericPkgDesc
            return . Right $ SourceInfo {
                srcPath               = destDir
              , srcPackageDescription = pkgDescr
              , srcVersion            = toSrcVersion (specVersion pkgDescr) }
    where
    -- remove all trailing '/', then assume that everything
    -- after the last '/' in location' is the package name
    location'     = Text.dropWhileEnd (=='/') location
    pkgName       = Text.unpack . snd $ Text.breakOnEnd (Text.pack "/") location'
    destDir       = srcCacheDir sourceConfig </> pkgName
    toSrcVersion :: Version -> Text
    toSrcVersion  = undefined
    (++.)         = Text.append
