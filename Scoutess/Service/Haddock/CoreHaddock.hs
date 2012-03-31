{-# LANGUAGE RecordWildCards #-}
{-

NOTES:

When 'cabal haddock' runs it gives warnings like:

Warning: The documentation for the following packages are not installed. No
links will be generated to these packages: base-unicode-symbols-0.2.2.3,
base64-bytestring-0.1.1.0, blaze-builder-0.3.1.0, blaze-html-0.4.3.1, rts-1.0,
hslogger-1.1.4, html-1.0.1.2, monad-control-0.3.1, network-2.3.0.11,
parsec-3.1.2, sendfile-0.7.4, syb-0.3.6, transformers-base-0.4.1,
utf8-string-0.3.7, xhtml-3000.2.0.5, zlib-0.5.3.3


Even though we include --read-interface flags for that. The warning is likely wrong and is based on what cabal itself automatically found. But, really, we do not want cabal finding anything except what we tell it. Perhaps we should invoke haddock directly instead of using cabal?

-}
module Scoutess.Service.Haddock.CoreHaddock where

import Control.Pipe
import Control.Pipe.Binary
import Control.Pipe.Process
import Control.Monad
import Control.Monad.Trans
import Data.ByteString       (ByteString)
import Data.ByteString.Char8 (pack)
import Distribution.Package (PackageIdentifier(..), PackageName(..))
import Distribution.PackageDescription (GenericPackageDescription(..), PackageDescription(..))
import Distribution.Simple.Configure
import Distribution.Simple.LocalBuildInfo

import qualified Distribution.Simple.Haddock as C
import Distribution.Simple.Setup (HaddockFlags(..), defaultHaddockFlags)
import Distribution.Text    (display)
import Distribution.Version (Version)
import Scoutess.Utils.Cabal (UnpackInfo(..))
import qualified Scoutess.Utils.Cabal   as Cabal
import qualified Scoutess.Utils.Haddock as Haddock
import System.Directory     (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath      ((</>), (<.>))
import System.IO

runHaddock unpackDir docDir packageIdentifier =
    runPipe $ haddock unpackDir docDir packageIdentifier >+> joinP >+> handleWriter stdout

haddock :: FilePath           -- ^ directory for unpacked source (from hackage)
        -> FilePath           -- ^ directory for generated haddock docs
        -> PackageIdentifier  -- ^ package to generate docs for
        -> Pipe () (Either ByteString ByteString) IO ()
haddock unpackDir docDir packageIdentifier =
  do let docDir' = docDir </> display packageIdentifier
     eDocDir' <- lift $ doesDirectoryExist docDir'
     if eDocDir' -- skip if output directory already exists (not the right thing to do if the docs are out of date)
      then return ()
      else do
       lift $ createDirectoryIfMissing True unpackDir
       r <- Cabal.unpack unpackDir packageIdentifier
       case r of
         (Left ec) ->
                yield (Left $ pack $ show ec)
         (Right unpackInfo) ->
             do let distPref      = (unpackPath unpackInfo </> "dist") -- I don't feel good about hardcoding "dist", move calculation to Cabal
                    dotCabalPath  = (unpackPath unpackInfo </> display (pkgName packageIdentifier) <.> "cabal")
                outdated <- lift $ do e <- doesFileExist (localBuildInfoFile distPref)
                                      if e
                                       then checkPersistBuildConfigOutdated distPref dotCabalPath
                                       else return True
                lift $ putStrLn $ "outdated: " ++ show (outdated, distPref, dotCabalPath)
                when (outdated) $ Cabal.configure (unpackPath unpackInfo) ["--user"]  >> return ()
                lbi <- lift $ getPersistBuildConfig distPref
                case libraryConfig lbi of
                  Nothing -> return () -- nothing to do if there is no library?
                  (Just clbi) ->
                      do let packageIds = map snd (componentPackageDeps clbi)
                         haddockDependencies unpackDir docDir packageIds
                         lift $ createDirectoryIfMissing True docDir'
                         includes <- mapM mkInclude packageIds
--                         lift $ putStrLn $ unwords $ concat $ includes
                         let flags = defaultHaddockFlags
                                     { haddockProgramArgs = ("-v", []) : concat includes
                                     }
                         lift $ C.haddock (localPkgDescr lbi) lbi [] flags
{-
 (unpackPath unpackInfo)
                                 ([ "--haddock-option", "-o", "--haddock-option", docDir'
                                  , "--html"
                                  , "--haddock-option", "--hoogle"
                                  ] ++ concat includes)
-}
                         return ()
    where
      mkInclude packageIdentifier
          | (pkgName packageIdentifier) `elem` (map PackageName blacklist) =
              return []
          where
            blacklist = ["rts"]
      mkInclude packageIdentifier =
          do let origDotHaddockPath = unpackDir </> (display packageIdentifier) </> "dist" </> "doc" </> "html" </> (display $ pkgName packageIdentifier) </> (display $ pkgName packageIdentifier) <.> "haddock"
                 destDotHaddockPath = docDir </> (display packageIdentifier) </> (display $ pkgName packageIdentifier) <.> "haddock"
             destExists  <- lift $ doesFileExist destDotHaddockPath
             destExists' <- if destExists then return True
                            else do origExists <- lift $ doesFileExist origDotHaddockPath
                                    if origExists
                                       then do lift $ copyFile origDotHaddockPath destDotHaddockPath
                                               return True
                                       else return False
             if destExists'
              then return [ ("--read-interface"
                          , ["../" ++ display packageIdentifier ++ "," ++ destDotHaddockPath])
                          ]
              else return []
          where
            precious  = ["base","ghc-prim","integer-gmp"]

haddockDependencies :: FilePath
                    -> FilePath
                    -> [PackageIdentifier]
                    -> Pipe () (Either ByteString ByteString) IO ()
haddockDependencies unpackDir docDir packageIdentifiers =
    mapM_ (haddock unpackDir docDir) packageIdentifiers