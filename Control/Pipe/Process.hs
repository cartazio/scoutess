-- | a (currently broken) 'Pipe' interface to 'runInteractiveProcess'
module Control.Pipes.Process where

import Control.Concurrent (killThread, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Trans
import Control.Pipe
import Data.ByteString (ByteString, empty, hGetSome, hPut)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import System.Exit
import System.IO
import System.Process

data IOAction
    = Stdout ByteString
    | StdoutClosed
    | Stderr ByteString
    | StderrClosed
    | InputReady
    | Terminated ExitCode

process :: FilePath                 -- ^ path to executable
        -> [String]                 -- ^ arguments to pass to executable
        -> Maybe String             -- ^ optional working directory
        -> Maybe [(String, String)] -- ^ optional environment (otherwise inherit)
        -> Pipe ByteString (Either ByteString ByteString) IO ExitCode
process executable args wd env =
    do action <- lift $ atomically newTChan
       input  <- lift $ atomically $ newTVar empty
       tids   <- lift $
               do (inh, outh, errh, proch) <- runInteractiveProcess executable args wd env
                  inTid <- forkIO $ forever $
                     do atomically $ writeTChan action InputReady
                        x <- atomically $ readTVar input
                        hPut inh x

                  outTid <- forkIO $ forever $
                     do b <- hGetSome outh 100
                        atomically $ writeTChan action (Stdout b)

                  errTid <- forkIO $ forever $
                     do b <- hGetSome errh 100
                        atomically $ writeTChan action (Stderr b)

                  termTid <- forkIO $
                     do ec <- waitForProcess proch
                        atomically $ writeTChan action (Terminated ec)
                  return [inTid, outTid, errTid, termTid]
       ec <- go action input True
       lift $ mapM_ killThread tids
       return ec

    where
      go :: TChan IOAction -> TVar ByteString -> Bool -> Pipe ByteString (Either ByteString ByteString) IO ExitCode
      go action input needsInput' =
          do -- here we really want to wait on readTChan returning
             -- *or* 'await' being ready to produce something to feed to 'inh'
             needsInput <-
                 if needsInput'
                   then do mx <- tryAwait
                           case mx of
                             Nothing  ->
                                 do return True
                             (Just x) ->
                                 do lift $ atomically $ writeTVar input x
                                    return False
                   else return False
             a <- lift $ atomically $ readTChan action
             case a of
               (Stdout b) ->
                   do yield (Right b)
                      go action input needsInput
               (Stderr b) ->
                   do yield (Left b)
                      go action input needsInput
               (Terminated ec) ->
                   do return ec
               InputReady ->
                   do go action input True
               _          ->
                   do go action input needsInput

test :: IO ()
test =
 runPipe $
    (forever $ yield (C.pack "foo\n")) >+>
    (process "/bin/cat" [] Nothing Nothing >>= \ec -> lift $ print ec) >+>
    (forever $ (lift . either C.putStr C.putStr ) =<< await)