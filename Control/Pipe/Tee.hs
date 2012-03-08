module Control.Pipe.Tee where

import Control.Monad.Trans   (MonadIO(..))
import Control.Pipe          (Pipe, (>+>), discardL, idP, pipe)
import Control.Pipe.Binary   (fileWriter)
import Control.Pipe.Monoidal ((***), splitP)
import Data.ByteString       (ByteString)

-- | copy the input to the output, but also write a copy to the specified log file
tee :: (MonadIO m) => 
           (a -> ByteString)   -- ^ function to convert the value to a 'ByteString' which can be written to the log
        -> FilePath -- ^ file to log to
        -> Pipe a a m ()
tee showBS logFile  =
    splitP >+> ((pipe showBS >+> fileWriter logFile) *** idP) >+> discardL

-- | copy the input to the output, but also write a copy to the specified log file
--
-- just an alias for @tee id@
teeBS :: (MonadIO m) => 
           FilePath -- ^ file to log to
        -> Pipe ByteString ByteString m ()
teeBS = tee id
