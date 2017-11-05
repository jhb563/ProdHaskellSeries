module Errors where

import Control.Exception.Safe (SomeException, handleAny)
import Data.ByteString.Lazy.Char8 (pack)
import Servant.Server (ServantErr(..), err500)

runWithServantHandler :: IO a -> IO (Either ServantErr a)
runWithServantHandler action = handleAny servantErrHandler (action >>= (return . Right))

servantErrHandler :: SomeException -> IO (Either ServantErr a)
servantErrHandler e = return $ Left $ err500 { errBody = pack (show e)}
