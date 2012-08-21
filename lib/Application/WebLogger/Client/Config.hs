{-# LANGUAGE OverloadedStrings #-}

module Application.WebLogger.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data WebLoggerConf = WebLoggerConf { 
  webloggerServerURL :: String
} deriving (Show)

getWebLoggerConf :: Config -> IO (Maybe WebLoggerConf)
getWebLoggerConf config = do  
  s <- C.lookup config "server" 
  return  (WebLoggerConf <$> s )
