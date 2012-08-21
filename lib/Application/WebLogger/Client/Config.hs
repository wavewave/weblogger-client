{-# LANGUAGE OverloadedStrings #-}

module Application.WebLogger.Client.Config where

import Control.Applicative
import Data.Configurator as C
import Data.Configurator.Types

data WebLoggerClientConfiguration = WebLoggerClientConfiguration { 
  yesodcrudServerURL :: String,
  yesodcrudClientURL :: String
} deriving (Show)

getWebLoggerClientConfiguration :: Config -> IO (Maybe WebLoggerClientConfiguration)
getWebLoggerClientConfiguration config = do  
  s <- C.lookup config "server" 
  c <- C.lookup config "client" 
  return  (WebLoggerClientConfiguration  <$> s <*> c )
