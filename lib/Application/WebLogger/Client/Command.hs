module Application.WebLogger.Client.Command where

import Control.Applicative
import Data.Configurator
--
import Application.WebLogger.Client.ProgType
import Application.WebLogger.Client.Job
import Application.WebLogger.Client.Config

-- | 
commandLineProcess :: WebLogger_client -> IO ()
commandLineProcess (Create cfg mn) = do 
  putStrLn "create called"
  mc <- getWebLoggerConf =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startCreate mn) mc
{- commandLineProcess (Get cfg n) = do 
  putStrLn "get called"
  mc <- getWebLoggerClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startGet n) mc
commandLineProcess (Put cfg n mn) = do 
  putStrLn "put called"
  mc <- getWebLoggerClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (\c-> startPut c n mn) mc
commandLineProcess (Delete cfg n) = do 
  putStrLn "delete called"
  mc <- getWebLoggerClientConfiguration =<< load [Required cfg] 
  maybe (error "cannot parse config") (flip startDelete n) mc -}
commandLineProcess (GetList cfg) = do 
  putStrLn "getlist called"
  mc <- getWebLoggerConf =<< load [Required cfg] 
  maybe (error "cannot parse config") startGetList mc