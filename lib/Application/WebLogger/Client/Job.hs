{-# LANGUAGE OverloadedStrings #-}

module Application.WebLogger.Client.Job where

import Debug.Trace

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as SC
import Data.Aeson.Types
import Data.Aeson.Encode as E
import Data.Aeson.Parser
import qualified Data.Attoparsec as A

import Network.HTTP.Types -- hiding (statusCode)
import Network.HTTP.Types.Status
import Network.HTTP.Conduit

import System.Directory 
import System.FilePath
import Unsafe.Coerce

import Application.WebLogger.Client.Config
import Application.WebLogger.Type
import qualified Data.ByteString as B

type Url = String 

-- | 
startCreate :: WebLoggerConf -> String -> IO () 
startCreate mc cnt = do 
  putStrLn "job started"
  cwd <- getCurrentDirectory
  let url = webloggerServerURL mc 
  let info = WebLoggerInfo cnt 
  response <- weblogger url ("upload") methodPost info
  putStrLn $ show response 


-- |
startGetList :: WebLoggerConf -> IO () 
startGetList mc = do 
  putStrLn "getlist: "
  let url = webloggerServerURL mc 
  r <- jsonFromServer url ("list") methodGet
  putStrLn $ show r 


-- |
jsonFromServer :: Url -> String -> Method -> IO (Either String (Result Value))
jsonFromServer url api mthd = do 
  request <- parseUrl (url </> api)
  withManager $ \manager -> do
    let requestjson = request { 
          method = mthd,
          requestHeaders = [ ("Accept", "application/json; charset=utf-8") ] 
          } 
    r <- httpLbs requestjson manager 
    if statusCode (responseStatus r) == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode (responseStatus r))) 

weblogger :: Url -> String -> Method -> WebLoggerInfo -> IO (Either String (Result Value))
weblogger url api mthd mi = do 
  request <- parseUrl (url </> api)
  -- debug
  print (url </> api)
  let mijson = E.encode (toJSON mi)
      myrequestbody = RequestBodyLBS mijson 
  let requestjson = request 
        { method = mthd
        , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
        , requestBody = myrequestbody } 
  print mijson
  -- end debug
  withManager $ \manager -> do
    let mijson = E.encode (toJSON mi)
        myrequestbody = RequestBodyLBS mijson 
    let requestjson = request 
          { method = mthd
          , requestHeaders = [ ("Accept", "application/json; charset=utf-8") ]
          , requestBody = myrequestbody } 
    r <- httpLbs requestjson manager 
    if statusCode (responseStatus r) == 200 
      then return . parseJson . SC.concat . C.toChunks . responseBody $ r
      else return (Left $ "status code : " ++ show (statusCode (responseStatus r))) 

parseJson :: (FromJSON a) => SC.ByteString -> Either String (Result a)
parseJson bs =
  let resultjson = trace (SC.unpack bs) $ A.parse json bs 
  in case resultjson of 
       (A.Done rest rjson) -> return (parse parseJSON rjson)
       _                 -> Left "parseJson" 