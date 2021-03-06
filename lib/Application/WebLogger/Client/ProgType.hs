{-# LANGUAGE DeriveDataTypeable #-}

module Application.WebLogger.Client.ProgType where 

import System.FilePath
import System.Console.CmdArgs hiding (name)

data WebLogger_client = 
          Create { config :: FilePath, modulename :: String }
        | GetList { config :: FilePath } 
          deriving (Show,Data,Typeable)

create :: WebLogger_client
create = Create { config = "test.conf"
                , modulename = "" &= typ "MODULENAME" &= argPos 0
                }
{-

        | Get    { config :: FilePath, name :: String } 
        | Put    { config :: FilePath, name :: FilePath, modulename :: String } 
        | Delete { config :: FilePath, name :: String } 


get :: WebLogger_client 
get = Get { config = "test.conf" 
          , name = "" &= typ "NAME" &= argPos 0 
          } 

put :: WebLogger_client 
put = Put { config = "test.conf"
          , name = "" &= typ "NAME" &= argPos 0 
          , modulename = "" &= typ "NAME" &= argPos 1
          }

delete :: WebLogger_client 
delete = Delete { config = "test.conf"
                , name = "" &= typ "NAME" &= argPos 0 
                }
-}

getlist :: WebLogger_client 
getlist = GetList { config = "test.conf" } 

mode = modes [ create, getlist ]

