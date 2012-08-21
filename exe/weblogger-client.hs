module Main where

import System.Console.CmdArgs

import Application.WebLogger.Client.ProgType
import Application.WebLogger.Client.Command

main :: IO () 
main = do 
  putStrLn "weblogger-client"
  param <- cmdArgs mode
  commandLineProcess param