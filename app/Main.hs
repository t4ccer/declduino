module Main where

import           Control.Monad.IO.Class
import           System.Console.CmdArgs (cmdArgs, modes)
import           System.Environment

import           Declduino.App
import           Declduino.Cli

main :: IO ()
main = do
  params <- cmdArgs (modes parameters)
  print params

debug :: String -> IO ()
debug args = withArgs (words args) (withProgName "declduino" main)

