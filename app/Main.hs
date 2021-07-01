module Main where

import           System.Console.CmdArgs (cmdArgs, modes)
import           System.Environment

import           Declduino.Cli

main :: IO ()
main = do
  params <- cmdArgs (modes parameters)
  print params

debug :: String -> IO ()
debug args = withArgs (words args) (withProgName "declduino" main)
