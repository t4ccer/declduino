{-# LANGUAGE DataKinds #-}
module Main where

import           Options.Applicative
import           System.Environment
import           System.Exit

import           Declduino.App
import           Declduino.Cli

main :: IO ()
main = do
  params <- execParser modeParser
  print params
  let env = ()
  res <- runApp env $ run params
  case res of
    Left _  -> exitFailure
    Right _ -> exitSuccess

run :: Mode -> App ()
run params = case params of
  ModeGenerate params'      -> runGenerate params'
  ModeHomeAssistant params' -> runHomeAssistant params'

runGenerate :: GenerateParameters -> App ()
runGenerate _ = return ()

runHomeAssistant :: HomeAssistantParameters -> App ()
runHomeAssistant _ = return ()

debug :: String -> IO ()
debug args = withArgs (words args) (withProgName "declduino" main)

