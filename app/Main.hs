{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Options.Applicative
import           System.Environment
import           System.Exit

import           Declduino.App
import           Declduino.Cli
import           Declduino.Device
import           Declduino.Yaml

main :: IO ()
main = do
  params <- execParser modeParser
  -- print params
  let env = ()
  res <- runApp env $ run params
  case res of
    Left _  -> do
      exitFailure
    Right _ -> do
      exitSuccess

run :: Mode -> App ()
run params = case params of
  ModeGenerate params'      -> runGenerate params'
  ModeHomeAssistant params' -> runHomeAssistant params'

runGenerate :: GenerateParameters -> App ()
runGenerate GenerateParameters{..} = do
  logDebug "Running generate"
  devices :: [Device] <- traverse decodeFile paramFiles
  liftIO $ mapM_ print devices
  return ()

runHomeAssistant :: HomeAssistantParameters -> App ()
runHomeAssistant _ = return ()

debug :: String -> IO ()
debug args = withArgs (words args) (withProgName "declduino" main)

