{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Text.IO            as T
import           Options.Applicative
import           System.Environment
import           System.Exit

import           Declduino.App
import           Declduino.Cli
import           Declduino.CodeGenerator
import           Declduino.Device
import           Declduino.Yaml

main :: IO ()
main = do
  params <- execParser modeParser
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
  codes <- traverse generateCode devices
  liftIO $ mapM_ (\(code, fname) -> T.writeFile fname code) $ zip codes $ fmap (changeExtension "ino") paramFiles
  return ()

changeExtension :: String -> FilePath -> FilePath
changeExtension ext org =
  case reverse $ dropWhile (/= '.') $ reverse org of
    ""   -> org <> "." <> ext
    base -> base <> ext

runHomeAssistant :: HomeAssistantParameters -> App ()
runHomeAssistant _ = return ()

debug :: String -> IO ()
debug args = withArgs (words args) (withProgName "declduino" main)

