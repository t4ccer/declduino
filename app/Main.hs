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
  res <- runAppIO $ run params
  case res of
    Left _  -> do
      exitFailure
    Right _ -> do
      exitSuccess

run :: (MonadIO m, MonadFail m, MonadLogger m) => Mode -> m ()
run params = case params of
  ModeGenerate params'      -> runGenerate params'
  ModeHomeAssistant params' -> runHomeAssistant params'

runGenerate :: (MonadIO m, MonadFail m, MonadLogger m) => GenerateParameters -> m ()
runGenerate GenerateParameters{..} = do
  logDebugN "Running generate"
  devices :: [Device] <- traverse decodeFile paramFiles
  codes <- traverse generateCode devices
  liftIO $ mapM_ (\(code, fname) -> putStrLn fname >> putStrLn "" >> T.putStrLn code) $ zip codes $ fmap (changeExtension "ino") paramFiles
  -- liftIO $ mapM_ (\(code, fname) -> T.writeFile fname code) $ zip codes $ fmap (changeExtension "ino") paramFiles
  return ()

changeExtension :: String -> FilePath -> FilePath
changeExtension ext org =
  case reverse $ dropWhile (/= '.') $ reverse org of
    ""   -> org <> "." <> ext
    base -> base <> ext

runHomeAssistant :: (MonadIO m, MonadFail m, MonadLogger m) => HomeAssistantParameters -> m ()
runHomeAssistant _ = return ()

debug :: String -> IO ()
debug args = withArgs (words args) (withProgName "declduino" main)

