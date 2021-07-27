{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}

module Declduino.Cli where

import           Options.Applicative
import Data.Text (Text)

import           Declduino.App
import           Declduino.Device

data GenerateParameters = GenerateParameters
  { paramVerbosity  :: LogLevel
  , paramDeviceName :: Maybe String
  , paramSsid       :: Maybe String
  , paramPass       :: Maybe String
  , paramMqttAddr   :: Maybe String
  , paramMqttPort   :: Maybe Int
  , paramBoard      :: Maybe BoardType
  , paramFiles      :: [FilePath]
  }
  deriving (Show)

data HomeAssistantParameters = HomeAssistantParameters
  { paramVerbosity  :: LogLevel
  , paramDeviceName :: Maybe String
  , paramOutput     :: FilePath
  , paramFiles      :: [FilePath]
  }
  deriving (Show)

data Mode
  = ModeGenerate      GenerateParameters
  | ModeHomeAssistant HomeAssistantParameters
  deriving (Show)

modeParser :: ParserInfo Mode
modeParser =
  info
    (helper <*> versionOption <*> programOptions)
    fullDesc

versionOption :: Parser (a -> a)
versionOption = infoOption "0.0.0-dev" (long "version" <> help "Show version")

logLevel :: ReadM LogLevel
logLevel = str @Text >>= \case
  "debug"   -> return LevelDebug
  "info"    -> return LevelInfo
  "warning" -> return LevelWarn
  "error"   -> return LevelError
  _         -> readerError "Accepted log levels: debug, info, warning, error"

boardType :: ReadM BoardType
boardType = str @Text >>= \case
  "esp32" -> return Esp32
  _       -> readerError "Accepted board types: esp32"

intOption :: Mod OptionFields Int -> Parser Int
intOption = option auto

verbosity :: Parser LogLevel
verbosity = option logLevel (long "verbosity" <> short 'v' <> value LevelInfo <> help "Set log verbosity level")

files :: Parser [FilePath]
files = some (argument str (metavar "FILES..."))

generateOptions :: Parser GenerateParameters
generateOptions = do
  paramVerbosity  <- verbosity
  paramDeviceName <- optional (strOption (long "name" <> short 'n' <> metavar "NAME" <> help "Overrides device name"))
  paramSsid       <- optional (strOption (long "ssid" <> short 's' <> metavar "SSID" <> help "Overrides WiFi SSID"))
  paramPass       <- optional (strOption (long "pass" <> short 'k' <> metavar "PASS" <> help "Overrides WiFi SSID "))
  paramMqttAddr   <- optional (strOption (long "mqtt" <> short 'm' <> metavar "ADDR" <> help "Overrides MQTT address"))
  paramMqttPort   <- optional (intOption (long "port" <> short 'p' <> metavar "PORT" <> help "Overrides MQTT port"))
  paramBoard      <- optional (option boardType (long "board" <> short 'b' <> metavar "BOARD" <> help "Overrides board type"))
  paramFiles      <- files
  pure GenerateParameters{..}

homeAssistantOptions :: Parser HomeAssistantParameters
homeAssistantOptions = do
  paramVerbosity  <- verbosity
  paramDeviceName <- optional (strOption (long "name" <> short 'n' <> metavar "NAME" <> help "Overrides device name"))
  paramOutput     <- strOption (long "output" <> short 'o' <> metavar "OUT" <> help "Output path")
  paramFiles      <- files
  pure HomeAssistantParameters{..}

programOptions :: Parser Mode
programOptions = hsubparser
  (  command "generate"      (info (ModeGenerate <$> generateOptions)
      (progDesc "Generate source code from provided `device` files."))
  <> command "homeassistant" (info (ModeHomeAssistant <$> homeAssistantOptions)
      (progDesc "Generate Home Assistant configuration for provided `device` files"))
  )
