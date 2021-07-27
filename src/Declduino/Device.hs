{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleContexts       #-}
module Declduino.Device where

import           Control.Monad
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Text        (Text)
import           Data.Yaml
import           GHC.Generics

import Declduino.App

newtype Seconds = Seconds { getSeconds :: Int }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Num, ToJSON, FromJSON)

newtype Miliseconds = Miliseconds { getMiliseconds :: Int }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Num, ToJSON, FromJSON)

data BoardType =
      Esp32
    deriving (Show, Eq, Generic)

instance FromJSON BoardType where
  parseJSON v@(String s) =
    case s of
      "esp32" -> pure Esp32
      _       -> prependFailure "parsing board type failed, " (unexpected v)
  parseJSON invalid    =
    prependFailure "parsing board type failed, "
      (typeMismatch "String" invalid)

-- TODO OnRequest
data Reporter
  = OnChange
    { reporterDebounce :: Miliseconds
    }
  | OnTime
    { reporterInterval :: Seconds
    }
    deriving (Show, Eq, Generic)

instance FromJSON Reporter where
  parseJSON = withObject "reporter" \v -> do
    type' :: Text <- v .: "type"
    case type' of
      "on-change" -> do
        reporterDebounce <- v .: "debounce"
        pure OnChange{..}
      "on-time" -> do
        reporterInterval <- v .: "interval"
        pure OnTime{..}
      _       -> prependFailure "parsing board type failed, " (unexpected (String type'))

data Component
  = DigitalOutput
    { componentName :: Text
    , componentPin  :: Int
    }
  | DigitalInput
    { componentName      :: Text
    , componentReporters :: [Reporter]
    , componentPin       :: Int
    }
  | PWMOutput
    { componentName          :: Text
    , componentPin           :: Int
    , componentSensorChannel :: Int
    }
  | DS18B20
    { componentName          :: Text
    , componentReporters     :: [Reporter]
    , componentPin           :: Int
    , componentSensorAddress :: Either Int Text
    }
    deriving (Show, Eq, Generic)

instance FromJSON Component where
  parseJSON = withObject "component" $ \v -> do
    type' :: Text <- v .: "type"
    case type' of
      "digital-output" -> do
        componentName <- v .: "name"
        componentPin  <- v .: "pin"
        pure DigitalOutput{..}
      "digital-input" -> do
        componentName      <- v .: "name"
        componentReporters <- v .: "reporters"
        componentPin       <- v .: "pin"
        pure DigitalInput{..}
      "pwm-output" -> do
        componentName <- v .: "name"
        componentPin  <- v .: "pin"
        let componentSensorChannel = -1
        pure PWMOutput{..}
      "ds18b20" -> do
        componentName <- v .: "name"
        componentReporters <- v .: "reporters"
        componentPin  <- v .: "pin"
        componentSensorAddressIdx <- v .:? "index"
        componentSensorAddressAddr <- v .:? "address"
        componentSensorAddress <-
          case componentSensorAddressIdx of
            Just idx -> return $ Left idx
            Nothing -> case componentSensorAddressAddr of
                         Just addr -> return $ Right addr
                         Nothing ->  prependFailure "parsing ds18b20 failed, "
                                       (unexpected (String "Only one of <index | address> can be set"))
        pure DS18B20{..}
      _       -> prependFailure "parsing board type failed, " (unexpected (String type'))

hasDs18b20 :: MonadReader Device m => m Bool
hasDs18b20 = do
  Device{..} <- ask
  pure $ go deviceComponents
  where
    go [] = False
    go ((DS18B20{}):_) = True
    go (_:xs) = go xs

data Device = Device
    { deviceBoard      :: BoardType
    , deviceName       :: Text
    , deviceSsid       :: Text
    , devicePass       :: Text
    , deviceMqtt       :: Text
    , devicePort       :: Int
    , deviceComponents :: [Component]
    }
    deriving (Show, Eq, Generic)

instance FromJSON Device where
  parseJSON (Object v) = do
    deviceBoard      <- v .: "board"
    deviceName       <- v .: "name"
    deviceSsid       <- v .: "ssid"
    devicePass       <- v .: "pass"
    deviceMqtt       <- v .: "mqtt"
    devicePort       <- v .: "port"
    deviceComponents <- v .: "components"
    pure $ Device{..}
  parseJSON invalid    =
    prependFailure "parsing Coord failed, "
      (typeMismatch "Object" invalid)


