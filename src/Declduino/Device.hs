{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
module Declduino.Device where

import           Data.Yaml
import           GHC.Generics

newtype Seconds = Seconds { getSeconds :: Int }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Num, ToJSON, FromJSON)

newtype Miliseconds = Miliseconds { getMiliseconds :: Int }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Num, ToJSON, FromJSON)

data BoardType =
      ESP32
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Reporter =
      OnChange Miliseconds
    | OnTime Seconds
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Component
  = DigitalOutput
    { componentName :: String
    , componentPin  :: Int
    }
  | DigitalInput
    { componentName      :: String
    , componentReporters :: [Reporter]
    , componentPin       :: Int
    }
  | PWMOutput
    { componentName          :: String
    , componentPin           :: Int
    , componentSensorchannel :: Int
    }
  | DS18B20
    { componentName         :: String
    , componentReporters    :: [Reporter]
    , componentPin          :: Int
    , componentSensorsName  :: String
    , componentsensorsIndex :: Int -- TODO Add address option
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Device = Device
    { deviceBoard      :: BoardType
    , deviceName       :: String
    , deviceSsid       :: String
    , devicePass       :: String
    , deviceMqtt       :: String
    , devicePort       :: Int
    , deviceComponents :: [Component]
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


