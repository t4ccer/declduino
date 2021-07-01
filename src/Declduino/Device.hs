{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Declduino.Device where

newtype Seconds = Seconds { getSeconds :: Int }
  deriving (Show, Eq, Ord, Num)

newtype Miliseconds = Miliseconds { getMiliseconds :: Int }
  deriving (Show, Eq, Ord, Num)

data BoardType =
      ESP32
    deriving(Show, Eq)

data Reporter =
      OnChange Miliseconds
    | OnTime Seconds
    deriving(Show, Eq)

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
    deriving(Show)

data Device = Device
    { deviceBoard      :: BoardType
    , deviceName       :: String
    , deviceSsid       :: String
    , devicePass       :: String
    , deviceMqtt       :: String
    , devicePort       :: Int
    , deviceComponents :: [Component]
    }
    deriving (Show)

