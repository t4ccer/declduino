{-# LANGUAGE DeriveDataTypeable, DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-cse #-}

module Parameters where

import System.Console.CmdArgs

data Parameters = Parameters 
    { ssid  :: String
    , pass  :: String
    , mqtt  :: String
    , board :: String
    , device_name :: String
    , files :: [FilePath]
    } deriving (Show, Data, Typeable)

parameters :: Parameters
parameters = Parameters
    { ssid        = def &= groupname "OPTIONS" &= typ "<wifi-ssid>"   &= help "Overrides WiFI SSID"
    , pass        = def &= groupname "OPTIONS" &= typ "<wifi-pass>"   &= help "Overrides WiFi password"
    , mqtt        = def &= groupname "OPTIONS" &= typ "<mqtt-addr>"   &= help "Overrides MQTT broker address"
    , board       = def &= groupname "OPTIONS" &= typ "<board-type>"  &= help "Overrides board type"
    , device_name = def &= groupname "OPTIONS" &= typ "<device-name>" &= help "Overrides device name" &= name "name" &= name "n" &= explicit
    , files       = def &= args &= typ "FILES"
    }
    &= summary "declduino by t4ccer"
    &= help "A declarative way to create Arudino-based IOT devices"
    &= program "declduino"
    &= helpArg [explicit, name "help", name "h", help "Take a guess"]
    &= versionArg [ignore]