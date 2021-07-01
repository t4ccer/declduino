{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS_GHC -fno-cse #-}

module Declduino.Cli where

import           Data.String
import           Data.String.Interpolate (i)
import           System.Console.CmdArgs

import           Declduino.App

data Parameters
    = Generate
        { paramVerbosity  :: LogLevel
        , paramSsid       :: String
        , paramPass       :: String
        , paramMqtt       :: String
        , paramMqttPort   :: Int
        , paramBoard      :: String
        , paramDeviceName :: String
        , paramFiles      :: [FilePath]
        }
    | HomeAssistant
        { paramVerbosity  :: LogLevel
        , paramOutput     :: FilePath
        , paramDeviceName :: String
        , paramFiles      :: [FilePath]
        }
    deriving (Show, Data, Typeable)

parameters :: [Parameters]
parameters =
    [Generate
        { paramSsid        = def  &= name "s" &= name "ssid"      &= groupname "OPTIONS" &= typ "<wifi-ssid>"   &= explicit &= help "Overrides WiFi SSID"
        , paramPass        = def  &= name "k" &= name "pass"      &= groupname "OPTIONS" &= typ "<wifi-pass>"   &= explicit &= help "Overrides WiFi key"
        , paramMqtt        = def  &= name "m" &= name "mqtt"      &= groupname "OPTIONS" &= typ "<mqtt-addr>"   &= explicit &= help "Overrides MQTT broker address"
        , paramMqttPort    = 1883 &= name "p" &= name "port"      &= groupname "OPTIONS" &= typ "<mqtt-port>"   &= explicit &= help "Overrides MQTT broker port"
        , paramBoard       = def  &= name "b" &= name "board"     &= groupname "OPTIONS" &= typ "<board-type>"  &= explicit &= help "Overrides board type"
        , paramDeviceName  = def  &= name "n" &= name "name"      &= groupname "OPTIONS" &= typ "<device-name>" &= explicit &= help "Overrides device name. Note that this option will override name of all devices"
        , paramVerbosity   = Info &= name "v" &= name "verbosity" &= groupname "OPTIONS" &= typ "<mode>"        &= explicit &= help "Sets verbosity. mode = <debug|info|warning|error>. Default: info"
        , paramFiles       = def  &= args &= typ "FILES"
        } &= help "Generate source code from provided `device` files."
    , HomeAssistant
        { paramOutput      = "configuration.yaml" &= name "o" &= name "output" &= explicit &= help "Configuration output file. Default: configuration.yaml" &= typ "<output.yaml>" &= groupname "OPTIONS"
        , paramDeviceName  = def  &= name "n" &= name "name"      &= groupname "OPTIONS" &= typ "<device-name>" &= explicit &= help "Overrides device name. Note that this option will override name of all devices"
        , paramVerbosity   = Info &= name "v" &= name "verbosity" &= groupname "OPTIONS" &= typ "<mode>"        &= explicit &= help "Sets verbosity. mode = <debug|info|warning|error>. Default: info"
        , paramFiles       = def  &= args &= typ "FILES"
        } &= help "Generate Home Assistant configuration for provided `device` files"
    ]
    &= summary "declduino by t4ccer"
    &= program "declduino"
    &= helpArg [explicit, name "help", name "h", help "Take a guess"]
    &= versionArg [ignore]
