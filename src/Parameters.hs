{-# LANGUAGE DeriveDataTypeable, DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-cse #-}

module Parameters where

import System.Console.CmdArgs
import CodeDecl
import Data.String

data Parameters = Parameters 
    { p_ssid  :: String
    , p_pass  :: String
    , p_mqtt  :: String
    , p_mqtt_port :: Int
    , p_board :: String
    , p_device_name :: String
    , p_files :: [FilePath]
    } deriving (Show, Data, Typeable)

parameters :: Parameters
parameters = Parameters
    { p_ssid        = def &= name "s" &= name "ssid"  &= groupname "OPTIONS" &= typ "<wifi-ssid>"   &= explicit &= help "Overrides WiFI SSID"
    , p_pass        = def &= name "p" &= name "pass"  &= groupname "OPTIONS" &= typ "<wifi-pass>"   &= explicit &= help "Overrides WiFi password"
    , p_mqtt        = def &= name "m" &= name "mqtt"  &= groupname "OPTIONS" &= typ "<mqtt-addr>"   &= explicit &= help "Overrides MQTT broker address"
    , p_mqtt_port   = 1883            &= name "port"  &= groupname "OPTIONS" &= typ "<mqtt-port>"   &= explicit &= help "Overrides MQTT broker port"
    , p_board       = def &= name "d" &= name "board" &= groupname "OPTIONS" &= typ "<board-type>"  &= explicit &= help "Overrides board type"
    , p_device_name = def &= name "n" &= name "name"  &= groupname "OPTIONS" &= typ "<device-name>" &= explicit &= help "Overrides device name"
    , p_files       = def &= args &= typ "FILES"
    }
    &= summary "declduino by t4ccer"
    -- &= help "A declarative way to create Arudino-based IOT devices"
    &= program "declduino"
    &= helpArg [explicit, name "help", name "h", help "Take a guess"]
    &= versionArg [ignore]

applyParameters :: Parameters -> Device -> Device
applyParameters params device = device
    { board       = apply (board device)       (fromString $ p_board params) (UnknownBoard (p_board params))
    , device_name = apply (device_name device) (p_device_name params)   ""
    , ssid        = apply (ssid device)        (p_ssid params)          ""
    , pass        = apply (pass device)        (p_pass params)          ""
    , mqtt        = apply (mqtt device)        (p_mqtt params)          ""
    , port        = apply (port device)        (p_mqtt_port parameters) 1883
    }

apply :: (Eq a) => a -> a -> a -> a
apply val new def'
    | new == def' = val
    | otherwise = new
