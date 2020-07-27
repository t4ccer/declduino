{-# LANGUAGE DeriveDataTypeable, DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-cse #-}

module Parameters where

import System.Console.CmdArgs

data Parameters = Parameters 
    { p_ssid  :: String
    , p_pass  :: String
    , p_mqtt  :: String
    , p_board :: String
    , p_device_name :: String
    , p_files :: [FilePath]
    } deriving (Show, Data, Typeable)

parameters :: Parameters
parameters = Parameters
    { p_ssid        = def &= name "s" &= name "ssid"  &= groupname "OPTIONS" &= typ "<wifi-ssid>"   &= help "Overrides WiFI SSID"
    , p_pass        = def &= name "p" &= name "pass"  &= groupname "OPTIONS" &= typ "<wifi-pass>"   &= help "Overrides WiFi password"
    , p_mqtt        = def &= name "m" &= name "mqtt"  &= groupname "OPTIONS" &= typ "<mqtt-addr>"   &= help "Overrides MQTT broker address"
    , p_board       = def &= name "d" &= name "board" &= groupname "OPTIONS" &= typ "<board-type>"  &= help "Overrides board type"
    , p_device_name = def &= name "n" &= name "name"  &= groupname "OPTIONS" &= typ "<device-name>" &= help "Overrides device name" &= explicit
    , p_files       = def &= args &= typ "FILES"
    }
    &= summary "declduino by t4ccer"
    &= help "A declarative way to create Arudino-based IOT devices"
    &= program "declduino"
    &= helpArg [explicit, name "help", name "h", help "Take a guess"]
    &= versionArg [ignore]

-- applyParameters :: Parameters -> Device -> Device
-- applyParameters params device = Device
--     { port = 0

--     }

-- apply :: (Eq a) => a -> a -> a -> a
-- apply val new def'
--     | new == def' = val
--     | otherwise = new
