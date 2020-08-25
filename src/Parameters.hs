{-# LANGUAGE DeriveDataTypeable, DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-cse #-}

module Parameters where

import System.Console.CmdArgs
import Board
import Data.String


data Parameters = 
      NoMode
    | Generate 
        { p_ssid        :: String
        , p_pass        :: String
        , p_mqtt        :: String
        , p_mqtt_port   :: Int
        , p_board       :: String
        , p_device_name :: String
        , p_files       :: [FilePath]
        } 
    | Hass
        { p_files       :: [FilePath]
        , p_output      :: FilePath
        }
    deriving (Show, Data, Typeable)

parameters :: [Parameters]
parameters = 
    [Generate
        { p_ssid        = def &= name "s" &= name "ssid"  &= groupname "OPTIONS" &= typ "<wifi-ssid>"   &= explicit &= help "Overrides WiFI SSID"
        , p_pass        = def &= name "p" &= name "pass"  &= groupname "OPTIONS" &= typ "<wifi-pass>"   &= explicit &= help "Overrides WiFi password"
        , p_mqtt        = def &= name "m" &= name "mqtt"  &= groupname "OPTIONS" &= typ "<mqtt-addr>"   &= explicit &= help "Overrides MQTT broker address"
        , p_mqtt_port   = 1883            &= name "port"  &= groupname "OPTIONS" &= typ "<mqtt-port>"   &= explicit &= help "Overrides MQTT broker port"
        , p_board       = def &= name "b" &= name "board" &= groupname "OPTIONS" &= typ "<board-type>"  &= explicit &= help "Overrides board type"
        , p_device_name = def &= name "n" &= name "name"  &= groupname "OPTIONS" &= typ "<device-name>" &= explicit &= help "Overrides device name"
        , p_files       = def &= args &= typ "FILES"
        }
    , Hass
        { p_files       = def &= args &= typ "FILES"
        , p_output      = "configuration.yaml" &= name "o" &= name "output" &= explicit &= help "Configuration output file" &= typ "<output.yaml>" &= groupname "OPTIONS"
        }
    , NoMode &= auto
    ]
    &= summary "declduino by t4ccer"
    &= program "declduino"
    &= helpArg [explicit, name "help", name "h", help "Take a guess"]
    &= versionArg [ignore]

applyParameters :: Parameters -> Device -> Result Device
applyParameters params device = if p_board params == ""
    then return new_dev
    else 
        case param_board of
            (Left e) -> Left e
            (Right v) -> return $ new_dev { board = v }
    where
        param_board :: Result BoardType
        param_board = fromString $ p_board params
        new_dev = device
            { device_name = apply (device_name device) (p_device_name params)   ""
            , ssid        = apply (ssid device)        (p_ssid params)          ""
            , pass        = apply (pass device)        (p_pass params)          ""
            , mqtt        = apply (mqtt device)        (p_mqtt params)          ""
            , port        = apply (port device)        (p_mqtt_port params) 1883
            }

apply :: (Eq a) => a -> a -> a -> a
apply val new def'
    | new == def' = val
    | otherwise = new
