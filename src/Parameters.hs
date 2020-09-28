{-# LANGUAGE DeriveDataTypeable, DuplicateRecordFields, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-cse #-}

module Parameters where

import System.Console.CmdArgs
import Data.String
import Data.String.Interpolate (i)
import Board
import FancyLogger

data Parameters = 
      NoMode
    | Generate 
        { p_ssid        :: String
        , p_pass        :: String
        , p_mqtt        :: String
        , p_mqtt_port   :: Int
        , p_board       :: String
        , p_device_name :: String
        , p_verbosity   :: LogLevel
        , p_format      :: LogFormat
        , p_files       :: [FilePath]
        } 
    | Hass
        { p_output      :: FilePath
        , p_device_name :: String
        , p_verbosity   :: LogLevel
        , p_format      :: LogFormat
        , p_files       :: [FilePath]
        }
    deriving (Show, Data, Typeable)

parameters :: [Parameters]
parameters = 
    [Generate
        { p_ssid        = def  &= name "s" &= name "ssid"      &= groupname "OPTIONS" &= typ "<wifi-ssid>"   &= explicit &= help "Overrides WiFI SSID"
        , p_pass        = def  &= name "p" &= name "pass"      &= groupname "OPTIONS" &= typ "<wifi-pass>"   &= explicit &= help "Overrides WiFi password"
        , p_mqtt        = def  &= name "m" &= name "mqtt"      &= groupname "OPTIONS" &= typ "<mqtt-addr>"   &= explicit &= help "Overrides MQTT broker address"
        , p_mqtt_port   = 1883             &= name "port"      &= groupname "OPTIONS" &= typ "<mqtt-port>"   &= explicit &= help "Overrides MQTT broker port"
        , p_board       = def  &= name "b" &= name "board"     &= groupname "OPTIONS" &= typ "<board-type>"  &= explicit &= help "Overrides board type"
        , p_device_name = def  &= name "n" &= name "name"      &= groupname "OPTIONS" &= typ "<device-name>" &= explicit &= help "Overrides device name. Note that this option will override name of all devices" 
        , p_verbosity   = Info &= name "v" &= name "verbosity" &= groupname "OPTIONS" &= typ "<mode>"        &= explicit &= help "Sets verbosity. mode = <debug|info|warning|error>. Default: info"
        , p_format      = List             &= name "format"    &= groupname "OPTIONS" &= typ "<format>"      &= explicit &= help "Sets output format. format = <list|json>. Default: list"
        , p_files       = def  &= args &= typ "FILES"
        }
    , Hass
        { p_output      = "configuration.yaml" &= name "o" &= name "output" &= explicit &= help "Configuration output file. Default: configuration.yaml" &= typ "<output.yaml>" &= groupname "OPTIONS"
        , p_device_name = def  &= name "n" &= name "name"      &= groupname "OPTIONS" &= typ "<device-name>" &= explicit &= help "Overrides device name. Note that this option will override name of all devices"
        , p_verbosity   = Info &= name "v" &= name "verbosity" &= groupname "OPTIONS" &= typ "<mode>"        &= explicit &= help "Sets verbosity. mode = <debug|info|warning|error>. Default: info"
        , p_format      = List             &= name "format"    &= groupname "OPTIONS" &= typ "<format>"      &= explicit &= help "Sets output format. format = <list|json>. Default: list"
        , p_files       = def  &= args &= typ "FILES"
        }
    , NoMode &= auto
    ]
    &= summary "declduino by t4ccer"
    &= program "declduino"
    &= helpArg [explicit, name "help", name "h", help "Take a guess"]
    &= versionArg [ignore]

applyParameters :: Parameters -> Device -> FancyLogger Device
applyParameters params device = case params of
    Generate {} -> if p_board params == "" 
        then 
            returnWithLog (Log Debug [i|Applied generate parameters for '#{device_name device}'|]) new_dev
        else do
            board_type <- fromString $ p_board params
            returnWithLog (Log Debug [i|Applied generate parameters for '#{device_name device}'|]) $ new_dev {board = board_type}
        where
             new_dev = device
                     { device_name = apply (device_name device) (p_device_name params)   ""
                     , ssid        = apply (ssid device)        (p_ssid params)          ""
                     , pass        = apply (pass device)        (p_pass params)          ""
                     , mqtt        = apply (mqtt device)        (p_mqtt params)          ""
                     , port        = apply (port device)        (p_mqtt_port params) 1883
                     }
    Hass     {} -> returnWithLog (Log Debug [i|Applied hass parameters for '#{device_name device}'|]) device
        { device_name = apply (device_name device) (p_device_name params) ""
        }
    NoMode   {} -> returnError "This should never happen"

apply :: (Eq a) => a -> a -> a -> a
apply val new def'
    | new == def' = val
    | otherwise = new
