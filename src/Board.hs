{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf, ScopedTypeVariables #-}

module Board where

import Data.Text (pack, unpack)
import Data.Yaml 
import Prelude hiding ((>>))
import Data.String
import Data.List (nub, (\\))
import FancyLogger

type Seconds = Int
type Miliseconds = Int

data BoardType = 
      ESP32
    deriving(Show, Eq)

data Reporter = 
      OnChange Miliseconds
    | OnTime Seconds
    deriving(Show, Eq)

data Component = 
      DigitalOutputComponent
      { component_name :: String
      , pin :: Int
      }
    | DigitalInputComponent
      { component_name :: String
      , pin :: Int
      , reports :: [Reporter]
      }
    | PWMOutputComponent
      { component_name :: String
      , pin :: Int
      , channel :: Int
      }
    | DS18B20Component
      { component_name :: String
      , pin :: Int
      , sensors :: [DS18B20Sensor]
      , reporters :: [Reporter]
      }
    deriving(Show)

data DS18B20Sensor = DS18B20Sensor 
    { sensor_name :: String 
    , index :: Int --TODO Add address option
    } deriving(Show)

data Device = Device
    { board :: BoardType
    , device_name::String
    , ssid::String
    , pass::String
    , mqtt::String
    , port::Int
    , components::[Component]
    }
    deriving(Show)

instance {-# OVERLAPS #-} FromJSON (FancyLogger Reporter) where
    parseJSON (Object v) = do
        decoded_type <- v .: pack "type"
        if
            | decoded_type == "on-change" -> do
                d <- v .: pack "debounce"
                return $ returnWithLog (Log Debug "Decoded on-change reporter") (OnChange d)
            | decoded_type == "on-time" -> do
                i <- v .: pack "interval"
                return $ returnWithLog (Log Debug "Decoded interval reporter") (OnTime i)
            | otherwise -> return $ returnError ("Unknown reporter type: " ++ decoded_type)
    parseJSON _ =  return $ returnError "Failed docoding reporter" 

instance {-# OVERLAPS #-} FromJSON (FancyLogger Component) where
    parseJSON (Object v) = do
        parsed_name :: FancyLogger String <- return <$> v .: pack "name"
        parsed_type <- v .: pack "type"
        if
            | parsed_type == "digital-output" -> do
                parsed_pin <- return <$> v .: pack "pin"
                let comp = DigitalOutputComponent <$> parsed_name <*> parsed_pin
                return $ appendLog (Log Debug "Parsed digital-output component") comp
            | parsed_type == "digital-input" -> undefined
            | otherwise -> return $ returnError "Unknown component type"
    parseJSON _ =  return $ returnError "Component parse error"

instance {-# OVERLAPS #-} FromJSON (FancyLogger DS18B20Sensor) where
    parseJSON (Object v) = do
        n <- v .: pack "name"
        i <- v .: pack "index"
        return $ return $ DS18B20Sensor n i
    parseJSON _ = return $ returnError "Failed decoding ds18b20 sensor"

instance IsString (FancyLogger BoardType) where
    fromString "esp32" = returnWithLog (Log Debug "Decoded board type from string (esp32)") ESP32
    fromString s = returnError ("Failed decoding board type '" ++ s++"'")

instance {-# OVERLAPS #-} FromJSON (FancyLogger BoardType) where
    parseJSON (String t) = return $ fromString (unpack t)
    parseJSON _ =  return $ returnError "Failed yaml decoding"


instance {-# OVERLAPS #-} FromJSON (FancyLogger Device) where
    parseJSON (Object v) = do
        parsed_board_type <-            v .: pack "board"
        parsed_name       <- return <$> v .: pack "name"
        parsed_ssid       <- return <$> v .: pack "ssid"
        parsed_pass       <- return <$> v .: pack "pass"
        parsed_mqtt       <- return <$> v .: pack "mqtt"
        parsed_port       <- return <$> v .: pack "port"
        parsed_components <- sequenceA <$> v .: pack "components"

        return $ Device <$> parsed_board_type <*> parsed_name <*> parsed_ssid <*> parsed_pass <*> parsed_mqtt <*> parsed_port <*> parsed_components
    parseJSON _ = return $ returnError "Device parse error"

hasNameConfilcts :: Device -> FancyLogger Device
hasNameConfilcts dev = if null repetitions
        then returnWithLog (Log Debug "Checked for components name conflicts") dev
        else returnError ("Components name conflict: " ++ head repetitions)
        where 
            names = map component_name $ components dev
            repetitions = names \\ nub names
