{-# LANGUAGE MultiParamTypeClasses #-}

module Board where

import Data.Text (pack)
import Data.Yaml 
import Prelude hiding ((>>))
import Data.String (fromString, IsString)

type Seconds = Int

data BoardType = 
      ESP32
    | UnknownBoard String
    deriving(Show, Eq)

instance IsString BoardType where
    fromString "esp32" = ESP32
    fromString x = UnknownBoard x

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
instance FromJSON Device where
    parseJSON (Object v) = do
        str_b  <- v .: pack "board"
        n  <- v .: pack "name"
        s  <- v .: pack "ssid"
        pa <- v .: pack "pass"
        m  <- v .: pack "mqtt"
        po <- v .: pack "port"
        c  <- v .: pack "components"
        return Device 
            { board = fromString str_b
            , device_name = n
            , ssid = s
            , pass = pa
            , mqtt = m
            , port = po
            , components = c
            }
    parseJSON _ = error "Device parse error"

data ComponentType = 
      DigitalOutput
    | DigitalInput
    | UnknownComponentType String
    deriving(Show, Eq)    

instance IsString ComponentType where
    fromString "digital-output" = DigitalOutput
    fromString "digital-input" = DigitalInput
    fromString x = UnknownComponentType x

data ReportType = 
      OnChangeT
    | OnTimeT
    | UnknownReportT String
    deriving(Show, Eq)

instance IsString ReportType where
    fromString "on-change" = OnChangeT
    fromString "on-time" = OnTimeT
    fromString x = UnknownReportT x

data Reporter = 
      OnChange
    | OnTime Seconds
    | UnknownReporter String
    deriving(Show, Eq)

instance FromJSON Reporter where
    parseJSON (Object v) = do
        t_str <- v .: pack "type"
        let t = fromString t_str
        case t of
            OnChangeT -> return OnChange
            OnTimeT -> do
                s <- v .: pack "interval"
                return (OnTime s)
            UnknownReportT x -> return (UnknownReporter x)

        -- return (UnknownReporter "")
    parseJSON _ = error "Reporter parse error"

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
    | UnknownComponent String
    deriving(Show)

instance FromJSON Component where
    parseJSON (Object v) = do
        n <- v .: pack "name"
        str_t <- v .: pack "type"
        let t = fromString str_t

        case t of
            DigitalOutput -> do
                p <- v .: pack "pin"
                return DigitalOutputComponent 
                    { component_name = n
                    , pin = p
                    }
            DigitalInput -> do
                p <- v .: pack "pin"
                r <- v .: pack "reporters"
                
                return DigitalInputComponent 
                    { component_name = n
                    , pin = p
                    , reports = r
                    }
    parseJSON _ = error "Component parse error"