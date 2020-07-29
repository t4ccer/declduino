{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}

module Board where

import Data.Text (pack, unpack)
import Data.Yaml 
import Prelude hiding ((>>))
import Data.String

data Error =
      YamlParserError
    | UnknownBoardError String
    | UnknownComponentError String
    | UnknownReporterError String
    deriving (Show, Eq)

type Seconds = Int

type Result a = Either Error a
toResult ::  Error -> Either ParseException (Result BoardType) -> Result BoardType
toResult e (Left _) = Left e
toResult _ (Right v) = v


data BoardType = 
      ESP32
    deriving(Show, Eq)

data Reporter = 
      OnChange
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
    deriving(Show)


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

instance {-# OVERLAPS #-} FromJSON (Result Reporter) where
    parseJSON (Object v) = do
        t <- v .: pack "type"
        if
            | t == "on-change" -> return $ Right OnChange
            | t == "on-time" -> do
                i <- v .: pack "interval"
                return $ Right (OnTime i)
            | otherwise -> return $ Left (UnknownReporterError t)
    parseJSON _ =  return $ Left YamlParserError

instance {-# OVERLAPS #-} FromJSON (Result Component) where
    parseJSON (Object v) = do
        n <- v .: pack "name"
        t <- v .: pack "type"
        if 
            | t == "digital-output" -> do
                p <- v .: pack "pin"
                return $ Right (DigitalOutputComponent n p) 
            | t == "digital-input" -> do 
                p <- v .: pack "pin"
                r <- v .: pack "reporters"
                let r' = sequenceA r
                case r' of
                    (Left e) -> return $ Left e
                    (Right rs) ->
                        return $ Right (DigitalInputComponent n p rs)
            | otherwise -> 
                return $ Left (UnknownComponentError t)
    parseJSON _ =  return $ Left YamlParserError


instance IsString (Result BoardType) where
    fromString "esp32" = Right ESP32
    fromString s = Left (UnknownBoardError s)
instance {-# OVERLAPS #-} FromJSON (Result BoardType) where
    parseJSON (String t) = return $ fromString (unpack t)
    parseJSON _ =  return $ Left YamlParserError


instance {-# OVERLAPS #-} FromJSON (Result Device) where
    parseJSON (Object v) = do
        b <- v .: pack "board"
        case b of
            (Left e) -> return $ Left e
            (Right b') -> do
                n  <- v .: pack "name"
                s  <- v .: pack "ssid"
                pa <- v .: pack "pass"
                m  <- v .: pack "mqtt"
                po <- v .: pack "port"
                c  <- v .: pack "components"
                let c' = sequenceA c
                case c' of
                    (Left e) -> return $ Left e
                    (Right cs) -> 
                        return $ Right Device 
                            { board = b'
                            , device_name = n
                            , ssid = s
                            , pass = pa
                            , mqtt = m
                            , port = po
                            , components = cs
                            }
    parseJSON _ =  return $ Left YamlParserError
