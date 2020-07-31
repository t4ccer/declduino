{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MultiWayIf #-}

module Board where

import Data.Text (pack, unpack)
import Data.Yaml 
import Prelude hiding ((>>))
import Data.String
import Data.List (nub, (\\))

data Error =
      YamlParserError
    | UnknownBoardError String
    | UnknownComponentError String
    | UnknownReporterError String
    | ComponentNameConfilctError String
    | BoardSpecificError String
    deriving (Show, Eq)

type Seconds = Int
type Miliseconds = Int

type Result a = Either Error a
toResult ::  Error -> Either ParseException (Result BoardType) -> Result BoardType
toResult e (Left _) = Left e
toResult _ (Right v) = v


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
            | t == "on-change" -> do
                d <- v .: pack "debounce"
                return $ Right (OnChange d)
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
            | t == "pwm-output" -> do
                p <- v .: pack "pin"
                return $ Right (PWMOutputComponent n p (-1)) 

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

hasNameConfilcts :: Device -> Result Device
hasNameConfilcts dev = if null repetitions
        then Right dev
        else Left $ ComponentNameConfilctError (head repetitions)
        where 
            names = map component_name $ components dev
            repetitions = names \\ nub names

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs