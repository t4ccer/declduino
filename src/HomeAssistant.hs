{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}

module HomeAssistant (devicesToEntities) where

import Data.Text (pack)
import Data.Yaml
import Data.Aeson (genericToJSON, defaultOptions, Options(..))
import GHC.Generics
import Board
import Data.List

data Entity = Entity
    { platform      :: String
    , unique_id     :: String
    , payload_on    :: Maybe String
    , payload_off   :: Maybe String
    , command_topic :: Maybe String
    , state_topic   :: Maybe String
    , name          :: String
    , brightness_command_topic :: Maybe String
    }
    deriving(Generic, Show)
instance ToJSON Entity where 
    toJSON = genericToJSON defaultOptions {omitNothingFields = True}

data EntityList = EntityList 
    { lst_name :: String
    , entities :: [Entity]
    }
    deriving (Show, Generic) 
instance ToJSON EntityList where
    toJSON (EntityList n ns) = object [pack n .= ns]
instance {-# OVERLAPS #-} ToJSON [EntityList] where
    toJSON xs = object $ map (\x -> pack (lst_name x) .= entities x) xs

instance Semigroup EntityList where
    (EntityList n xs) <> (EntityList _ ys) = EntityList n (xs++ys)

emptyEntity :: Entity
emptyEntity = Entity
    { platform = "mqtt"
    , unique_id = ""
    , name = ""
    , payload_off = Nothing
    , payload_on = Nothing
    , command_topic = Nothing
    , state_topic = Nothing
    , brightness_command_topic = Nothing
    }

flatEL :: [EntityList] -> EntityList
flatEL [] = EntityList "" []
flatEL (x:xs) = foldl (<>) x xs

devicesToEntities :: [Device] -> [EntityList]
devicesToEntities = map flatEL . groupBy (\x y -> lst_name x == lst_name y) . sortOn lst_name . concatMap deviceToEntities

deviceToEntities :: Device -> [EntityList]
deviceToEntities dev = ents
    where
        ents = map (componentToEntity dev) comps
        comps = components dev

componentToEntity :: Device -> Component -> EntityList
componentToEntity dev comp = case comp of
    DigitalOutputComponent{} -> EntityList "light" [namedEntity
        { payload_on = Just "1"
        , payload_off = Just "0"
        , command_topic = Just baseTopic 
        }]
    DigitalInputComponent{}  -> EntityList "binary_sensor" [namedEntity
        { payload_on = Just "1"
        , payload_off = Just "0"
        , state_topic = Just baseTopic
        }]
    PWMOutputComponent{}     -> EntityList "light" [namedEntity
        { payload_on = Just "1"
        , payload_off = Just "0"
        , command_topic = Just (baseTopic ++ "/mode") 
        , brightness_command_topic = Just (baseTopic ++ "/pwm") 
        }]
    DS18B20Component{}       -> flatEL $ map mkEntity $ sensors comp
        where
            mkEntity (DS18B20Sensor n _) = EntityList "sensor" [namedEntity
                { state_topic = Just (baseTopic ++ "/" ++ n)
                , name = mkName ++ "_" ++ n
                , unique_id = mkName ++ "_" ++ n 
                }]
    where
        mkName = "declduino_" ++ device_name dev ++ "_" ++ component_name comp
        baseTopic = "declduino/" ++ device_name dev ++ "/" ++ component_name comp
        namedEntity :: Entity = emptyEntity {name = mkName, unique_id = mkName}
        