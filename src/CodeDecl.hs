{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module CodeDecl where

import CodeGen
import GHC.Generics
import Data.Text ()
import Data.Yaml


data Device = Device
    { board :: String
    , name::String
    , ssid::String
    , pass::String
    , mqtt::String
    , port::Int
    , components::[Component]
    }
    deriving(Generic, Show)
instance FromJSON Device
instance ToJSON Device

data ComponentType = 
      DigitalOutput
    | DigitalInput
    deriving(Generic, Show, Eq)
instance FromJSON ComponentType
instance ToJSON ComponentType

data Component = Component
    { componentType::ComponentType
    , name::String
    , pin::Int
    }
    deriving(Generic, Show)
instance FromJSON Component
instance ToJSON Component

deviceToCode :: Device -> [CodeToken]
deviceToCode dev
    | board dev == "esp32" =
        [Include "<WiFi.h>"
        ,Include "<PubSubClient.h>"

        ,VarDecl "WiFiClient" "espClient" [],Semicolon,NL
        ,VarDecl "PubSubClient" "client" [Variable "espClient"],Semicolon,NL]

        ++funcs++
        
        [Function Void "callback" [Argument "char*" "topic",Argument "byte*" "message", Argument "unsigned int" "length"] 
            conds

        ,Function Void "setup" [] 
            ([Call "WiFi.begin" [StringLit ssid', StringLit pass'],Semicolon,NL
            ,WhileLoop [Call "WiFi.status" [], Op NotEquals, Value (Variable "WL_CONNECTED")] 
                [Call "delay" [IntLit 500],Semicolon,NL]    
            ,Call "client.setServer" [StringLit mqtt', IntLit port'],Semicolon,NL
            ,Call "client.setCallback" [Variable "callback"],Semicolon,NL
            ,Call "client.connect" [StringLit "esp32"],Semicolon,NL
            ]++
            subs++
            pins)
        ,Function Void "loop" []
            [Call "client.loop" []
            ,Semicolon
            ]
        ]
    | otherwise = error "Unsupported board"
    where
        ssid' = ssid dev
        pass' = pass dev
        mqtt' = mqtt dev
        port' = port dev
        funcs = map componentToFunc $ components dev
        conds = map (componentToCallbackCond dev) $ components dev
        subs  = concatMap (componentToSubs dev) $ components dev
        pins  = concatMap componentToPinMode $ components dev

compName :: Component -> String
compName = name::Component->String

devName :: Device -> String
devName = name::Device->String

componentToCallbackCond :: Device -> Component -> CodeToken
componentToCallbackCond dev comp = 
    If [Call "String" [Variable "topic"], Op Equals, Value (StringLit ("declduino/"++devName dev++"/"++compName comp))] 
        [Call "handle_led" [Variable "message", Variable "length"], Semicolon, NL]

componentToFunc :: Component -> CodeToken
componentToFunc comp
    | componentType comp == DigitalOutput = 
        Function Void funcName [Argument "byte*" "message", Argument "unsigned int" "length"] 
            [If [Value (Variable "length"), Op NotEquals, Value (IntLit 1)] [Return []]
                
            ,If [Value (Variable "message[0]"), Op Equals, Value (CharLit '1')] 
                [Call "digitalWrite" [IntLit pin', Variable "HIGH"],Semicolon,NL]
            ,Else [If [Value (Variable "message[0]"), Op Equals, Value (CharLit '0')] 
                [Call "digitalWrite" [IntLit pin', Variable "LOW"],Semicolon,NL]]
            ]
    | otherwise = error "Unsupported device"
        where
            pin' = pin comp
            funcName = "handle_" ++ compName comp

componentToPinMode :: Component -> [CodeToken]
componentToPinMode comp 
    | componentType comp == DigitalOutput = [Call "pinMode" [IntLit (pin comp), Variable "OUTPUT"],Semicolon,NL]
    | otherwise = error "Unsopported device"

componentToSubs :: Device -> Component -> [CodeToken]
componentToSubs dev comp 
    | componentType comp == DigitalOutput = [Call "client.subscribe" [StringLit ("declduino/"++devName dev++"/"++compName comp)],Semicolon,NL]
    | otherwise = error "Unsopported device"