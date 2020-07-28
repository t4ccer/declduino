{-# LANGUAGE RebindableSyntax #-}

module CodeDecl where

import CodeGen
import Data.Text (pack)
import Data.Yaml 
import Prelude hiding ((>>))

data Device = Device
    { board :: String
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
        b  <- v .: pack "board"
        n  <- v .: pack "name"
        s  <- v .: pack "ssid"
        pa <- v .: pack "pass"
        m  <- v .: pack "mqtt"
        po <- v .: pack "port"
        c  <- v .: pack "components"
        return Device 
            { board = b
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
    deriving(Show, Eq)    

data Component = Component
    { component_type::ComponentType
    , component_name::String
    , pin::Int
    }
    deriving(Show)

instance FromJSON Component where
    parseJSON (Object v) = do
        t <- v .: pack "type"
        n <- v .: pack "name"
        p <- v .: pack "pin"
        return Component 
            { component_type = to_type t
            , component_name = n
            , pin = p
            }
        where
            to_type "digital-output" = DigitalOutput
            to_type _ = error "Component type parse error"
    parseJSON _ = error "Component parse error"



deviceToCode :: Device -> [CodeToken]
deviceToCode dev
    | board dev == "esp32" = do
        Include "\"WiFi.h\""
        Include "<PubSubClient.h>"

        VarDecl "WiFiClient" "espClient" []
        Semicolon
        VarDecl "PubSubClient" "client" [Variable "espClient"]
        Semicolon
        end 
        <> funcs 
        <> do
        Function Void "callback" [Argument "char*" "topic",Argument "byte*" "message", Argument "unsigned int" "length"] conds
        Function Void "setup" [] ((do 
            Call "WiFi.begin" [StringLit ssid', StringLit pass']
            Semicolon
            WhileLoop [Call "WiFi.status" [], Op NotEquals, Value (Variable "WL_CONNECTED")] (do
                Call "delay" [IntLit 500]
                Semicolon
                NL
                end)
            Call "client.setServer" [StringLit mqtt', IntLit port'] 
            Semicolon
            Call "client.setCallback" [Variable "callback"] 
            Semicolon
            Call "client.connect" [StringLit "esp32"] 
            Semicolon
            end)
            <> subs
            <> pins)
        end
        <> (do
        Function Void "loop" [] (do
            Call "client.loop" []
            Semicolon
            end)
        end)
        
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


componentToCallbackCond :: Device -> Component -> CodeToken
componentToCallbackCond dev comp = 
    If [Call "String" [Variable "topic"], Op Equals, Value (StringLit ("declduino/"++device_name dev++"/"++component_name comp))] (do 
        Call "handle_led" [Variable "message", Variable "length"]
        Semicolon
        NL
        end)

componentToFunc :: Component -> CodeToken
componentToFunc comp
    | component_type comp == DigitalOutput = 
        Function Void funcName [Argument "byte*" "message", Argument "unsigned int" "length"] (do
            If [Value (Variable "length"), Op NotEquals, Value (IntLit 1)] (do
                Return[]
                end)
            If [Value (Variable "message[0]"), Op Equals, Value (CharLit '1')] (do
                Call "digitalWrite" [IntLit pin', Variable "HIGH"] 
                Semicolon 
                NL
                end)
            Else (do 
                If [Value (Variable "message[0]"), Op Equals, Value (CharLit '0')] (do
                    Call "digitalWrite" [IntLit pin', Variable "LOW"] 
                    Semicolon
                    NL
                    end)
                end)
            end)
    | otherwise = error "Unsupported device"
        where
            pin' = pin comp
            funcName = "handle_" ++ component_name comp

componentToPinMode :: Component -> [CodeToken]
componentToPinMode comp 
    | component_type comp == DigitalOutput = do
         Call "pinMode" [IntLit (pin comp), Variable "OUTPUT"] 
         Semicolon 
         NL
         end
    
    | otherwise = error "Unsopported device"

componentToSubs :: Device -> Component -> [CodeToken]
componentToSubs dev comp 
    | component_type comp == DigitalOutput = do
        Call "client.subscribe" [StringLit ("declduino/"++device_name dev++"/"++component_name comp)]
        Semicolon 
        NL
        end
    | otherwise = error "Unsopported device"

infixr 0 >>
(>>) :: a -> [a] -> [a]
(>>) a = (<>) [a]

end :: [a]
end = mempty
