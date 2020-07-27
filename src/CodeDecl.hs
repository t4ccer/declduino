{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, RebindableSyntax #-}

module CodeDecl where

import CodeGen
import GHC.Generics
import Data.Text ()
import Data.Yaml hiding ((.:))
import Prelude hiding ((>>), return)

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
    | board dev == "esp32" = do
        Include "<WiFi.h>"
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

compName :: Component -> String
compName = name::Component->String

devName :: Device -> String
devName = name::Device->String

componentToCallbackCond :: Device -> Component -> CodeToken
componentToCallbackCond dev comp = 
    If [Call "String" [Variable "topic"], Op Equals, Value (StringLit ("declduino/"++devName dev++"/"++compName comp))] (do 
        Call "handle_led" [Variable "message", Variable "length"]
        Semicolon
        NL
        end)

componentToFunc :: Component -> CodeToken
componentToFunc comp
    | componentType comp == DigitalOutput = 
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
            funcName = "handle_" ++ compName comp

componentToPinMode :: Component -> [CodeToken]
componentToPinMode comp 
    | componentType comp == DigitalOutput = do
         Call "pinMode" [IntLit (pin comp), Variable "OUTPUT"] 
         Semicolon 
         NL
         end
    
    | otherwise = error "Unsopported device"

componentToSubs :: Device -> Component -> [CodeToken]
componentToSubs dev comp 
    | componentType comp == DigitalOutput = do
        Call "client.subscribe" [StringLit ("declduino/"++devName dev++"/"++compName comp)]
        Semicolon 
        NL
        end
    | otherwise = error "Unsopported device"

infixr 0 >>
(>>) :: a -> [a] -> [a]
(>>) a = (<>) [a]

end :: [a]
end = mempty
