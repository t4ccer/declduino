{-# LANGUAGE RebindableSyntax #-}

module CodeDecl where

import CodeGen
import Data.Text (pack)
import Data.Yaml 
import Prelude hiding ((>>))
import Data.String (fromString, IsString)
import Data.List (nub)

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

data ReportMode = 
      OnChange
    | UnknownReportMode String
    deriving(Show, Eq)

instance IsString ReportMode where
    fromString "on-change" = OnChange
    fromString x = UnknownReportMode x

data Component = 
      DigitalOutputComponent
      { component_name :: String
      , pin :: Int
      }
    | DigitalInputComponent
      { component_name :: String
      , pin :: Int
      , reports :: [ReportMode]
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
                r_strs <- v .: pack "reports"
                return DigitalInputComponent 
                    { component_name = n
                    , pin = p
                    , reports = fmap fromString r_strs
                    }
    parseJSON _ = error "Component parse error"

-- For do noatation
infixr 0 >>
(>>) :: a -> [a] -> [a]
(>>) a = (<>) [a]

end :: [a]
end = mempty

deviceToCode :: Device -> [CodeToken]
deviceToCode dev
    | board dev == ESP32 = do
        Include "\"WiFi.h\""
        Include "<PubSubClient.h>"

        VarDecl "WiFiClient" "espClient" []
        Semicolon
        VarDecl "PubSubClient" "client" [Variable "espClient"]
        Semicolon
        end
        <> globals
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
        Function Void "loop" [] ((do
            Call "client.loop" []
            Semicolon
            NL
            end)
            <> loopHandls)
        end)
        
    | otherwise = error "Unsupported board"
    where
        ssid' = ssid dev
        pass' = pass dev
        mqtt' = mqtt dev
        port' = port dev
        funcs = map (componentToCallback dev) $ components dev
        conds = map (componentToCallbackCond dev) $ components dev
        subs  = concatMap (componentToSubs dev) $ components dev
        pins  = concatMap (componentToPinMode dev) $ components dev
        loopHandls = concatMap (componentToLoopHandle dev) $ components dev
        globals = concatMap (componentToGlobals dev) $ components dev


componentToLoopHandle :: Device -> Component -> [CodeToken]
componentToLoopHandle dev comp = case board dev of
    ESP32 -> case comp of
        DigitalOutputComponent {} -> do
            end
        DigitalInputComponent {} -> do
            Call func_name []
            Semicolon
            NL
            end
        _ -> undefined
    UnknownBoard x -> error ("Unknown board " ++ x)
    where
        func_name = "handle_" ++ component_name comp

componentToCallbackCond :: Device -> Component -> CodeToken
componentToCallbackCond dev comp = case board dev of
    ESP32 -> case comp of
        DigitalOutputComponent {} ->
            If [Call "String" [Variable "topic"], Op Equals, Value (StringLit ("declduino/"++device_name dev++"/"++component_name comp))] (do 
                Call func_name [Variable "message", Variable "length"]
                Semicolon
                NL
                end)
        DigitalInputComponent {} -> NL
        _ -> undefined
    UnknownBoard x -> error ("Unknown board " ++ x)
    where
        func_name = "handle" ++ component_name comp


componentToCallback :: Device -> Component -> CodeToken
componentToCallback dev comp = case board dev of
    ESP32 -> case comp of 
        DigitalOutputComponent _ c_pin ->
            Function Void func_name [Argument "byte*" "message", Argument "unsigned int" "length"] (do
            If [Value (Variable "length"), Op NotEquals, Value (IntLit 1)] (do
                Return[]
                end)
            If [Value (Variable "message[0]"), Op Equals, Value (CharLit '1')] (do
                Call "digitalWrite" [IntLit c_pin, Variable "HIGH"] 
                Semicolon 
                NL
                end)
            Else (do 
                If [Value (Variable "message[0]"), Op Equals, Value (CharLit '0')] (do
                    Call "digitalWrite" [IntLit c_pin, Variable "LOW"] 
                    Semicolon
                    NL
                    end)
                end)
            end)
        DigitalInputComponent _ c_pin reps ->
                Function Void func_name [] (concatMap gen_reporter $ nub reps)
            where 
                gen_reporter OnChange = do
                        VarDecl "int" "new_state" []
                        Semicolon
                        Assigment "new_state" [Call "digitalRead" [IntLit c_pin]]
                        Semicolon
                        NL
                        If [Value (Variable "new_state"), Op NotEquals,  Value (Variable (component_name comp ++ "_state_" ++ show c_pin))] (do
                            Assigment (component_name comp ++ "_state_" ++ show c_pin) [Value (Variable "new_state")]
                            Semicolon
                            NL
                            VarDecl "char" "x[2]" []
                            Semicolon
                            NL
                            Assigment "x[0]" [Value (CharLit '0'), Op Plus, Value (Variable "new_state")]
                            Semicolon
                            Assigment "x[1]" [Value (IntLit 0)]
                            Semicolon
                            NL
                            Call "client.publish" [StringLit ("declduino/"++device_name dev++"/"++component_name comp), Variable "x"]
                            Semicolon
                            NL
                            Call "delay" [IntLit 10]
                            Semicolon
                            end)
                        end
        UnknownComponent x -> error ("Unknown component "++x)
    UnknownBoard x -> error ("Unknown board " ++ x)
    where
        func_name = "handle_" ++ component_name comp

componentToGlobals :: Device -> Component -> [CodeToken]
componentToGlobals dev comp = case board dev of
    ESP32 -> case comp of 
        DigitalOutputComponent {} -> do
            end
        DigitalInputComponent _ c_pin _ -> do
            VarDecl "int" (component_name comp ++ "_state_" ++ show c_pin) [Variable "LOW"]
            Semicolon 
            NL
            end
        UnknownComponent x -> error ("Unknown component "++x)
    UnknownBoard x -> error ("Unknown board " ++ x)

componentToPinMode :: Device -> Component -> [CodeToken]
componentToPinMode dev comp = case board dev of
    ESP32 -> case comp of 
        DigitalOutputComponent _ c_pin -> do
            Call "pinMode" [IntLit c_pin, Variable "OUTPUT"] 
            Semicolon 
            NL
            end
        DigitalInputComponent _ c_pin _ -> do
            Call "pinMode" [IntLit c_pin, Variable "INPUT"] 
            Semicolon 
            NL
            end
        UnknownComponent x -> error ("Unknown component "++x)
    UnknownBoard x -> error ("Unknown board " ++ x)


componentToSubs :: Device -> Component -> [CodeToken]
componentToSubs dev comp  = case board dev of
    ESP32 -> case comp of 
        DigitalOutputComponent _ _ -> do
            Call "client.subscribe" [StringLit ("declduino/"++device_name dev++"/"++component_name comp)]
            Semicolon 
            NL
            end
        DigitalInputComponent {} -> do
            end
        UnknownComponent x -> error ("Unknown component "++x)
    UnknownBoard x -> error ("Unknown board " ++ x)
