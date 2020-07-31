{-# LANGUAGE RebindableSyntax #-}

module Board.ESP32 (deviceToCode) where

import Board
import CodeGen
import Prelude hiding ((>>))
import Data.List (nub)

-- For do noatation
(>>) :: a -> [a] -> [a]
(>>) a = (<>) [a]

end :: [a]
end = mempty

deviceToCode :: Device -> Result String
deviceToCode dev = do
    withAssignedPWM <- assignPWMChannels dev
    codify withAssignedPWM

codify :: Device -> Result String
codify dev = return $ tokensToCode $ do
    Include "\"WiFi.h\""
    Include "<PubSubClient.h>"
    Include "<ESPmDNS.h>"
    Include "<WiFiUdp.h>"
    Include "<ArduinoOTA.h>"

    VarDecl "WiFiClient" "espClient" []
    Semicolon
    VarDecl "PubSubClient" "client" [Variable "espClient"]
    Semicolon
    end
    <> globals
    <> funcs 
    <> do
    Function Void "callback" [Argument "char*" "topic",Argument "byte*" "message", Argument "unsigned int" "length"] conds
    Function Void "reconnect" [] (do
        If [Call "WiFi.status" [], Op NotEquals, Value (Variable "WL_CONNECTED")] (do
            Call "WiFi.begin" [StringLit ssid', StringLit pass']
            Semicolon
            WhileLoop [Call "WiFi.status" [], Op NotEquals, Value (Variable "WL_CONNECTED")] (do
                Call "delay" [IntLit 500]
                Semicolon
                NL
                end)
            end)
        WhileLoop [Op Negate, Call "client.connected" []] (do
            Call "client.setServer" [StringLit mqtt', IntLit port'] 
            Semicolon
            Call "client.setCallback" [Variable "callback"] 
            Semicolon
            Call "client.connect" [StringLit ("declduion-" ++ device_name dev)] 
            Semicolon
            end)
        end)
    Function Void "setup" [] ((do 
        Call "reconnect" []
        Semicolon

        Call "ArduinoOTA.onEnd" [Variable "[](){delay(1000);ESP.restart();}"]
        Semicolon
        Call "ArduinoOTA.setPort" [IntLit 3232]
        Semicolon
        Call "ArduinoOTA.begin" []
        Semicolon


        end)
        <> subs
        <> pins)
    end
    <> (do
    Function Void "loop" [] ((do
        Call "reconnect" []
        Semicolon
        Call "client.loop" []
        Semicolon
        NL
        Call "ArduinoOTA.handle" []
        Semicolon
        NL
        end)
        <> loopHandls)
    end)
    where
        getCodeChunks f d = concatMap (f d) $ components d
        getCodeChunk  f d = map (f d) $ components d

        ssid' = ssid dev
        pass' = pass dev
        mqtt' = mqtt dev
        port' = port dev

        funcs      = getCodeChunk  componentToCallback     dev
        conds      = getCodeChunk  componentToCallbackCond dev
        subs       = getCodeChunks componentToSubs         dev
        pins       = getCodeChunks componentToPinMode      dev
        loopHandls = getCodeChunks componentToLoopHandle   dev
        globals    = getCodeChunks componentToGlobals      dev

assignPWMChannels :: Device -> Result Device
assignPWMChannels dev 
    | length pwms > 16 = Left (BoardSpecificError ("ESP32 can support only 16 pwm outputs, You declared " ++ show (length pwms)))
    | otherwise = return $ dev { components=assigned }
    where
        pwms = [PWMOutputComponent n p (-1) | (PWMOutputComponent n p _) <- components dev]
        assigned = assign (components dev) 0
        assign [x] i = case x of
            PWMOutputComponent n p _ -> [PWMOutputComponent n p i]
            v -> [v]
        assign (x:xs) i = case x of
            PWMOutputComponent n p _ -> PWMOutputComponent n p i : assign xs (i+1)
            v -> v : assign xs i 
        assign [] _ = []

componentToLoopHandle :: Device -> Component -> [CodeToken]
componentToLoopHandle _ comp = case comp of
    DigitalOutputComponent {} -> do
        end
    DigitalInputComponent {} -> do
        Call func_name []
        Semicolon
        NL
        end
    PWMOutputComponent {} -> do
        end
    where
        func_name = "handle_" ++ component_name comp

componentToCallbackCond :: Device -> Component -> CodeToken
componentToCallbackCond dev comp = case comp of
    DigitalOutputComponent {} ->
        If [Call "String" [Variable "topic"], Op Equals, Value (StringLit ("declduino/"++device_name dev++"/"++component_name comp))] (do 
            Call func_name [Variable "message", Variable "length"]
            Semicolon
            NL
            end)
    PWMOutputComponent {} ->
        If [Call "String" [Variable "topic"], Op Equals, Value (StringLit ("declduino/"++device_name dev++"/"++component_name comp))] (do 
            Call func_name [Variable "message", Variable "length"]
            Semicolon
            NL
            end)
    DigitalInputComponent {} -> NL
    where
        func_name = "handle_" ++ component_name comp

componentToGlobals :: Device -> Component -> [CodeToken]
componentToGlobals _ comp =  case comp of 
    DigitalOutputComponent {} -> do
        end
    DigitalInputComponent _ c_pin _ -> do
        VarDecl "int" (component_name comp ++ "_state_" ++ show c_pin) [Variable "LOW"]
        Semicolon 
        NL
        VarDecl "unsigned long " (component_name comp ++ "_previousMillis") [IntLit 0]
        Semicolon 
        NL
        end
    PWMOutputComponent {} -> do
        end

componentToPinMode :: Device -> Component -> [CodeToken]
componentToPinMode _ comp = case comp of 
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
    PWMOutputComponent _ p c -> do
        Call "ledcSetup" [IntLit c, IntLit 5000, IntLit p]
        Semicolon
        NL
        Call "ledcAttachPin" [IntLit p, IntLit c]
        Semicolon
        end

componentToSubs :: Device -> Component -> [CodeToken]
componentToSubs dev comp  = case comp of 
    DigitalOutputComponent {} -> do
        Call "client.subscribe" [StringLit ("declduino/"++device_name dev++"/"++component_name comp)]
        Semicolon 
        NL
        end
    DigitalInputComponent {} -> do
        end
    PWMOutputComponent {} -> do
        Call "client.subscribe" [StringLit ("declduino/"++device_name dev++"/"++component_name comp)]
        Semicolon 
        NL
        end

componentToCallback :: Device -> Component -> CodeToken
componentToCallback dev comp = case comp of 
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
        Else (do 
            If [Value (Variable "message[0]"), Op Equals, Value (CharLit 's')] (do
                Call "digitalWrite" [IntLit c_pin, Variable("digitalRead("++ show c_pin ++")^1")] 
                Semicolon
                NL
                end)
            end)
        end)
    DigitalInputComponent _ c_pin reps ->
            Function Void func_name [] (concatMap gen_reporter $ nub reps)
        where 
            gen_reporter (OnChange d)= do
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
                        Call "delay" [IntLit d]
                        Semicolon
                        end)
                    end
            gen_reporter (OnTime i) = do
                VarDecl "unsigned long" "ms" []
                Semicolon
                Assigment "ms" [Call "millis" []]
                Semicolon 
                NL
                
                If [Value (Variable "ms"), Op Minus, Value (Variable (component_name comp ++ "_previousMillis")), Op GreaterOrEquals, Value (IntLit (i*1000))] (do
                    Assigment (component_name comp ++ "_previousMillis") [Value (Variable "ms")]
                    Semicolon
                    VarDecl "char" "x[2]" []
                    Semicolon
                    NL
                    Assigment "x[0]" [Value (CharLit '0'), Op Plus, Value (Variable ("digitalRead("++show c_pin++")"))]
                    Semicolon
                    Assigment "x[1]" [Value (IntLit 0)]
                    Semicolon
                    NL
                    Call "client.publish" [StringLit ("declduino/"++device_name dev++"/"++component_name comp), Variable "x"]
                    Semicolon
                    NL
                    end)

                end
    PWMOutputComponent _ _ c ->
        Function Void func_name [Argument "byte*" "message", Argument "unsigned int" "length"] (do
            If [Value (Variable "length"), Op NotEquals, Value (IntLit 2)] (do
                Return[]
                end)
            VarDecl "int" "value" [IntLit 0]
            Semicolon
            AddTo "value" [Value (IntLit 16), Op Asterisk, Trenary 
                [Value (Variable "message[0]"), Op LessOrEquals, Value (CharLit '9')] 
                [Value (Variable "message[0]"), Op Minus, Value (CharLit '0')] 
                [Value (Variable "message[0]"), Op Minus, Value (CharLit 'a'), Op Plus, Value (IntLit 10)]]
            Semicolon
            AddTo "value" [Trenary 
                [Value (Variable "message[1]"), Op LessOrEquals, Value (CharLit '9')] 
                [Value (Variable "message[1]"), Op Minus, Value (CharLit '0')] 
                [Value (Variable "message[1]"), Op Minus, Value (CharLit 'a'), Op Plus, Value (IntLit 10)]]
            Semicolon
            Call "ledcWrite" [IntLit c, Variable "value"]
            Semicolon
            end) 
    where
        func_name = "handle_" ++ component_name comp