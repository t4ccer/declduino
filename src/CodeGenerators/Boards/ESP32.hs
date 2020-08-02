 {-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-} --Becouse of dsl
{-# LANGUAGE ScopedTypeVariables #-}

-- module CodeGenerators.Boards.ESP32 (generateCode) where
module CodeGenerators.Boards.ESP32  where
    
import CodeGenerators.LanguageDSL.ESP32Specific
import CodeGenerators.LanguageDSL.CGenerator
import Board
import Error
import Prelude hiding ((+), (==), (*), (-), (/=), (>=), (^))
import qualified Prelude ((+), (*))
import Data.Char (ord)

generateCode :: Device -> Result String
generateCode dev = do
    assigned <- assignPWMChannels dev
    return $ base assigned

assignPWMChannels :: Device -> Result Device
assignPWMChannels dev 
    | length pwms Prelude.> 16 = Left (BoardSpecificError ("ESP32 can support only 16 pwm outputs, You declared " ++ show (length pwms)))
    | otherwise = return $ dev { components=assigned }
    where
        pwms = [PWMOutputComponent n p (-1) | (PWMOutputComponent n p _) <- components dev]
        assigned = assign (components dev) 0
        assign :: [Component] -> Int -> [Component]
        assign [x] i = case x of
            PWMOutputComponent n p _ -> [PWMOutputComponent n p i]
            v -> [v]
        assign (x:xs) i = case x of
            PWMOutputComponent n p _ -> PWMOutputComponent n p i : assign xs (i Prelude.+ 1)
            v -> v : assign xs i 
        assign [] _ = []

base :: Device -> String
base dev =  generate $ do
    include "<stdio.h>"
    include "<WiFi.h>"
    include "<PubSubClient.h>"
    include "<ESPmDNS.h>"
    include "<WiFiUdp.h>"
    include "<ArduinoOTA.h>"
    comment "Includes"
    allIncludes

    comment "Globals"
    espClient :: LVal (Class WiFiClient) <- declareGlobal "espClient"
    client :: LVal (Class PubSubClient) <- declareGlobal "client(espClient)" -- Do not change name
    allGlobals

    restart :: Fun (IO ())
        <- defineNewFun "restart" () $ \ _ -> do
            scall delay (lit 1000)
            scall restartESP

    allCallbacks
    callback :: Fun (Ptr Char -> Ptr Byte -> Int -> IO ()) 
        <- defineNewFun "callback" ("topic" :> "message" :> "length") $ \ f topic message len -> do
            comment "Callback conditions"
            allSubs

    reconnect :: Fun (IO ())
        <- defineNewFun "reconnect" () $ \_ -> do
            comment "WiFi"
            iff (call wifiStatus /= lit 3)
                (do
                    scall wifiBegin (lit (ssid dev)) (lit (pass dev))
                    while (call wifiStatus /= lit 3) 
                        (scall delay (lit 500))
                    )

            comment "MQTT"
            iff (call mqttConnected == lit 0)
                (do
                    scall mqttSetServer (lit (mqtt dev)) (lit (port dev)) -- Port is also parameter
                    scall mqttSetCallback (funPtr callback)
                    scall mqttConnect (lit hostname)
                    mqttSubs
                    )

    setup :: Fun (IO ())
        <- defineNewFun "setup" () $ \setup -> do
            scall reconnect
            comment "OTA"
            scall arduinoOTAOnEnd $ funPtr restart
            scall arduinoOTASetPort (lit 3232) 
            scall arduinoOTASetHostname (lit hostname)
            scall arduinoOTABegin
            allSetups
    
    loop :: Fun (IO ())
        <- defineNewFun "loop" () $ \loop -> do
            scall reconnect
            scall mqttLoop
            scall arduinoOTALoop
            allLoopHandlers
            
    noCode --Yes, it really has to be here
    where
        getCodeChunks f d = map (f d) $ components d
        allGlobals = flat $ getCodeChunks componentToGlobals dev
        allCallbacks = flat $ getCodeChunks componentToCallbacks dev
        allLoopHandlers = flatS $ getCodeChunks componentToLoopHandlers dev
        allIncludes = flat $ getCodeChunks componentToIncludes dev
        allSubs = flatS $ subscriptionsToCode $ concatMap (componentToSubscriptions dev) $ components dev
        allSetups = flatS $ map (componentToSetup dev) $ components dev
        mqttSubs = flatS $ subscriptionsToMQTT $ concatMap (componentToSubscriptions dev) $ components dev
        hostname = "declduino_"++device_name dev

componentToSetup :: Device -> Component -> Stmt () ()
componentToSetup _ comp = case comp of
    DigitalOutputComponent _ pin' -> do
        scall pinMode (lit pin') output
    DigitalInputComponent _ pin' _  -> do
        scall pinMode (lit pin') input
    PWMOutputComponent _ p c  -> do
        scall ledcSetup (lit c) (lit 5000) (lit p)
        scall ledcAttachPin (lit p) (lit c)

componentToIncludes :: Device -> Component -> Decl ()
componentToIncludes _ comp = case comp of
    DigitalOutputComponent {} -> noCode
    DigitalInputComponent {}  -> noCode
    PWMOutputComponent {}     -> noCode

componentToLoopHandlers :: Device -> Component -> Stmt () ()
componentToLoopHandlers _ comp = case comp of
    DigitalOutputComponent {} -> noCodeS
    DigitalInputComponent {}  -> do 
        stmt $ trustMe ("handle_" ++ component_name comp ++ "()")
        noCodeS
    PWMOutputComponent {}     -> noCodeS

componentToGlobals :: Device -> Component -> Decl ()
componentToGlobals _ comp = case comp of
    DigitalOutputComponent {} -> noCode
    DigitalInputComponent {}  -> do 
        _ :: LVal Int <- declareGlobal (component_name comp ++ "_state(0)")
        _ :: LVal Int <- declareGlobal (component_name comp ++ "_previousMillis(0)")
        noCode
    PWMOutputComponent {}     -> do 
        _ :: LVal Int <- declareGlobal (component_name comp ++ "_state_pwm(0)")
        _ :: LVal Int <- declareGlobal (component_name comp ++ "_state_mode(0)")
        noCode

        
componentToCallbacks :: Device -> Component -> Decl ()
componentToCallbacks dev comp = case comp of
    DigitalOutputComponent n pin'     -> do
        _ :: Fun (Ptr Byte -> Int -> IO()) 
            <- defineNewFun ("handle_" ++ n) ("msg" :> "len") $ \_ msg len -> do
                iff (len == lit 1) (do
                    v <- newvar "x"
                    v =: msg ! lit 0

                    ifte (v == lit (Byte $ ord '0'))
                        (scall digitalWrite (lit pin') (lit 0))
                        (ifte (v == lit (Byte $ ord '1'))
                            (scall digitalWrite (lit pin') (lit 1)) 
                            (iff (v == lit (Byte $ ord 's'))
                                (do 
                                    x <- newvar "x"
                                    x =: call digitalRead (lit pin')
                                    x =: x ^ lit 1
                                    scall digitalWrite (lit pin') x
                                    )))
                    )
        noCode --Yes, it really has to be here
    DigitalInputComponent n pin' reps -> do
        _ :: Fun (IO ())
            <- defineNewFun ("handle_" ++ n) () $ \_ -> do
                reporters
                noCodeS
        noCode
            where
                reporters = flatS genReporters
                genReporters = map genReporter reps
                genReporter (OnChange d) = do
                    newState <- newvar "newState"
                    newState =: call digitalRead (lit pin')
                    iff (newState /= state) (do
                        state =: newState
                        x :: LVal (Ptr Char) <- "x" =. arrayMalloc (lit 2)
                        x ! lit 0 =: call intToChar (newState + lit (ord '0'))
                        x ! lit 1 =: call intToChar (lit 0)
                        scall mqttPublish (lit ("declduino/"++device_name dev++"/"++n)) x
                        scall delay (lit d)
                        noCodeS)
                    where
                        state = externVar (n ++ "_state")

                genReporter (OnTime i) = do
                    ms :: LVal Int <- newvar "ms"
                    ms =: call millis
                    iff (ms - prevMs >= lit (1000 Prelude.* i)) (do
                        newState <- newvar "newState"
                        x :: LVal (Ptr Char) <- "x" =. arrayMalloc (lit 2)
                        x ! lit 0 =: call intToChar (newState + lit (ord '0'))
                        x ! lit 1 =: call intToChar (lit 0)
                        scall mqttPublish (lit ("declduino/"++device_name dev++"/"++n)) x
                        noCodeS)
                    noCodeS
                    where
                        prevMs = externVar (n ++ "_previousMillis")
    PWMOutputComponent n _ channel'   -> do
        _ :: Fun (Ptr Byte -> Int -> IO()) 
            <- defineNewFun ("handle_" ++ n ++ "_pwm") ("msg" :> "len") $ \_ msg len -> do
                state_pwm =: lit 0
                forFromTo "i" (lit 0) (lit 1) len $ \i -> do
                    state_pwm =: state_pwm * lit 10
                    state_pwm =: state_pwm + call toInt (msg ! i) - lit (ord '0')
                    noCodeS 
                scall ledcWrite (lit channel') (state_pwm * state_mode)
                noCodeS
        noCode
        _ :: Fun (Ptr Byte -> Int -> IO()) 
            <- defineNewFun ("handle_" ++ n ++ "_mode") ("msg" :> "len") $ \_ msg len -> do
                iff (len == lit 1) (do

                    state_mode =: call toInt (msg ! lit 0) - lit (ord '0')

                    scall ledcWrite (lit channel') (state_pwm * state_mode)
                    noCodeS)
                noCodeS
        noCode
        where
            state_pwm :: LVal Int
            state_pwm  = externVar (n ++ "_state_pwm")
            state_mode :: LVal Int
            state_mode = externVar (n ++ "_state_mode")

componentToSubscriptions :: Device -> Component -> [(String, String)]
componentToSubscriptions dev comp = case comp of
    DigitalOutputComponent n _ -> [("declduino/"++devName++"/"++n, "handle_"++n)]
    DigitalInputComponent {}   -> []
    PWMOutputComponent n _ _   -> 
        [ ("declduino/"++devName++"/"++n++"/pwm", "handle_"++n++"_pwm")
        , ("declduino/"++devName++"/"++n++"/mode", "handle_"++n++"_mode")
        ]
    where
        devName = device_name dev

subscriptionsToCode :: [(String, String)] -> [Stmt r ()]
subscriptionsToCode = map (stmt . trustMe . \(t, f) -> "if (String(topic) == \"" ++ t ++ "\") " ++ f ++ "(message, length)")

subscriptionsToMQTT :: [(String, String)] -> [Stmt r ()]
subscriptionsToMQTT = map (stmt . trustMe . \(t, _) -> "client.subscribe(\"" ++ t ++ "\")")

flat :: [Decl ()] -> Decl ()
flat = foldl (>>) noCode

flatS :: [Stmt () ()] -> Stmt () ()
flatS = foldl (>>) noCodeS
