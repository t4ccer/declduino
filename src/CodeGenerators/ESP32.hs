{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches #-} --Because of dsl
{-# LANGUAGE ScopedTypeVariables #-}

module CodeGenerators.ESP32  where
    
import ArduGen
import ArduGen.Base
import ArduGen.Libraries.PubSubClient
import qualified ArduGen.Libraries.DS18b20 as DS
import ArduGen.Arduino hiding (map)
import ArduGen.ESP32
import Board
import Prelude hiding ((+), (==), (*), (-), (/=), (>=), (^))
import qualified Prelude ((+), (*))
import Data.Char (ord)
import FancyLogger
import Logs

toStrPtr :: LVal a -> RVal (Ptr Char)
toStrPtr v = trustMe ("String("++unVal v ++ ").c_str()")

generateCode :: Device -> FancyLogger String
generateCode dev = do
    assigned <- assignPWMChannels dev
    returnWithLog (Log Debug ("Generated code for '" ++ device_name dev ++ "'")) $ base assigned

assignPWMChannels :: Device -> FancyLogger Device
assignPWMChannels dev 
    | length pwms Prelude.> 16 = returnError ("ESP32 can support only 16 pwm outputs, declared: " ++ show (length pwms))
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
    client :: LVal (Class PubSubClient) <- declareGlobal "client" -- Do not change name
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
            iff (call (mqttConnected client) == lit 0)
                (do
                    scall (mqttSetServer client) (lit (mqtt dev)) (lit (port dev)) 
                    scall (mqttSetCallback client) (funPtr callback)
                    scall (mqttConnect client) (lit hostname)
                    mqttSubs
                    )

    setup :: Fun (IO ())
        <- defineNewFun "setup" () $ \setup -> do
            client =: mqttInitializeClient espClient
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
            scall (mqttLoop client)
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
        scall ledcSetup (lit c) (lit 5000) (lit 8)
        scall ledcAttachPin (lit p) (lit c)
    DS18B20Component _ p _ _-> do
        ow =: call DS.initializeOneWire (lit p)
        sens =: DS.initializeDallasTemperature ow
        noCodeS
      where
          ow = externVar (component_name comp ++ "_oneWire")
          sens = externVar (component_name comp ++ "_sensors")

componentToIncludes :: Device -> Component -> Decl ()
componentToIncludes _ comp = case comp of
    DigitalOutputComponent {} -> noCode
    DigitalInputComponent {}  -> noCode
    PWMOutputComponent {}     -> noCode
    DS18B20Component {}       -> DS.requiredIncludes 

componentToLoopHandlers :: Device -> Component -> Stmt () ()
componentToLoopHandlers _ comp = case comp of
    DigitalOutputComponent {} -> noCodeS
    DigitalInputComponent {}  -> do 
        stmt $ trustMe ("handle_" ++ component_name comp ++ "()")
        noCodeS
    PWMOutputComponent {}     -> noCodeS
    DS18B20Component {}       -> do 
        stmt $ trustMe ("handle_" ++ component_name comp ++ "()")
        noCodeS

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
    DS18B20Component n _ s r  -> do
        _ :: LVal DS.OneWire <- declareGlobal (n ++ "_oneWire")
        _ :: LVal DS.DallasTemperature <- declareGlobal (n ++ "_sensors")
        _ :: LVal Int <- declareGlobal (component_name comp ++ "_previousMillis(0)")
        declPrevs
        noCode
        where
            declPrevs = flat $ map declPrev s
            declPrev sens = do
                _ :: LVal Double <- declareGlobal (n ++ "_state_" ++ sensor_name sens ++ "(0)")
                noCode
                --noCode
        
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
                reporters'
                noCodeS
        noCode
            where
                reporters' = flatS genReporters
                genReporters = map genReporter reps
                genReporter (OnChange d) = do
                    newState <- newvar "newState"
                    newState =: call digitalRead (lit pin')
                    iff (newState /= state) (do
                        state =: newState
                        x :: LVal (Ptr Char) <- "x" =. arrayMalloc (lit 2)
                        x ! lit 0 =: call toChar (newState + lit (ord '0'))
                        x ! lit 1 =: call toChar (int 0)
                        scall (mqttPublish client) (lit ("declduino/"++device_name dev++"/"++n)) x
                        scall delay (lit d)
                        noCodeS)
                    where
                        client = externVar "client"
                        state = externVar (n ++ "_state")

                genReporter (OnTime i) = do
                    ms :: LVal Int <- newvar "ms"
                    ms =: call millis
                    iff (ms - prevMs >= lit (1000 Prelude.* i)) (do
                        newState <- newvar "newState"
                        x :: LVal (Ptr Char) <- "x" =. arrayMalloc (lit 2)
                        x ! lit 0 =: call toChar (newState + lit (ord '0'))
                        x ! lit 1 =: call toChar (int 0)
                        scall (mqttPublish client) (lit ("declduino/"++device_name dev++"/"++n)) x
                        noCodeS)
                    noCodeS
                    where
                        client = externVar "client"
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
    DS18B20Component n _ sensors' reps     -> do
        _ :: Fun (IO ()) 
            <- defineNewFun ("handle_" ++ n) () $ \_ -> do
                scall (DS.requestTemperatures sens)
                reporters'
        noCode
        where
            client = externVar "client"
            sens = externVar (n++"_sensors")
            reporters' = flatS $ map handleReporter reps
            handleReporter (OnChange d) = do
                handleSensorsOnChange
                scall delay (lit d)
            handleReporter (OnTime i) = do
                ms :: LVal Int <- newvar "ms"
                ms =: call millis
                iff (ms - prevMs >= lit (1000 Prelude.* i)) $ do
                    prevMs =: ms
                    handleSensorsOnTime
                where
                    prevMs = externVar (n ++ "_previousMillis")
            handleSensorsOnTime = flatS $ map handleSensorOnTime sensors'
                where
                    handleSensorOnTime s = do
                        temp <- newvar "temp"
                        temp =: call (DS.getTempCByIndex sens) (lit $ index s)
                        scall (mqttPublish client) (lit ("declduino/thermo-one/thermo/" ++ sensor_name s)) (toStrPtr temp)
            handleSensorsOnChange = flatS $ map handleSensorOnChange sensors'
                where
                    handleSensorOnChange s = do
                        temp <- newvar "temp"
                        temp =: call (DS.getTempCByIndex sens) (lit $ index s)
                        iff (state /= temp) $ do
                            state =: temp
                            scall (mqttPublish client) (lit ("declduino/thermo-one/thermo/" ++ sensor_name s)) (toStrPtr temp) 
                        where
                            state = externVar (n ++ "_state_" ++ sensor_name s)

componentToSubscriptions :: Device -> Component -> [(String, String)]
componentToSubscriptions dev comp = case comp of
    DigitalOutputComponent n _ -> [("declduino/"++devName++"/"++n, "handle_"++n)]
    DigitalInputComponent {}   -> []
    PWMOutputComponent n _ _   -> 
        [ ("declduino/"++devName++"/"++n++"/pwm", "handle_"++n++"_pwm")
        , ("declduino/"++devName++"/"++n++"/mode", "handle_"++n++"_mode")
        ]
    DS18B20Component {}        -> []
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
