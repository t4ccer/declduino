module CodeGenerators.LanguageDSL.ESP32Specific where

import CodeGenerators.LanguageDSL.CGenerator

data WiFiClient
instance ClassClass WiFiClient where
  className _ = "WiFiClient"

data PubSubClient
instance ClassClass PubSubClient where
  className _ = "PubSubClient"

restartESP :: Fun (IO ())
restartESP = declareFunExtern "ESP.restart"

delay :: Fun (Int -> IO ())
delay = declareFunExtern "delay"

digitalWrite :: Fun (Int -> Int -> IO())
digitalWrite = declareFunExtern "digitalWrite"

digitalRead :: Fun (Int -> IO Int)
digitalRead = declareFunExtern "digitalRead"

arduinoOTABegin :: Fun (IO ())
arduinoOTABegin = declareFunExtern "ArduinoOTA.begin"

arduinoOTASetHostname :: Fun (String -> IO ())
arduinoOTASetHostname = declareFunExtern "ArduinoOTA.setHostname"

arduinoOTASetPort :: Fun (Int -> IO ())
arduinoOTASetPort = declareFunExtern "ArduinoOTA.setPort"

arduinoOTAOnEnd :: Fun (Fun (IO ()) -> IO ())
arduinoOTAOnEnd = declareFunExtern "ArduinoOTA.onEnd"

arduinoOTALoop :: Fun (IO ())
arduinoOTALoop = declareFunExtern "ArduinoOTA.handle"

wifiStatus :: Fun (IO Int)
wifiStatus = declareFunExtern "WiFi.status"

wifiBegin :: Fun (String -> String -> IO ())
wifiBegin = declareFunExtern "WiFi.begin"

mqttSetServer :: Fun(String -> Int -> IO ())
mqttSetServer = declareFunExtern "client.setServer"

mqttConnected :: Fun (IO Int)
mqttConnected = declareFunExtern "client.connected"

mqttSetCallback :: Fun (Fun (Ptr Char -> Ptr Byte -> Int -> IO ()) -> IO ())
mqttSetCallback = declareFunExtern "client.setCallback"

mqttConnect :: Fun (String -> IO ())
mqttConnect = declareFunExtern "client.connect"

mqttLoop :: Fun (IO ())
mqttLoop = declareFunExtern "client.loop"

mqttPublish :: Fun(String -> Ptr Char -> IO ())
mqttPublish = declareFunExtern "client.publish"

millis :: Fun (IO Int)
millis = declareFunExtern "millis"

ledcWrite :: Fun(Int -> Int -> IO ())
ledcWrite = declareFunExtern "ledcWrite"
