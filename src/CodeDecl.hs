module CodeDecl (deviceToCode) where

import Board
import qualified Board.ESP32 as ESP32

deviceToCode :: Device -> Result String
deviceToCode dev = case board dev of
    ESP32 -> ESP32.deviceToCode dev
