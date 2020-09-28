{-# LANGUAGE QuasiQuotes #-}

module CodeGenerators where
import Board
import qualified CodeGenerators.ESP32 as ESP32
import FancyLogger
import Logs
import Data.String.Interpolate (i)

generateCode :: Device -> FancyLogger String
generateCode dev = case board dev of
    ESP32 -> appendLog (Log Debug [i|Generating code for '#{device_name dev}'|]) $ ESP32.generateCode dev


