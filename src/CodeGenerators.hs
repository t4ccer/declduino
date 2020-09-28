{-# LANGUAGE QuasiQuotes #-}

module CodeGenerators where

import Data.String.Interpolate (i)
import Board
import FancyLogger
import qualified CodeGenerators.ESP32 as ESP32

generateCode :: Device -> FancyLogger String
generateCode dev = case board dev of
    ESP32 -> appendLog (Log Debug [i|Generating code for '#{device_name dev}'|]) $ ESP32.generateCode dev


