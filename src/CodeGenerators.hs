
module CodeGenerators where
import Board
import qualified CodeGenerators.ESP32 as ESP32
import FancyLogger

generateCode :: Device -> FancyLogger String
generateCode dev = case board dev of
    ESP32 -> appendLog (Log Debug "Generating code") $ ESP32.generateCode dev


