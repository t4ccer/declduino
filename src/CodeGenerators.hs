
module CodeGenerators where
import Board
import qualified CodeGenerators.Boards.ESP32 as ESP32

generateCode :: Device -> Result String
generateCode dev = case board dev of
    ESP32 -> ESP32.generateCode dev


