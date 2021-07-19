module Declduino.CodeGenerator where

import           Data.Text                     (Text)

import           Declduino.App
import qualified Declduino.CodeGenerator.Esp32 as Esp32
import           Declduino.Device

generateCode :: (MonadFail m, MonadLogger m) => Device -> m Text
generateCode device =
  case deviceBoard device of
    Esp32 -> Esp32.generateCode device
