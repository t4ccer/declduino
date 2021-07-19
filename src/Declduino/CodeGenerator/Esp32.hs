module Declduino.CodeGenerator.Esp32 where

import           Data.Text                      (Text)
import           Language.C.DSL

import           Declduino.App
import           Declduino.CodeGenerator.Common
import           Declduino.Device

generateCode :: (MonadFail m, MonadLogger m) => Device -> m Text
generateCode _ = undefined

