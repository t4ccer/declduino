module Declduino.Yaml where

import           Data.String.Interpolate (i)
import qualified Data.Yaml               as Yaml

import           Declduino.App

-- TODO implement proper YamlException formatting
decodeFile :: (Yaml.FromJSON a, MonadFail m, MonadIO m) => FilePath -> m a
decodeFile fp = liftIOEither (\e -> [i|Decoding file '#{fp} failed: #{renderException e}'|]) $ Yaml.decodeFileEither fp

renderException :: Yaml.ParseException -> String
renderException (Yaml.AesonException e) = e
renderException e                       = show e


