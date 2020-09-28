{-# LANGUAGE DeriveDataTypeable #-}

module Logs where

import System.Console.CmdArgs (Data)
import Prelude hiding (log)
import Data.Aeson hiding (Error)
import Data.Text (pack)

data LogLevel = 
      Debug
    | Info
    | Warning
    | Error
    deriving (Show, Eq, Ord, Data)

data Log = Log LogLevel String
    deriving Show

instance ToJSON Log where
    toJSON (Log lvl msg) = object 
        [ pack "level"   .= show lvl
        , pack "message" .= msg
        ]

data LogFormat = 
      List
    | JSON
    deriving (Show, Data)
