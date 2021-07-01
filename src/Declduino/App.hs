{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Declduino.App where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Data

type Env = ()

type Log = ()

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  | Critical
  deriving (Eq, Ord, Bounded, Show, Data)

type Logs = [Log]

type Error = ()

newtype App a = App { getApp :: ReaderT Env (WriterT Logs (ExceptT Error IO)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadWriter Logs, MonadError Error)

addLog :: Log -> App ()
addLog = undefined

getLogs :: App a -> App Logs
getLogs = undefined

runApp :: Env -> App a -> IO (Either Error a)
runApp = undefined

