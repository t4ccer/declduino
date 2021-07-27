{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Declduino.App
  ( MonadIO, liftIO
  , ReaderT, runReaderT
  , MonadLogger, LogLevel(..)
  , logDebugN, logInfoN, logWarnN, logErrorN
  , MonadFail, fail
  , MonadReader, ask, asks
  , liftIOEither
  , runAppIO
  , whenM
  ) where

import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Data
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T

liftIOEither :: (MonadIO m, MonadFail m) => (e -> String) -> IO (Either e a) -> m a
liftIOEither f m = do
  m' <- liftIO m
  case m' of
    Left e  -> fail $ f e
    Right a -> pure a

newtype AppIO a = AppIO {getAppIO :: LoggingT (ExceptT LogLine IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance MonadFail AppIO where
  fail e = do
    logErrorN $ T.pack e
    AppIO $ lift $ ExceptT $ pure $ Left (defaultLoc, T.pack e, LevelError, toLogStr e)

runAppIO :: AppIO a -> IO (Either Text a)
runAppIO = runExceptT . withExceptT (T.decodeUtf8 . fromLogStr . \(a,b,c,d) -> defaultLogStr a b c d) . runStdoutLoggingT . getAppIO

whenM :: Monad m => m Bool -> m () -> m ()
whenM m eff = m >>= \cond -> when cond eff
  
