{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE ViewPatterns       #-}
module Declduino.App
  ( Env
  , Log(..), LogLevel(..), Logs
  , logInfo, logDebug, logWarning, logError, throwError
  , App
  , runApp
  , liftIO, liftMaybe, liftIOMaybe, liftEither, liftIOEither
  ) where

import           Control.Monad.Reader
import           Data.Data

type Env = ()

data Log = Log LogLevel String
  deriving (Show)

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord, Bounded, Show, Data)

type Logs = [Log]

newtype App a = App { getApp :: ReaderT Env IO (Either Logs a, Logs) }
  deriving (Functor)

instance Applicative App where
  pure a = App $ lift $ return (Right a, [])
  (getApp -> f) <*> (getApp -> a) = App $ ReaderT $ \env -> do
    (f', wf) <- runReaderT f env
    (a', wa) <- runReaderT a env
    return (f' <*> a', wf <> wa)

instance Monad App where
  (getApp -> a) >>= f = App $ ReaderT $ \env -> do
    (a', wa) <- runReaderT a env
    case a' of
      Left e  -> pure (Left e, e)
      Right a'' -> do
        (b', wb) <- runReaderT (getApp $ f a'') env
        return (b', wa <> wb)

instance MonadIO App where
  liftIO m = App $ ReaderT $ \_ -> fmap (\a -> (Right a, [])) m

addLog :: Log -> App ()
addLog l = do
  liftIO $ print l
  App $ lift $ return (Right (), pure l)

logDebug :: String -> App ()
logDebug = addLog . Log Debug

logInfo :: String -> App ()
logInfo = addLog . Log Info

logWarning :: String -> App ()
logWarning = addLog . Log Warning

logError :: String -> App a
logError e = App $ lift $ return (Left l, l)
  where l = [Log Error e]

throwError :: String -> App a
throwError = logError

liftMaybe :: String -> Maybe a -> App a
liftMaybe e Nothing  = logError e
liftMaybe _ (Just x) = return x

liftIOMaybe :: String -> IO (Maybe a) -> App a
liftIOMaybe e m = join $ liftIO $ fmap (liftMaybe e) m

liftEither :: (e -> String) -> Either e a -> App a
liftEither f (Left e)  = logError $ f e
liftEither _ (Right x) = return x

liftIOEither :: (e -> String) -> IO (Either e a) -> App a
liftIOEither f m = join $ liftIO $ fmap (liftEither f) m

runApp :: Env -> App a -> IO (Either Logs (a, Logs))
runApp env (getApp -> m) = do
  x <- runReaderT m env
  case fst x of
    Right a -> return $ return (a, snd x)
    Left _  -> return $ Left $ snd x

