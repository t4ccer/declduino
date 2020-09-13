{-# LANGUAGE FlexibleInstances, ConstrainedClassMethods, DeriveFunctor,DeriveDataTypeable #-}

module FancyLogger where

import Control.Monad (void)
import System.Console.CmdArgs (Data)
import Prelude hiding (log)

data LogLevel = 
      Debug
    | Info
    | Warning
    | Error
    deriving (Show, Eq, Ord, Data)


data Log = Log LogLevel String
    deriving Show


class Logger l where
    returnError   :: String -> l a
    appendLogs    :: [Log] -> l a -> l a
    tell          :: l a -> l [Log]

    returnWithLog :: Monad l => Log -> a -> l a
    returnWithLog log = appendLog log . return

    appendLog     :: Log -> l a -> l a
    appendLog l = appendLogs [l] 

data Status = Success | Failure

newtype FancyLogger a = FancyLogger (IO (Maybe a, [Log]))
    deriving (Functor)

instance Applicative FancyLogger where
    pure a = FancyLogger (return (Just a, []))
    FancyLogger f <*> FancyLogger a = FancyLogger $ do
        (f', w)  <- f
        (a', w') <- a
        return (f'<*>a', w<>w')
    
instance Monad FancyLogger where
    FancyLogger a >>= f = FancyLogger $ do
        (a', w) <- a
        let FancyLogger y = case a' of
                Just v -> f v
                Nothing -> FancyLogger $ return (Nothing, [])
        (b, w') <- y
        return (b, w<>w')

instance Logger FancyLogger where
    tell (FancyLogger a) = FancyLogger $ do
        (_, w) <- a
        return (Just w, w)
    returnError e = FancyLogger $ return (Nothing, [Log Error e])
    appendLogs logs (FancyLogger a) = FancyLogger $ do
        (v, logs') <- a
        return (v, logs<>logs')

printLogs :: FancyLogger a -> IO ()
printLogs (FancyLogger a) = do
    (_, w) <- a
    print w

prettyPrintLogs :: LogLevel -> FancyLogger a -> IO ()
prettyPrintLogs level (FancyLogger a) = do
    (_, w) <- a
    let w' = filter (\(Log l _) -> l >= level) w
    mapM_ (putStrLn . \(Log l m) -> showLevel l ++m) w'
    where
        showLevel Debug   = "[ DEBUG ] "
        showLevel Info    = "[ INFO  ] "
        showLevel Warning = "[WARNING] "
        showLevel Error   = "[ ERROR ] "


fromIO :: IO a -> FancyLogger a
fromIO = FancyLogger . fmap (\x -> (Just x, []))


toLogs :: FancyLogger a -> IO [Log]
toLogs (FancyLogger a) = do
    (_, w) <- a
    return w

runLogger :: FancyLogger a -> IO ()
runLogger (FancyLogger a) = void $ do
    (io , _) <- a
    return io

