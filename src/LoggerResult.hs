{-# LANGUAGE DatatypeContexts #-}

module LoggerResult where

data LogLevel = 
      Debug
    | Warning
    | Error
    deriving Show


data Log = Log LogLevel String
    deriving Show


data LoggerResult a = LoggerResult [Log] (Maybe a)
    deriving Show

instance Functor LoggerResult where
    fmap f (LoggerResult w m) = LoggerResult w $ fmap f m

instance Applicative LoggerResult where
    pure v = LoggerResult [] $ Just v
    LoggerResult w f <*> LoggerResult w' v = case f of
        Nothing -> LoggerResult (w <> w') Nothing
        Just f' -> case v of
            Nothing -> LoggerResult (w <> w')   Nothing
            Just v' -> LoggerResult (w <> w') $ Just (f' v')

instance Monad LoggerResult where
    return = pure
    LoggerResult w v >>= f = case v of
        Nothing -> LoggerResult w Nothing
        Just v' -> LoggerResult (w <> w') v''
            where 
                LoggerResult w' v'' = f v'

mkResult :: a -> LoggerResult a
mkResult = return

returnError :: Log -> LoggerResult a
returnError e = LoggerResult [e] Nothing

returnWithLog :: Log -> a -> LoggerResult a
returnWithLog l v = LoggerResult [l] $ Just v

appendLog :: Log -> LoggerResult a -> LoggerResult a
appendLog l (LoggerResult w v) = LoggerResult (w++[l]) v

appendLogs :: [Log] -> LoggerResult a -> LoggerResult a
appendLogs ls (LoggerResult w v) = LoggerResult (w++ls) v

tell :: LoggerResult a -> LoggerResult [Log]
tell (LoggerResult w _) = LoggerResult w $ Just w