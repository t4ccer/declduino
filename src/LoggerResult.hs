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

class Logger l where
    returnError   :: Log -> l a
    returnWithLog :: Log -> a -> l a
    appendLogs    :: [Log] -> l a -> l a
    tell          :: l a -> l [Log]

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

instance Logger LoggerResult where
    returnError e = LoggerResult [e] Nothing
    returnWithLog l v = LoggerResult [l] $ Just v
    tell (LoggerResult w _) = LoggerResult w $ Just w
    appendLogs ls (LoggerResult w v) = LoggerResult (w++ls) v

appendLog :: Logger l => Log -> l a -> l a
appendLog l = appendLogs [l] 


newtype IOLoggerResult a = IOLoggerResult (LoggerResult (IO a))

instance Functor IOLoggerResult where
    fmap f (IOLoggerResult (LoggerResult w io))= IOLoggerResult (LoggerResult w $ fmap (fmap f) io)

instance Applicative IOLoggerResult where
    pure v = IOLoggerResult (LoggerResult [] (return $ return v))
    (IOLoggerResult (LoggerResult w f)) <*> (IOLoggerResult (LoggerResult w' v)) = case f of
        Nothing -> IOLoggerResult (LoggerResult (w++w') Nothing)
        Just f' -> case v of
            Nothing -> IOLoggerResult $ LoggerResult (w++w') Nothing
            Just v' -> IOLoggerResult $ LoggerResult (w++w') $ Just $ f' <*> v'

instance Monad IOLoggerResult where
    return = pure
    IOLoggerResult (LoggerResult w v) >>= f = case v of
        Nothing -> IOLoggerResult $ LoggerResult w Nothing
        Just v' -> fromIO v' >>= f

instance Logger IOLoggerResult where
    returnError l = IOLoggerResult (LoggerResult [l] Nothing)
    returnWithLog l v = IOLoggerResult (LoggerResult [l] $ return $ return v)
    appendLogs ls (IOLoggerResult (LoggerResult w v)) = IOLoggerResult (LoggerResult (ls++w) v)
    tell (IOLoggerResult (LoggerResult w _)) = return w

fromIO :: IO a -> IOLoggerResult a
fromIO v = IOLoggerResult (LoggerResult [] $ return v)
