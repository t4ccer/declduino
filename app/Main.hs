module Main where

import CodeGenerators (generateCode)
import Board
import Parameters
import Data.Yaml
import System.Console.CmdArgs (cmdArgs, modes)
import Error
import Control.Monad.Trans.Except
import Data.List (isPrefixOf)
import Control.Monad (zipWithM)
import HomeAssistant


main :: IO ()
main = do
    params <- cmdArgs (modes parameters)
    res <-    run params
    case res of
        Left e -> print e
        Right() -> return ()

run :: Parameters -> IO(Result ())
run params = case params of
    NoMode{}   -> do
        putStrLn "Run declduino -h for help"
        return $ return ()
    Generate{} -> runGenerate params
    Hass{}     -> runHass params

runGenerate :: Parameters -> IO (Result ())
runGenerate params = runExceptT $ do
    params'           <- ExceptT $ return $ verifyParams params
    decodedDevices    <- ExceptT $ decodeYamlFiles params'
    devicesWithParams <- ExceptT $ return $ traverse (applyParameters params') decodedDevices
    _                 <- ExceptT $ return $ traverse hasNameConfilcts devicesWithParams
    codes             <- ExceptT $ sequenceA <$> traverse (return . generateCode) devicesWithParams
    ExceptT $ fmap (Right . const ()) $ zipWithM toFile codes $ p_files params'

runHass :: Parameters -> IO (Result ())
runHass params = do
    putStrLn "Hass generator is not implemented yet!"
    runExceptT $ do
        params' <- ExceptT $ return $ verifyParams params
        decodedDevices    <- ExceptT $ decodeYamlFiles params'
        devicesWithParams <- ExceptT $ return $ traverse (applyParameters params') decodedDevices 
        entities          <- ExceptT $ return $ return $ devicesToEntities devicesWithParams
        ExceptT $ return <$> encodeFile (p_output params') entities 

verifyParams :: Parameters -> Result Parameters
verifyParams params 
    | null $ p_files params = Left $ ParametersError "No files provided"
    | otherwise             = Right normalizedParams
    where
        normalizedParams = params {p_files = normalizePaths $ p_files params} 
        normalizePaths = map normalizePath
        normalizePath f = 
            let f' = map (\x -> if x == '\\' then '/' else x) f
            in
            if "./" `isPrefixOf` f' 
                then drop 2 f'
                else f'

decodeYamlFiles :: Parameters -> IO (Result [Device])
decodeYamlFiles params = do
    let fnames = p_files params
    x <- mapM decodeYamlFile fnames
    return $ sequenceA x

decodeYamlFile :: FilePath -> IO (Result Device)
decodeYamlFile f = do 
    dec <- decodeFileEither f
    case dec of 
        (Left _) -> return $ Left YamlParserError
        (Right v) -> return v

changeExt :: String -> String -> String
changeExt ext = (++"."++ext) . head . wordsWhen ('.' ==)

toFile :: String -> FilePath -> IO()
toFile c f = do
    putStrLn ("Created file " ++ n)
    writeFile n c 
    where 
        n = changeExt "ino" f

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'
