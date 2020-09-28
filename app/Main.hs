{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}
module Main where

import Control.Monad (zipWithM_)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf)
import Data.Yaml
import Data.String.Interpolate (i)
import System.Console.CmdArgs (cmdArgs, modes)
import Board
import CodeGenerators (generateCode)
import Parameters
import HomeAssistant
import FancyLogger
import Logs

main :: IO ()
main = do
    params <- cmdArgs (modes parameters)
    let res = run params
    printLogs (p_format params) (p_verbosity params) res

run :: Parameters -> FancyLogger ()
run params = case params of
    NoMode{}   -> do
        returnWithLog (Log Info "Run declduino -h for help") ()
    Generate{} -> runGenerate params
    Hass{}     -> runHass params

runGenerate :: Parameters -> FancyLogger ()
runGenerate params =  do
    params'           <- verifyParams params
    decodedDevices    <- decodeYamlFiles params'
    returnWithLog (Log Debug "Applying parameters") ()
    devicesWithParams <- traverse (applyParameters params') decodedDevices
    traverse_ hasNameConfilcts devicesWithParams
    codes             <- traverse generateCode devicesWithParams
    zipWithM_ toFile codes $ p_files params'

runHass :: Parameters -> FancyLogger ()
runHass params = do
    returnWithLog (Log Warning "HASS generator is still in preview") ()
    params'           <- verifyParams params
    decodedDevices    <- decodeYamlFiles params'
    devicesWithParams <- traverse (applyParameters params') decodedDevices 
    let entities       = devicesToEntities devicesWithParams
    appendLog (Log Info [i|Created file '#{p_output params'}'|]) $ fromIO $ encodeFile (p_output params') entities 
        
verifyParams :: Parameters -> FancyLogger Parameters
verifyParams params 
    | null $ p_files params = returnError "No files provided"
    | otherwise             = returnWithLog (Log Debug "Verified params") normalizedParams
    where
        normalizedParams = params {p_files = normalizePaths $ p_files params} 
        normalizePaths = map normalizePath
        normalizePath f = 
            let f' = map (\x -> if x == '\\' then '/' else x) f
            in
            if "./" `isPrefixOf` f' 
                then drop 2 f'
                else f'

decodeYamlFiles :: Parameters -> FancyLogger [Device]
decodeYamlFiles params = do
    returnWithLog (Log Debug "Decoding files") ()
    mapM decodeYamlFile $ p_files params

decodeYamlFile :: FilePath -> FancyLogger Device
decodeYamlFile f = do 
    decoded_device :: Either ParseException (FancyLogger Device) <- fromIO $ decodeFileEither f
    case decoded_device of
        Left e -> returnError [i|Failed decoding file '#{f}': #{show e}|]
        Right d -> appendLog (Log Debug [i|Decoding file '#{f}'|]) d

changeExt :: String -> String -> String
changeExt ext = (++"."++ext) . head . wordsWhen ('.' ==)

toFile :: String -> FilePath -> FancyLogger ()
toFile c f = do
    appendLog (Log Info [i|Created file '#{n}'|]) $ fromIO $ writeFile n c 
    where 
        n = changeExt "ino" f

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'
