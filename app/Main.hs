module Main where

import CodeGen
import CodeDecl (Device, deviceToCode)
import Data.Yaml
import System.Environment
import System.Exit

main :: IO ()
main = do 
    file <- filename
    decoded <- decodeFileEither file
    code <- eitherToCode decoded
    writeFile "out.ino" code

filename :: IO FilePath
filename = getArgs >>= getFName

getFName :: [String] -> IO String
getFName x
    | length x /= 1 = putStrLn "Usage: declduino <fname.yaml>" >> exitWith (ExitFailure 1)
    | otherwise = return $ head x

eitherToCode :: Either ParseException Device -> IO String
eitherToCode (Left v) = putStrLn ("File parsing error: " ++ show v) >> exitWith (ExitFailure 1)
eitherToCode (Right v) = return $ declToCode v

declToCode :: Device -> String
declToCode = tokensToCode . deviceToCode
