module Main where

import CodeDecl (deviceToCode)
import Board
import Parameters
import Data.Yaml
import System.Exit
import System.Console.CmdArgs (cmdArgs)
import Control.Monad

main :: IO ()
main = do 
    params <- cmdArgs parameters
    let names = p_files params
    if null names then do
        putStrLn "Run declduino -h for usage info"
        exitWith (ExitFailure 1)
    else do
        decoded <- mapM decodeYamlFile names
        let codes = fmap (>>= processDevice params) decoded
        zipWithM_ toFile names codes
        exitSuccess

processDevice :: Parameters -> Device  -> Result String
processDevice params dev = do
    applied <- applyParameters params dev
    checked <- hasNameConfilcts applied
    deviceToCode checked

decodeYamlFile :: FilePath -> IO (Result Device)
decodeYamlFile f = do 
    dec <- decodeFileEither f
    case dec of 
        (Left _) -> return $ Left YamlParserError
        (Right v) -> return v



changeExt :: String -> String -> String
changeExt ext = (++"."++ext) . head . wordsWhen ('.' ==)

toFile :: FilePath -> Result String -> IO()
toFile f (Left e) = putStrLn ("An error occurred when processing '"++f++"': " ++ show e)
toFile f (Right c) = do
    putStrLn ("Created file " ++ n)
    writeFile n c 
    where 
        n = changeExt "ino" f

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'
