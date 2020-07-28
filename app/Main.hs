module Main where

import CodeDecl (deviceToCode)
import Board
import Parameters
import Data.Yaml
import System.Exit
import System.Console.CmdArgs (cmdArgs)

main :: IO ()
main = do 
    params <- cmdArgs parameters
    if null (p_files params) then do
        putStrLn "Run declduino -h for usage info"
        exitWith (ExitFailure 1)
    else do
        decoded <- mapM decodeFileEither $ p_files params
        let applied = fmap (fmap (applyParameters params)) decoded
        let codes = map (fmap deviceToCode) applied
        let ino_names = map ((++".ino") . head . wordsWhen ('.' ==)) $ p_files params
        mapM_ (\ (x, y, z) -> toFile x y z) $ zip3 (p_files params) ino_names codes
        putStrLn ""

toFile :: FilePath -> FilePath -> Either ParseException String -> IO()
toFile _ f (Right x) = do 
    writeFile f x
    putStrLn ("Created file " ++ f)
toFile o _ (Left x) = do
    putStrLn ("Error with processing " ++ o)
    putStrLn ("\t"++show x)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

eitherToCode :: Either ParseException Device -> IO String
eitherToCode (Left v) = putStrLn ("File parsing error: " ++ show v) >> exitWith (ExitFailure 1)
eitherToCode (Right v) = return $ deviceToCode v
