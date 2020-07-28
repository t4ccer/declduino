module Main where

import CodeGen
import CodeDecl (Device, deviceToCode)
import Parameters
import Data.Yaml
import System.Exit
import Control.Monad
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
        code <- mapM eitherToCode applied
        let names = map ((++".ino") . head . wordsWhen ('.' ==)) $ p_files params
        zipWithM_ writeFile names code

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


eitherToCode :: Either ParseException Device -> IO String
eitherToCode (Left v) = putStrLn ("File parsing error: " ++ show v) >> exitWith (ExitFailure 1)
eitherToCode (Right v) = return $ declToCode v

declToCode :: Device -> String
declToCode = tokensToCode . deviceToCode
