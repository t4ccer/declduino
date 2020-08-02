module Main where

-- import CodeDecl (deviceToCode)
import CodeGenerators (generateCode)
import Board
import Parameters
import Data.Yaml
import System.Console.CmdArgs (cmdArgs)
import Error
import Control.Monad.Trans.Except

testMain :: IO ()
testMain = do
    res <- testRun
    case res of
        Left err -> print err
        Right xs -> uncurry toFile xs

testRun :: IO (Either Error (String, FilePath))
testRun = runExceptT $ do
    let fname = "examples/esp32-builtin-led-pwm.yaml"
    let inoName = changeExt "ino" fname
    device <- ExceptT $ decodeYamlFile fname
    _      <- ExceptT $ return $ hasNameConfilcts device
    code   <- ExceptT $ return $ generateCode device
    ExceptT $ return $ return (code, inoName)

main :: IO ()
main = do
    res <- run
    case res of
        Left err -> print err
        Right xs -> mapM_ (uncurry toFile) xs

-- run :: IO (Result [String])
run :: IO (Result [(String, FilePath)])
run = runExceptT $ do
    params            <- ExceptT $ return <$> cmdArgs parameters
    _                 <- ExceptT $ verifyParams params
    decodedDevices    <- ExceptT $ sequenceA <$> mapM decodeYamlFile (p_files params)
    devicesWithParams <- ExceptT $ return $ traverse (applyParameters params) decodedDevices
    _                 <- ExceptT $ return $ traverse hasNameConfilcts devicesWithParams
    codes             <- ExceptT $ sequenceA <$> traverse (return . generateCode) devicesWithParams
    ExceptT $ return $ return $ zip codes (p_files params)

verifyParams :: Parameters -> IO (Result Parameters)
verifyParams params 
    | null $ p_files params = return $ Left $ ParametersError "No files provided"
    | otherwise = return $ Right params

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

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'
