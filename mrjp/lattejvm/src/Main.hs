import System.Environment

import Control.Monad (void)
import Data.List (intercalate, sort)
import System.Cmd (system)
import System.FilePath (dropExtension, replaceExtension, takeDirectory, takeFileName)

import Text.Parsec.String

import Latte.JvmTranslator
import Latte.Parser
import Latte.Verifier

jasminPath :: String
jasminPath = "lib/jasmin.jar"

usage :: String -> String
usage progName = "usage: " ++ progName ++ " <input-file>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> getProgName >>= (putStrLn . usage)
    (x:xs) -> do 
      parseResult <- parseFromFile parser x
      case parseResult of
        Left err -> putStrLn "ERROR" >> print err
        Right program -> case verify program of
            --(m, []) -> putStrLn "OK" >> toLlvmToFile m "test.ll"
            (m, []) -> do 
              putStrLn "OK"
              let classFile = dropExtension $ takeFileName x
              let jfile = replaceExtension x ".j"
              let classDir = takeDirectory x
              toJvmToFile m classFile jfile
              void $ system $ "java -jar " ++ jasminPath ++ " -d " ++ classDir ++ " " ++ jfile ++ " > /dev/null"
            (_, xs) -> putStrLn "ERROR" >> (putStrLn $ intercalate "\n" $ map show $ sort xs)
