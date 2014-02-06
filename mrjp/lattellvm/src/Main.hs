import System.Environment

import Control.Monad (void)
import Data.List (intercalate, sort)
import System.Cmd (system)
import System.Exit (exitFailure)
import System.FilePath (replaceExtension, replaceFileName)

import Text.Parsec.String

import Latte.LlvmTranslator
import Latte.Optimizer
import Latte.Parser
import Latte.Verifier

usage :: String -> String
usage progName = "usage: " ++ progName ++ " <input-file>"

llvmAsCmd :: String
llvmAsCmd = "llvm-as-3.3"

llcCmd :: String
llcCmd = "llc-3.3"

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
            (m, []) -> do
              putStrLn "OK"
              let (m1, ws) = optimize m
              putStrLn $ intercalate "\n" $ map show $ sort ws
              let llFile = replaceExtension x ".ll"
              let bcFile = replaceExtension x ".bc"
              let sFile = replaceExtension x ".s"
              toLlvmToFile m1 llFile
              void $ system $ llvmAsCmd ++ " " ++ llFile
              void $ system $ llcCmd ++ " -o " ++ sFile ++ " " ++ bcFile
              void $ system $ "g++ -o " ++ (replaceFileName x "a.out") ++ " " ++ sFile
              void $ system $ "rm " ++ bcFile ++ " " ++ sFile
            (_, xs) -> do 
              putStrLn "ERROR"
              putStrLn $ intercalate "\n" $ map show $ sort xs
              exitFailure 

