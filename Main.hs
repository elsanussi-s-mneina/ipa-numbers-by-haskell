module Main where

import System.Environment (getArgs)

import IPANumberToUnicode 
import UnicodeToIPANumber

helpMessage :: String
helpMessage = "To convert a character to a number use the --cn flag. To convert a number to a character use the --nc flag."

ipaCharacterToNumberWrapper :: String -> String
ipaCharacterToNumberWrapper = show . unicodeToNumber . head

numberToUnicodeWrapper :: String -> String
numberToUnicodeWrapper = return . numberToUnicode . read 

showHelp :: IO ()
showHelp = putStrLn helpMessage

handleArguments :: [String] -> IO ()
handleArguments args =
  case args of 
    ["--cn"] -> interact ipaCharacterToNumberWrapper
    ["--nc"] -> interact numberToUnicodeWrapper
    _ -> showHelp



main :: IO ()
main =
  getArgs >>= handleArguments 
 
