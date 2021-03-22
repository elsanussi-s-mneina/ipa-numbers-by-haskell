module Main where

import System.Environment (getArgs)

import IPANumberToUnicode 
import UnicodeToIPANumber

versionNumber :: String
versionNumber = "0.3.0"

programName :: String
programName = "IPA Number and Character Converter"

helpMessage :: String
helpMessage = "Usage: \n --cn \t To convert a character to a number use the flag.\n --nc \t To convert a number to a character use the flag. \n --version \t To see the version number of this executable."

ipaCharacterToNumberWrapper :: String -> String
ipaCharacterToNumberWrapper = show . unicodeToNumber . head

numberToUnicodeWrapper :: String -> String
numberToUnicodeWrapper = return . numberToUnicode . read 

showHelp :: IO ()
showHelp = putStrLn helpMessage

-- | Display the program name and version number.
showProgramVersion :: IO ()
showProgramVersion = putStrLn (unwords [programName, versionNumber])

-- | Converts a function that applies to a single token in a string,
--  and makes a similar function that does the same but with
-- strings that have multiple tokens.
-- This allows us to handle "104 103" instead of just
-- "103" and "104" in separate strings.
-- Please note that it does not preserve the specific
-- kind of whitespace, every whitespace will
-- eventually be converted to a space character.
perWord :: (String -> String) -> (String -> String)
perWord x = unwords . map x . words

handleArguments :: [String] -> IO ()
handleArguments args =
  case args of 
    ["--cn"] -> interact (perWord ipaCharacterToNumberWrapper)
    ["--nc"] -> interact  (perWord numberToUnicodeWrapper)
    ["--version"]  -> showProgramVersion
    _ -> showHelp



main :: IO ()
main =
  getArgs >>= handleArguments 
 
