module Main where

import Data.Char (isSpace)
import System.Environment (getArgs)
import IPANumberToUnicode 
import UnicodeToIPANumber
import Data.List (intercalate)

-- | The version number
versionNumber :: String
versionNumber = "0.3.1"

-- | The user-facing name of the program
programName :: String
programName = "IPA Number and Character Converter"

-- | This is a short message to show users who need to know how to use it.
helpMessage :: String
helpMessage = "Usage: \n --cn \t To convert a character to a number use the flag.\n --nc \t To convert a number to a character use the flag. \n --version \t To see the version number of this executable.\n --ncps \t To convert numbers to characters while preserving whitespace."

-- | A function to convert an IPA character to a number
-- when both are stored as strings.
ipaCharacterToNumberWrapper :: String -> String
ipaCharacterToNumberWrapper = show . unicodeToNumber . head

-- | a function to convert IPA numbers to characters
-- when both are stored as strings.
-- This function does not preserve whitespace. All consecutive
-- whitespace becomes a single space characte.r
numberToUnicodeWrapper :: String -> String
numberToUnicodeWrapper = return . numberToUnicode . read 

-- | Returns how many characters could be part of a number
-- at the beginning of a string.
recognizeNumber :: String -> Int
recognizeNumber [] = 0
recognizeNumber ('0':xs) = 1 + recognizeNumber xs
recognizeNumber ('1':xs) = 1 + recognizeNumber xs
recognizeNumber ('2':xs) = 1 + recognizeNumber xs
recognizeNumber ('3':xs) = 1 + recognizeNumber xs
recognizeNumber ('4':xs) = 1 + recognizeNumber xs
recognizeNumber ('5':xs) = 1 + recognizeNumber xs
recognizeNumber ('6':xs) = 1 + recognizeNumber xs
recognizeNumber ('7':xs) = 1 + recognizeNumber xs
recognizeNumber ('8':xs) = 1 + recognizeNumber xs
recognizeNumber ('9':xs) = 1 + recognizeNumber xs
recognizeNumber (_:xs) = 0

-- | This preserves tabs, space, and newline characters as they are.
numberToUnicodeWrapperPreserveSpaces :: [Char] -> [Char]
numberToUnicodeWrapperPreserveSpaces text = 
  let recognizedCharCount = recognizeNumber text
  in if recognizedCharCount > 0
      then numberToUnicode (read (take recognizedCharCount text)) : numberToUnicodeWrapperPreserveSpaces (drop recognizedCharCount text) 
      else if null text
        then []
        else head text:numberToUnicodeWrapperPreserveSpaces (tail text)


-- | Prints a message explaining how to use the program.
-- It prints the information to the command line for 
-- the user of the program to read.
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

-- | Splits text into its whitespace
spaces :: String -> [String]
spaces text = go text []
  where go :: String -> String -> [String]
        go [] spacePlace = [spacePlace]
        go (' ':s) spacePlace = go s (spacePlace ++ [' '])
        go ('\t':s) spacePlace = go s (spacePlace ++ ['\t'])
        go ('\n':s) spacePlace = go s (spacePlace ++ ['\n'])
        go ('\r':s) spacePlace = go s (spacePlace ++ ['\r'])
        go (x:s) spacePlace = 
          if null spacePlace then go s [] else spacePlace: go s []        


-- | The function decides what
-- to do based on command line arguments.
handleArguments :: [String] -> IO ()
handleArguments args =
  case args of 
    ["--cn"] -> interact (perWord ipaCharacterToNumberWrapper) >> putStrLn ""
    ["--nc"] -> interact  (perWord numberToUnicodeWrapper) >> putStrLn ""   -- Puts a new line after
    ["--ncps"] -> interact  numberToUnicodeWrapperPreserveSpaces >> putStrLn ""   -- Puts a new line after
    ["--version"]  -> showProgramVersion
    _ -> showHelp


-- | Program execution starts in the `main` function.
-- It gets the command line arguments
-- in order to handle them.
main :: IO ()
main =
  -- putStrLn (intercalate "," (spaces "hello  hi\ttheis\ntell\tEND")) >>
  getArgs >>= handleArguments 
 
