module Main where

import IPANumberToUnicode 
import UnicodeToIPANumber


numberToUnicodeWrapper :: String -> String
numberToUnicodeWrapper = return . numberToUnicode . read 

main :: IO ()
main = interact numberToUnicodeWrapper
--  putStrLn [(numberToUnicode 141)]
