module TestMain where

import UnicodeToIPANumber

main :: IO ()
main =
  if unicodeToNumber 'b' /= 102
  then error "b should be 102"
  else return ()
  >>
  if unicodeToNumber 'p' /= 101
  then error "p should be 101"
  else return ()  
  >>
  putStrLn "All Tests passed successfully, but we need more tests"
