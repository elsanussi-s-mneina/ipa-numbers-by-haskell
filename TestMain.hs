module TestMain where
import Control.Monad (unless)
import UnicodeToIPANumber

main :: IO ()
main =
  unless (unicodeToNumber 'b' == 102)
  (error "b should be 102")
  >>
  unless (unicodeToNumber 'p' == 101)
  (error "p should be 101")
  >>
  putStrLn "All Tests passed successfully, but we need more tests"
