module TestMain where
import Control.Monad (unless)
import IPANumberToUnicodeSpec
import UnicodeToIPANumberSpec

import IPAUnicodeConstantsSpec
import IPANumberConstantsSpec

main :: IO ()
main =
  runNumberToUnicodeTests
  >>
  unicodeToIPANumberSpec
  >>
  runIpaUnicodeSpec
  >>
  ipaNumbersSpec
  >>
  putStrLn "All Tests passed successfully"
