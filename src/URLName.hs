module URLName
  ( convert
  ) where

import Data.Char (isAlphaNum, isAscii, toLower)

convert :: String -> String
convert = map convertChar

convertChar :: Char -> Char
convertChar char
  | isAscii char && isAlphaNum char = toLower char
  | char == '.' = '.'
  | otherwise   = '-'
