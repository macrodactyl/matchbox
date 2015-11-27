module URLName
  ( convert
  ) where

import Data.Char (isAlphaNum, isAscii)
import Data.Text

convert :: Text -> Text
convert = Data.Text.map convertChar . toLower

convertChar :: Char -> Char
convertChar char
  | isAscii char && isAlphaNum char = char
  | char == '.' = '.'
  | otherwise   = '-'
