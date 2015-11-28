{-# LANGUAGE OverloadedStrings #-}

module URLNameSpec (main, spec) where

import URLName
import Test.Hspec
import Test.QuickCheck
import Data.Char (isLower, isAlpha, isDigit, isAscii)
import Data.Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
  describe "URLName.convert" $ do
    it "is a no op with OK text" $
      convert "hello.world" `shouldBe` "hello.world"

    it "lowercases chars" $
      convert "Hello.World" `shouldBe` "hello.world"

    it "preserves numbers" $
      convert "1234567890" `shouldBe` "1234567890"

    it "converts punctuation" $
      convert "hi!£$%^&*()-_" `shouldBe` "hi-----------"

    it "always results in alphanumerics and .s and -s" $ property $
      \x -> Data.Text.all isOKChar . convert . pack $ (x :: String)


isOKChar :: Char -> Bool
isOKChar '.' = True
isOKChar '-' = True
isOKChar x
  | isAlpha x = isAscii x && isLower x
  | isDigit x = True
  | otherwise = False
