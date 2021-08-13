module UtilsSpec
  ( spec
  ) where

import           Test.Hspec
import           Utils

spec :: Spec
spec = do
  describe "Utils" $ do
    it "correct modulus" $ do
      modulus `shouldBe` 20201227

    it "modexp works for simple cases" $ do
      modexp 1 1 2 `shouldBe` 1
      modexp 2 2 100 `shouldBe` 4
      modexp 5 4 7 `shouldBe` 2
      modexp 3 3 13 `shouldBe` 1
      modexp 0 100 1000007 `shouldBe` 0

    describe "AOC Example" $ do
      it "can calculate reverse modular exponent" $ do
        revmodexp 7 5764801 modulus `shouldBe` 8
        revmodexp 7 17807724 modulus `shouldBe` 11

      it "can calculate encryption key" $ do
        modexp 17807724 8 modulus `shouldBe` 14897079
        modexp 5764801 11 modulus `shouldBe` 14897079
      