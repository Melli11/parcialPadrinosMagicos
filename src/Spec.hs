module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2
    it "El pdepreludat se instaló correctamente" $ do
      triple 1 `shouldBe` 3
    it "El pdepreludat se instaló correctamente" $ do
      cuadruple 1 `shouldBe` 4

