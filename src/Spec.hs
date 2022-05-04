module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)


correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParte1
  -- suiteDeTestsDeParte2
  -- suiteDeTestsDeParte3
suiteDeTestsDeParte1 = describe "Concediendo Deseos" $ do
-- data Chico = Chico {
--     nombre :: String,
--     edad :: Number,
--     habilidad :: [Habilidades],
--     deseos :: [Deseos]
--     } 
  -- let chico = Chico "bobi" 10 [] []
  -- let chicoConHabilidad = Chico "bobi" 10 ["Prog Haskell Super Sr"] []
  -- let chicoMocoso = Chico "bobi" 10 ["Aprender a sacarse los mocos"] []

  let chico = Chico {
    nombre = "bobi",
    edad = 10 ,
    habilidad = []
    --deseos = []
    }
  let chicoConHabilidad  = Chico {
    nombre = "bobi",
    edad = 10 ,
    habilidad = ["Prog Haskell Super Sr"]
    --deseos = []
    }

  let chicoMocoso = Chico {
    nombre = "bobi",
    edad = 10 ,
    habilidad = ["Sacarse los mocos"]
    --deseos = []
    }

  describe "Aprender habilidades" $ do
    it "Sea una nueva habilidad  sacarse los mocos entonces el chico aprenderá a sacarse los mocos" $ do
      aprenderHabilidades ["Sacarse los mocos"] chico `shouldBe` chicoMocoso
    it "Sea un chico con habilidad si no le enseño nada seguira con la misma habilidad" $ do
      aprenderHabilidades [] chicoConHabilidad `shouldBe` chicoConHabilidad 
    
