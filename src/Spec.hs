module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)


correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParte1
  suiteDeTestsDeParte2
  -- suiteDeTestsDeParte3
suiteDeTestsDeParte1 = describe "Concediendo Deseos" $ do

  describe "Aprender habilidades" $ do
    it "Sea una nueva habilidad llamada Conducir entonces un chico aprenderá una nueva habilidad " $ do
      length (habilidad (aprenderHabilidades ["Conducir"] chico)) `shouldBe`  1
    it "Sea un chico sin habilidad si no le enseño nada seguira siendo un inutil" $ do
      length (habilidad (aprenderHabilidades [] chico)) `shouldBe` 0 
    it "Sea un chico con habilidad si no le enseño nada seguira con la misma habilidad" $ do
      length (habilidad (aprenderHabilidades [] chicoConHabilidad)) `shouldBe` 1 
  describe "Mayoria de Edad" $ do
    it "Sea un niño y le cumplo el deseo de ser Mayor entonces tendrá 18" $ do
      edad (serMayor chico) `shouldBe` 18 

suiteDeTestsDeParte2 = describe "Padrinos magios" $ do
  let chicoSinHabilidadCon2Deseos = Chico "bobi" 10 [] [aprenderHabilidades ["Andar en bicicleta"],serMayor]
  let chicoConPrimerDeseoCumplido = Chico "bobi" 10 ["Andar en bicicleta"] [serMayor]
  describe "Wanda" $ do
    it "Dado un chico, wanda le cumple el primer deseo " $ do
      length (habilidad (wanda chicoSinHabilidadCon2Deseos)) `shouldBe`  1
    it "Dado un chico, wanda lo hace madurar  " $ do
      edad (wanda chicoSinHabilidadCon2Deseos) `shouldBe`  11
  describe "Cosmo" $ do
    it "Dado un chico, cosmo lo hace desmadurar, quedando con la mitad de años de edad" $ do
      edad (cosmo chicoSinHabilidadCon2Deseos) `shouldBe`  5
    it "Como cosmo es olvidadizo, no le concede ningún deseo." $ do
      length (habilidad (cosmo chicoSinHabilidadCon2Deseos)) `shouldBe`  0
  describe "muffinMagico" $ do
    it "Dado un chico le concede todos sus deseos, ser mayor y aprender a bicicletear." $ do
      length (habilidad(muffinMagico chicoSinHabilidadCon2Deseos)) `shouldBe` 1 
      edad (muffinMagico chicoSinHabilidadCon2Deseos) `shouldBe`  18 

-- tieneHabilidad "Conducir" chicoPeligroso `shouldBe` True
-- esSuperMaduro taxiDriver True

--quienConquistaA vicky pretendientes 
-- Chico {nombre = "bobi", edad = 10, habilidad = ["Ser un supermodelo noruego"], deseo = [<una función>]}