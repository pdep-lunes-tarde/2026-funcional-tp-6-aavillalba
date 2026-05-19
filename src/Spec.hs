module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
  describe "TP 6 Composición" $ do

    describe "Parte 1" $ do
      it "El precio final de pdepBurger deberia ser 110" $ do
        precioFinal pdepBurger `shouldBe` 110

    describe "Parte 2" $ do
      it "El precio final del dobleCuarto deberia ser 84" $ do
        precioFinal dobleCuarto `shouldBe` 84
      
      it "El precio final de la bigPdep deberia ser 89" $ do
        precioFinal bigPdep `shouldBe` 89
      
      it "El precio final del dobleCuarto delDia deberia valer 88" $ do
        precioFinal (delDia dobleCuarto) `shouldBe` 88

    describe "Parte 3" $ do
      it "El dobleCuartoVegano deberia reemplazar los ingredientes y calcular su precio correctamente (76)" $ do
        precioFinal dobleCuartoVegano `shouldBe` 76
      
      it "Agrandar una hamburguesa veggie deberia agregar un PatiVegano (PatiVegano cuesta 10)" $ do
        -- cuarto de libra veggie base: 20, PanIntegral(3) + PatiVegano(10) + QuesoAlmendras(15) + PanIntegral(3) = 51
        -- Agrandado suma otro PatiVegano (+10). Total = 61.
        let cuartoVeggieAgrandado = agrandar . cambiarPanDePati . hacerVeggie $ cuartoDeLibra
        precioFinal cuartoVeggieAgrandado `shouldBe` 61