module Spec where
import PdePreludat
import Library
import Test.Hspec


correrTests :: IO ()
correrTests = hspec $ do
  suiteDeTestsDeParte1
  suiteDeTestsDeParte2
  -- suiteDeTestsDeParte3

suiteDeTestsDeParte1 = describe "Parte 1: Criterios " $ do

    describe "Criterios de viajes" $ do
      it "el cliente “Lucas” que vive en Victoria" $ do
        clienteLucas  `shouldBe` ("Lucas","Victoria")
      it "Sea un chofer cuyo  criterio de viajes es : aceptar cualquier viaje, entonces aceptará cualquier viaje" $ do
        aceptaCualquierViaje viajeDeEjemplo `shouldBe` True
      it "Sea un chofer que solo acepta los viajes cuyo costo salgan mas de 200, entoces aceptará un viaje cuyo costo es 400." $ do
        aceptaLosViajesMayoresA200 viajeDeCosto400 `shouldBe` True
      it "Sea un chofer que solo acepta los viajes cuyo costo salgan mas de 200, entoces NO aceptará un viaje cuyo costo es 100." $ do
        aceptaLosViajesMayoresA200 viajeDeCosto100 `shouldBe` False
      it "Sea un chofer que solo acepta los viajes de acuerdo a la longitud del nombre del pasajero," $ do
        aceptaViajeDeAcuerdoAlNombreDelCli 4 viajeDeLeoDeFlores  `shouldBe` False
        aceptaViajeDeAcuerdoAlNombreDelCli 4 viajeDeLucasDeVictoria  `shouldBe` True
      it "Sea un chofer que solo acepta los viajes de acuerdo a la zona donde viva el pasajero " $ do
        aceptaViajesQueNOseanDe "Flores" viajeDeLeoDeFlores `shouldBe` False
        aceptaViajesQueNOseanDe "Flores"  viajeDeLucasDeVictoria `shouldBe` True

suiteDeTestsDeParte2 = describe "Parte 2: Viajes" $ do
    describe "Criterios de viajes" $ do
      it "Dado un chofer y un viaje,el chofer puede realizar el viaje si el viaje cumple con los requisitos" $ do
        puedeTomarViajeUnChofer viajeDeCosto100 choferAlejandra  `shouldBe` True
        puedeTomarViajeUnChofer viajeDeCosto100 choferDaniel  `shouldBe` True
      it "Sea un viaje que no cumple con los requisitos impuestos por el chofer entonces no podrá ser realizado" $ do
        puedeTomarViajeUnChofer viajeDeJuanDeOlivos choferDaniel  `shouldBe` False

-- suiteDeTestsDeParte2 = describe "Parte 2: de Persona a Ranger " $ do

--     describe "convertirEnPowerRanger" $ do
--       it "Dada una persona y un color, entonces convierto a la persona en un Ranger de ese color" $ do
--         convertirEnPowerRanger Verde jony `shouldBe` (Verde,["SuperEspiritu libre","SuperVago"],18)
--         convertirEnPowerRanger Rojo casuality `shouldBe` (Rojo,[],0)
--       it "Dada una lista de habilidades las puedo potenciar  " $ do
--         potenciarHabilidades jony  `shouldBe` ["SuperEspiritu libre","SuperVago"]
--         potenciarHabilidades casuality `shouldBe` []
--       it "Dada una personsa le doy un nivel de pelea acorde a la cantidad de letras de sus habilidades " $ do
--         darNivelDePelea jony `shouldBe` 18
--         darNivelDePelea casuality `shouldBe` 0

-- suiteDeTestsDeParte3 = describe "Parte 4: Buscando al lider " $ do

--     describe "findOrElse" $ do
--       it "Dada una condición, un valor y una lista, devuelve el primer elemento  que cumpla la condición, o el valor dado si ninguno la cumple" $ do
--         findOrElse even 3 [1,7,11] `shouldBe` 3
--         findOrElse even 3 [1,7,2] `shouldBe` 2
--         findOrElse ("a"==) "b" ["a","c","d"] `shouldBe` "a"
--         findOrElse ("a"==) "b" ["x","c","d"] `shouldBe` "b"
--       it "Dada una lista de Rangers que tiene en su equipo al Ranger Rojo entonces él será el lider" $ do
--         rangerLider listaDeRangers  `shouldBe` (Rojo,["Horrible","Necio"],10)
--       it "Dada una lista de Rangers que NO tiene en su equipo al Ranger Rojo entonces  el lider será el primero de la lista , en este caso el Amarillo" $ do
--         rangerLider listaDeRangers2 `shouldBe` (Amarillo,["Horrible","Necio"],10)
