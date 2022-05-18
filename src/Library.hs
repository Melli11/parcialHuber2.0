module Library where
import PdePreludat

data Chofer = Chofer{
    nombre :: String,
    kilometros :: Number,
    viajesRealizados :: [Viaje],
    condicionDeViaje :: CondicionViaje
} deriving (Show)

data Viaje = Viaje {
    fechaDeViaje :: (Dia,Mes,Anio),
    cliente :: Cliente,
    costo :: Number
} deriving (Show)

type Dia = Number
type Mes = Number
type Anio = Number
type Nombre = String
type Domicilio = String
type Cliente = (Nombre,Domicilio)

-- Funciones Auxiliares

nombreDelCliente :: Viaje -> Nombre
nombreDelCliente = fst.cliente

domicilioDelCliente :: Viaje -> Nombre
domicilioDelCliente = snd.cliente

type CondicionViaje = Viaje -> Bool

aceptaCualquierViaje :: CondicionViaje
aceptaCualquierViaje _ = True
aceptaLosViajesMayoresA200 :: CondicionViaje
aceptaLosViajesMayoresA200  =  (>200).costo
aceptaViajeDeAcuerdoAlNombreDelCli :: Number -> CondicionViaje
aceptaViajeDeAcuerdoAlNombreDelCli cantidadLetras = (cantidadLetras == ).length . nombreDelCliente

aceptaViajeSegunZona :: Domicilio -> CondicionViaje
aceptaViajeSegunZona zona viaje =    zona  /=  domicilioDelCliente viaje

aceptaViajeSegunZona' :: Domicilio -> CondicionViaje
aceptaViajeSegunZona' zona =   (/= zona)  .   domicilioDelCliente

-- (1 punto) Definir las siguientes expresiones:
-- a. el cliente “Lucas” que vive en Victoria

clienteLucas :: Cliente
clienteLucas = ("Lucas","Victoria")

-- b. el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente
-- Lucas el 20/04/2017 cuyo costo fue $ 150, y toma los viajes donde el
-- cliente no viva en “Olivos”.

choferDaniel :: Chofer
choferDaniel = Chofer "Daniel" 23500 [Viaje (20,04,2017) clienteLucas 150] (aceptaViajeSegunZona "Olivos")

-- c. la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma
-- cualquier viaje.

choferAlejandra :: Chofer
choferAlejandra = Chofer "Alejandra" 180000 [] aceptaCualquierViaje

-- 4(1 punto) Saber si un chofer puede tomar un viaje.

viajeDeEjemplo = Viaje (20,04,2017) clienteLucas 150


-- puedeTomarViajeUnChofer ::  (Viaje -> Bool) -> Chofer 
puedeTomarViajeUnChofer :: Viaje -> Chofer -> Bool
puedeTomarViajeUnChofer  unViaje unChofer = condicionDeViaje unChofer unViaje

-- En consola 
-- *Spec Library Spec> puedeTomarViajeUnChofer  viajeDeEjemplo  choferDaniel 

