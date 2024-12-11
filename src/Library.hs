module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Punto 1

data Chofer = UnChofer {
    nombreChofer :: String,
    kilometraje :: Number,
    viajes :: [Viaje],
    condicion :: Condicion
} deriving(Show, Eq)

data Viaje = UnViaje {
    fecha :: (Number, Number, Number),
    cliente :: Cliente,
    costo :: Number
} deriving(Show, Eq)

data Cliente = UnCliente {
    nombreCliente :: String,
    domicilio :: String
} deriving(Show, Eq)

-- Punto 2

type Condicion = Viaje -> Bool

cualquiera :: Condicion
cualquiera _ = True

masDe200 :: Condicion
masDe200 = (> 200).costo

nombreLargo :: Number -> Condicion
nombreLargo n = (> n).length.nombreCliente.cliente

noViveEn :: String -> Condicion
noViveEn zona = (/= zona).domicilio.cliente

-- Punto 3

lucas :: Cliente
lucas = UnCliente "Lucas" "Victoria"

daniel :: Chofer
daniel = UnChofer "Daniel" 23500 [(UnViaje (20,4,2017) lucas 150)] (noViveEn "Olivos")

alejandra :: Chofer
alejandra = UnChofer "Alejandra" 180000 [] cualquiera

-- Punto 4

puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer = (condicion chofer) viaje

-- Punto 5

liquidacion :: Chofer -> Number
liquidacion = sum.(map costo).viajes

-- Punto 6

quienesTomanViaje :: Viaje -> [Chofer] -> [Chofer]
quienesTomanViaje viaje = filter (puedeTomarViaje viaje)

conMenosViajes :: [Chofer] -> Chofer
conMenosViajes [chofer] = chofer
conMenosViajes (chofer1:chofer2:resto)
    | (length.viajes) chofer1 <= (length.viajes) chofer2 = conMenosViajes (chofer1:resto)
    | otherwise = conMenosViajes (chofer2:resto)

incorporarViaje :: Viaje -> Chofer -> Chofer
incorporarViaje viaje chofer = chofer {viajes = (viajes chofer) ++ [viaje]}

algunoLoRealiza :: Viaje -> [Chofer] -> Bool
algunoLoRealiza viaje choferes = not(null (quienesTomanViaje viaje choferes))

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje = (incorporarViaje viaje).conMenosViajes.(quienesTomanViaje viaje)

-- Punto 7

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

nitoInfy :: Chofer
nitoInfy = UnChofer "Nito Infy" 70000 (repetirViaje (UnViaje (11,3,2017) lucas 50)) (nombreLargo 2)

otroViaje :: Viaje
otroViaje = UnViaje (2,5,2017) lucas 500