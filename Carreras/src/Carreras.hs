module Carreras where

import Text.Show.Functions

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista


--Punto 1
data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
} deriving (Show, Eq)

type Carrera = [Auto]
type Evento = Carrera -> Carrera
type Color = String
type Posicion = (Int, Color)
type AlterarVelocidad = Int->Int
type PowerUp = Auto->Carrera->Carrera

mario::Auto
luigi::Auto
pitch::Auto
donkey::Auto
yoshi::Auto
mario = UnAuto "rojo" 120 0
luigi = UnAuto "verde" 120 0
pitch = UnAuto "rosa" 120 0
donkey = UnAuto "marron" 120 0
yoshi = UnAuto "amarillo" 120 0
competidores::[Auto]
competidores = [mario,luigi,pitch,donkey,yoshi]
eventosEj::[Evento]
eventosEj = [correnTodos 30, usaPowerUp "verde" (jetPack 10), usaPowerUp "rojo" terremoto, correnTodos 40, usaPowerUp "rojo" (miguelitos 20), usaPowerUp "marron" (jetPack 6), correnTodos 10]

--1a
estanCerca :: Auto->Auto->Bool
estanCerca auto1 auto2 = not (sonElMismoAuto auto1 auto2) &&  10 > abs ((distanciaRecorrida auto1) - (distanciaRecorrida auto2))

sonElMismoAuto :: Auto->Auto->Bool
sonElMismoAuto auto1 auto2 = color auto1 == color auto2

--1b
vaTranquilo::Auto->Carrera->Bool
vaTranquilo auto carrera= (not $ tieneAutoCerca auto (autosCompetidores auto carrera)) && vaGanando auto (autosCompetidores auto carrera)

tieneAutoCerca::Auto->Carrera->Bool
tieneAutoCerca auto = any (estanCerca auto)

vaGanando::Auto->Carrera->Bool
vaGanando auto = all (recorrioMas auto)

recorrioMas::Auto->Auto->Bool
recorrioMas auto1 auto2 = distanciaRecorrida auto1 > distanciaRecorrida auto2

autosCompetidores :: Auto->Carrera->Carrera
autosCompetidores auto = filter (not.sonElMismoAuto auto) 

--1c
puestoAuto::Auto->Carrera->Posicion
puestoAuto auto carrera = (1 + (length . filter (flip recorrioMas auto)) (autosCompetidores auto carrera), color auto)

--Punto 2
--2a
recorrerDurante ::Auto->Int->Auto
recorrerDurante auto tiempo = UnAuto (color auto) (velocidad auto) (distanciaRecorrida auto + (tiempo * velocidad auto))

--2bi

modificarVelocidad::AlterarVelocidad->Auto->Auto
modificarVelocidad operador auto = auto {velocidad = max 0 $ operador (velocidad auto)}


--2bii
bajarVelocidad::Int->Auto->Auto
bajarVelocidad cantidad = modificarVelocidad (flip (-) cantidad) 
--Punto
--3a


terremoto::PowerUp
terremoto auto carrera = auto : afectarALosQueCumplen (estanCerca auto) (bajarVelocidad 50) (autosCompetidores auto carrera)

miguelitos::Int->PowerUp
miguelitos cantidad auto carrera = auto : afectarALosQueCumplen (recorrioMas auto) (bajarVelocidad cantidad) (autosCompetidores auto carrera)

jetPack::Int->PowerUp
jetPack cantidad auto carrera = afectarALosQueCumplen (sonElMismoAuto auto) (efectoJetPack cantidad) carrera

efectoJetPack::Int->Auto->Auto
efectoJetPack cantidad auto = auto {distanciaRecorrida = distanciaRecorrida $ recorrerJetPack auto cantidad} 

recorrerJetPack :: Auto->Int->Auto
recorrerJetPack auto = recorrerDurante(auto {velocidad = velocidad $ duplicarVelocidad auto})

duplicarVelocidad::Auto->Auto
duplicarVelocidad = modificarVelocidad (*2)

--Punto 4
--4a

--ordenarPorPosicion::[Posicion]->[Posicion]
--ordenarPorPosicion posiciones = undefined

obtenerPosiciones::Carrera->[Posicion]
obtenerPosiciones carrera = map (flip puestoAuto carrera) carrera


simularCarrera :: Carrera -> [Evento] -> [Posicion]
simularCarrera carrera = obtenerPosiciones . foldr ($) carrera

correnTodos :: Int->Evento
correnTodos tiempo = map (flip recorrerDurante tiempo) 

usaPowerUp :: Color->PowerUp->Evento
usaPowerUp colorAuto powerUp carrera = powerUp (encontrarAuto colorAuto carrera) carrera

encontrarAuto::Color->Carrera->Auto
encontrarAuto colorAuto = head.filter ((==) colorAuto . color )



