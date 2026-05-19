module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu | Papas | PatiVegano | PanIntegral
    deriving (Eq, Show)

precioIngrediente :: Ingrediente -> Number
precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente Papas = 10
precioIngrediente PatiVegano = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = precioBase hamburguesa + sum (map precioIngrediente(ingredientes hamburguesa))

cuartoDeLibra :: Hamburguesa
cuartoDeLibra = Hamburguesa 20 [Pan, Carne, Cheddar, Pan]

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = hamburguesa { ingredientes = ingrediente : ingredientes hamburguesa }

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = hamburguesa { precioBase = precioBase hamburguesa * (100 - porcentaje) / 100 }

esIngredienteBase :: Ingrediente -> Bool
esIngredienteBase Carne = True
esIngredienteBase Pollo = True
esIngredienteBase PatiVegano = True
esIngredienteBase _ = False

ingredienteBase :: Hamburguesa -> Ingrediente
ingredienteBase hamburguesa = head (filter esIngredienteBase (ingredientes hamburguesa))

agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente (ingredienteBase hamburguesa) hamburguesa

pdepBurger :: Hamburguesa
pdepBurger = descuento 20 . agregarIngrediente Cheddar . agregarIngrediente Panceta . agrandar . agrandar $ cuartoDeLibra


-- =================================================================
-- PARTE 2: Algunas hamburguesas más
-- =================================================================

dobleCuarto :: Hamburguesa
dobleCuarto = agregarIngrediente Cheddar . agregarIngrediente Carne $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

-- APLICACIÓN PARCIAL (No recibe la hamburguesa explícitamente, se infiere)
delDia :: Hamburguesa -> Hamburguesa
delDia = descuento 30 . agregarIngrediente Papas


-- =================================================================
-- PARTE 3: Modificadores Veganos
-- =================================================================

transformarIngredientes :: (Ingrediente -> Ingrediente) -> Hamburguesa -> Hamburguesa
transformarIngredientes transformador hamburguesa = hamburguesa { ingredientes = map transformador (ingredientes hamburguesa) }

reemplazarVegano :: Ingrediente -> Ingrediente
reemplazarVegano Carne   = PatiVegano
reemplazarVegano Pollo   = PatiVegano
reemplazarVegano Cheddar = QuesoDeAlmendras
reemplazarVegano Panceta = BaconDeTofu
reemplazarVegano otro    = otro 

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = transformarIngredientes reemplazarVegano

reemplazarPan :: Ingrediente -> Ingrediente
reemplazarPan Pan  = PanIntegral
reemplazarPan otro = otro

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati = transformarIngredientes reemplazarPan

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePati . hacerVeggie $ dobleCuarto