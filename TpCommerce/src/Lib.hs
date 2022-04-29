type Producto = (String, Int)

bufanda :: Producto
bufanda = ("bufanda", 8000)

vocales :: String
vocales = "aeiouáéíóúAEIOUÁÉÍÓÚ"

productoCorriente :: Producto -> Bool
productoCorriente (nombre, _) = elem (head nombre) vocales

contieneX :: String -> Bool
contieneX = elem 'x'
contieneY :: String -> Bool
contieneY = elem 'y'

productoDeLujo :: Producto -> Bool
productoDeLujo (nombre, _) = contieneX nombre || contieneY nombre

productoCodiciado :: Producto -> Bool
productoCodiciado (nombre, _) = (>10) . length $ nombre

descodiciarProducto :: Producto -> Producto
descodiciarProducto (nombre, precio) = (take 10 nombre, precio)

productoDeElite :: Producto -> Bool
productoDeElite producto = (productoDeLujo producto) && (productoCodiciado producto) && (not . productoCorriente $ producto)

productoXL :: Producto -> Producto
productoXL (nombre, precio) = (nombre ++ " XL", precio)

darVueltaNombre :: Producto -> Producto
darVueltaNombre (nombre, precio) = (reverse nombre, precio)

versionBarata :: Producto -> Producto
versionBarata producto = darVueltaNombre . descodiciarProducto $ producto

aplicarDescuento :: Producto -> Int -> Producto
aplicarDescuento (nombre, precio) descuento = (nombre, precio - descuento)

entregaSencilla :: String -> Bool
entregaSencilla fecha = even . length $ fecha

precioTotal :: Producto -> Int -> Int -> Int -> Producto
precioTotal (nombre, precio) cantidad descuento costoDeEnvio = (nombre, (+costoDeEnvio) . (*cantidad) . (subtract descuento) $ precio )