module Trabalho
(
inserirBomba,
inserirBombas,
somaBombas,
distBombas,
somaProxBomba,
verificaBomba,
auxiliarSoma,
verificarSoma
)where

type Coordenadas = (Int, Int)
type Valor = Int
type Elem = (Coordenadas,Valor)
type Matriz = [Elem]

inserirBomba:: Int -> Int -> Matriz -> Matriz -> Matriz
inserirBomba a b [] mtzFinal = mtzFinal
inserirBomba a b (((x, y), z): mtz) mtzFinal = 
    if(a == x && b == y) then
        mtzFinal++[((x, y), -1)]++mtz 
    else
        inserirBomba a b mtz (mtzFinal++[((x, y), z)])

inserirBombas :: [(Int, Int)] -> Matriz -> Matriz
inserirBombas [] mtz = mtz
inserirBombas ((x, y): mtzTail) mtz =  inserirBombas mtzTail (inserirBomba x y mtz []) 

verificarSoma:: Int -> Int -> Matriz -> Matriz-> Matriz
verificarSoma num1 num2 [] anterior = [ ]
verificarSoma num1 num2 (((x, y), z): mtz) anterior = if(num1 == x && num2 == y && z /= -1) then anterior++(reverse ([((x,y), z+1)]++ mtz)) else verificarSoma num1 num2 mtz  anterior++[((x,y), z)]

auxiliarSoma:: Int -> Int -> Matriz -> Matriz-> Matriz
auxiliarSoma num1 num2 matriz anterior = reverse(verificarSoma num1 num2 matriz anterior)

somaBombas :: Int -> Matriz -> Int
somaBombas num [] = num
somaBombas num (((x, y), v) : mtz) = if (v == -2) then (somaBombas (num+1) mtz) else (somaBombas (num) mtz)

distBombas:: Matriz -> Matriz -> Matriz
distBombas [] matriz = matriz
distBombas (((x, y), z): mtz) matriz = if (z == -1) then distBombas mtz (somaProxBomba x y matriz) else (distBombas mtz matriz)

somaProxBomba:: Int -> Int -> Matriz -> Matriz
somaProxBomba x y mtz = auxiliarSoma x (y-1) (auxiliarSoma x (y+1) (auxiliarSoma (x-1) y (auxiliarSoma (x-1) (y+1) (auxiliarSoma (x-1) (y-1) (auxiliarSoma (x+1) y (auxiliarSoma (x+1) (y+1) (auxiliarSoma (x+1) (y-1) mtz [])[])[])[])[])[])[])[]

verificaBomba :: (Int, Int) -> Matriz -> Bool
verificaBomba tupla [] = False
verificaBomba (x, y) (((a, b), c):mtzTail) = 
    if (x == a && y == b && c == -1) then 
        True 
    else 
        verificaBomba (x, y) mtzTail