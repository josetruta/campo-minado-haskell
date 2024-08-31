module FunAjuda
(
criarMapa,
revelarMapa,
modificarMapa,
modificarPosicao,
preencheCoordLinha,
retornaValorLinha,
converterListaIntParaString,
pegarValoresMapa,
imprimirMapa,
revelar,
revelarCruz,
revelarMuitos,
marcarBomba,
revelarArredores,
faltaBombasTexto,
geraNumeroAleatorio,
geraTuplaAleatoria,
geraPosicoesAleatorias
)where

import Formigueiro
import System.IO.Unsafe
import System.Random

type Coordenadas = (Int, Int)
type Valor = Int
type Elem = (Coordenadas,Valor)
type Matriz = [Elem]


geraNumeroAleatorio :: Int -> Int -> Int
geraNumeroAleatorio num semente = 
    if (semente `mod` 2 == 0) then
        (((semente * 20) `mod` num) + 1)
    else
        (((semente * 43) `mod` num) + 1)

geraTuplaAleatoria :: Int -> Int -> Int -> Int -> [(Int, Int)] -> (Int, Int)
geraTuplaAleatoria linhas colunas semente1 semente2 lista
    | (x, y) `elem` lista = geraTuplaAleatoria linhas colunas (semente1+137) (semente2+331) lista
    | otherwise = (x, y)
    where x = geraNumeroAleatorio linhas (semente1)
          y = geraNumeroAleatorio colunas (semente2)

geraPosicoesAleatorias :: Int -> Int -> Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
geraPosicoesAleatorias linhas colunas 0 semente1 semente2 lista = lista
geraPosicoesAleatorias linhas colunas bombas semente1 semente2 lista =
    geraPosicoesAleatorias linhas colunas (bombas-1) (semente1 + 59) (semente2 + 277) ((geraTuplaAleatoria linhas colunas semente1 semente2 lista) : lista)

criarMapa:: Int -> Int -> Int -> Matriz
criarMapa a b c = [((x,y), c) | x <-[1,2..a], y <-[1,2..b]]

revelarMapa:: Matriz -> Matriz -> Matriz -> Matriz
revelarMapa [] mtzInterna mtzUsuario = mtzUsuario
revelarMapa (((a,b), c):mtzInternaTail) mtzInterna mtzUsuario = revelarMapa mtzInternaTail mtzInterna (modificarMapa a b mtzInterna mtzUsuario)

modificarMapa:: Int -> Int -> Matriz -> Matriz -> Matriz
modificarMapa x y (((a,b), c):mtz) mtz_usuario = if (x == a && y == b) then ((modificarPosicao x y c mtz_usuario [])) else modificarMapa x y mtz mtz_usuario

modificarPosicao:: Int -> Int -> Int -> Matriz -> Matriz -> Matriz 
modificarPosicao x y z (((a,b), c):mtzUsuario) mtzFinal = if (x == a && y == b) then mtzFinal ++ (([((x, y), z)] ++ mtzUsuario)) else modificarPosicao x y z mtzUsuario (mtzFinal ++ [((a,b), c)])
        
preencheCoordLinha :: Int -> [Int]
preencheCoordLinha coluna =  [ i+1 | i <- [0..coluna]]

preencheLinhaDiv :: Int -> [Int]
preencheLinhaDiv coluna =  [ (-9) | i <- [0..coluna]]

marcarBomba:: Int -> Int -> Matriz -> Matriz -> Matriz
marcarBomba x y (((a,b), c):mtz) mtz_usuario = if (x == a && y == b) then ((modificarPosicao x y (-10) mtz_usuario [])) else marcarBomba x y mtz mtz_usuario

retornaValorLinha :: Int -> Int -> Matriz -> [Int]
retornaValorLinha linha 0 matriz = []
retornaValorLinha linha coluna (((x, y), v):mtz)
    | coluna == 1 = v:[-7, linha]
    | linha == x = v:retornaValorLinha linha (coluna-1) mtz
    | otherwise = retornaValorLinha linha coluna mtz

converterListaIntParaString :: [Int] -> String
converterListaIntParaString [] = ""
converterListaIntParaString (h:t) 
    | h == -2 = "+ " ++ converterListaIntParaString t
    | h == -1 = "B " ++ converterListaIntParaString t
    | h == -10 = "F " ++ converterListaIntParaString t
    | h == -9 = "_ " ++ converterListaIntParaString t
    | h == -7 = "| " ++ converterListaIntParaString t
    | otherwise = show h ++ " " ++ converterListaIntParaString t

pegarValoresMapa :: Int -> Int -> Int -> Matriz -> String
pegarValoresMapa linha linhas colunas matriz
    | linha == (-1) = converterListaIntParaString (preencheCoordLinha (colunas-1)) ++ "\n" ++ pegarValoresMapa(linha+1) linhas colunas matriz
    | linha == 0 = converterListaIntParaString (preencheLinhaDiv (colunas-1)) ++ "\n" ++ pegarValoresMapa(linha+1) linhas colunas matriz
    | linha == (linhas+1) = ""
    | otherwise = converterListaIntParaString (retornaValorLinha linha colunas matriz) ++ "\n" ++ pegarValoresMapa (linha+1) linhas colunas matriz

imprimirMapa :: Int -> Int -> Matriz -> IO()
imprimirMapa linhas colunas matriz = putStrLn (pegarValoresMapa (-1) linhas colunas matriz)

revelar :: Int -> Int -> Matriz -> Bool
revelar x y (((a,b),v):mtzTail) = if (a==x && b==y) then (v/=(-2)) else (revelar x y mtzTail)

revelarCruz :: Int -> Int -> Int -> Int -> Matriz -> Matriz -> Matriz
revelarCruz x y linhas colunas mtzUsuario mtzInterna
    | (x == 1 &&  y == 1) = modificarMapa (x+1) y mtzInterna (modificarMapa x (y+1) mtzInterna mtzUsuario)
    | (x == 1 && y == colunas) = modificarMapa (x+1) y mtzInterna (modificarMapa x (y-1) mtzInterna mtzUsuario)
    | (x == linhas && y == colunas) = modificarMapa (x-1) y mtzInterna (modificarMapa x (y-1) mtzInterna mtzUsuario)
    | (x == linhas && y==1) = modificarMapa (x-1) y mtzInterna (modificarMapa x (y+1) mtzInterna mtzUsuario)
    | (x==1 && y > 1 && y < colunas) = modificarMapa (x+1) y mtzInterna (modificarMapa x (y-1) mtzInterna (modificarMapa x (y+1) mtzInterna mtzUsuario))
    | (x>1 && x < linhas && y==colunas) = modificarMapa (x-1) y mtzInterna (modificarMapa (x+1) (y) mtzInterna (modificarMapa x (y-1) mtzInterna mtzUsuario))
    | (x== linhas && y > 1 && y < colunas) = modificarMapa (x-1) y mtzInterna (modificarMapa x (y-1) mtzInterna (modificarMapa x (y+1) mtzInterna mtzUsuario))
    | (x>1 && x<linhas && y==1) = modificarMapa x (y+1) mtzInterna (modificarMapa (x-1) y mtzInterna (modificarMapa (x+1) y mtzInterna mtzUsuario))
    | otherwise = modificarMapa (x-1) y mtzInterna (modificarMapa (x) (y+1) mtzInterna (modificarMapa (x+1) (y) mtzInterna (modificarMapa x (y-1) mtzInterna mtzUsuario)))

revelarArredores :: Int -> Int -> Int -> Int -> Matriz -> Bool
revelarArredores x y linhas colunas mtzUsuario
    | (x == 1 && y == 1) = (revelar (x) (y+1) mtzUsuario) && (revelar (x+1) (y) mtzUsuario)
    | (x == 1 && y == colunas) = (revelar (x) (y-1) mtzUsuario) && (revelar (x+1) (y) mtzUsuario)
    | (x == linhas && y == colunas) = (revelar (x) (y-1) mtzUsuario) && (revelar (x-1) (y) mtzUsuario)
    | (x == linhas && y == 1) = (revelar (x) (y+1) mtzUsuario) && (revelar (x-1) (y) mtzUsuario)
    | (x == 1 && y > 1 && y < colunas) = (revelar (x) (y+1) mtzUsuario) && (revelar (x) (y-1) mtzUsuario) && (revelar (x+1) (y) mtzUsuario)
    | (x > 1 && x < linhas && y == colunas) = (revelar (x-1) (y) mtzUsuario) && (revelar (x+1) (y) mtzUsuario) && (revelar (x) (y-1) mtzUsuario)
    | (x == linhas && y > 1 && y < colunas) = (revelar (x-1) (y) mtzUsuario) && (revelar (x) (y-1) mtzUsuario) && (revelar (x) (y+1) mtzUsuario)
    | (x > 1 && x < linhas && y == 1) = (revelar (x) (y+1) mtzUsuario) && (revelar (x-1) (y) mtzUsuario) && (revelar (x+1) (y) mtzUsuario)
    | otherwise = (revelar (x-1) (y) mtzUsuario) && (revelar (x) (y+1) mtzUsuario) && (revelar (x+1) (y) mtzUsuario) && (revelar (x) (y-1) mtzUsuario)  

revelarMuitos :: Int -> Int -> Matriz -> Matriz -> Matriz -> Matriz
revelarMuitos linhas colunas [] mtzUsuario mtzInterna = mtzUsuario
revelarMuitos linhas colunas (((x, y), z):mtzUsuarioTail) mtzUsuario mtzInterna = 
    if (z /= 0) then
        revelarMuitos linhas colunas mtzUsuarioTail mtzUsuario mtzInterna
    else 
        if (revelarArredores x y linhas colunas mtzUsuario) then
            revelarMuitos linhas colunas mtzUsuarioTail mtzUsuario mtzInterna
        else
            revelarMuitos linhas colunas (revelarCruz x y linhas colunas mtzUsuario mtzInterna) (revelarCruz x y linhas colunas mtzUsuario mtzInterna) mtzInterna

faltaBombas :: Int -> Matriz -> Int
faltaBombas contador [] = contador
faltaBombas contador (((x, y), value) : matriz) = if (value == -2) then 
    (faltaBombas (contador+1) matriz) 
    else 
        (faltaBombas (contador) matriz)

faltaBombasTexto :: Int -> Matriz -> IO()         
faltaBombasTexto bombas matriz = do
    let qtBombas = faltaBombas 0 matriz
    let bombasPossiveis = qtBombas - bombas
    if (bombasPossiveis <= 0 ) then do
        putStrLn "---------------------------------------------"
        putStrLn ("Quantidade de BOMBAS:"++ show bombas ++ "  ")
        putStrLn "BOMBAS: 0"
        putStrLn "---------------------------------------------"
    else do
        putStrLn "---------------------------------------------"
        putStrLn ("Quantidade de BOMBAS:"++ show bombas ++"  ")
        putStrLn ("BOMBAS: " ++ show bombasPossiveis ++"")                           
        putStrLn "---------------------------------------------"
        

