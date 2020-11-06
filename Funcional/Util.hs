module Util
(   verificaCadastro,
    autenticaUsuario,
    listarString,
    quickSortServicoCategoria,
    quickSortServicoAvaliacao,
    procuraPalavras,
    validaInt,
    validaFloat,
    limpaTela,
    pausa3s,
    pausa2s,
    pausa1s,
    pausa5s
) where

import Dados
import Data.Char
import qualified System.Process
import Text.Read

verificaCadastro:: String -> [(String,String)] -> Bool
verificaCadastro _ [] = False
verificaCadastro email (x:xs)
        | ((fst x) == email) = True
        | otherwise = verificaCadastro email xs

autenticaUsuario:: String -> String -> [(String,String)] -> Bool
autenticaUsuario _ _ [] = False
autenticaUsuario email senha (x:xs)
        | (x ==(email,senha)) = True
        | otherwise = autenticaUsuario email senha xs

listarString:: [String] -> String
listarString [] = ""
listarString (x:[]) = x 
listarString (x:xs) = x ++"\n" ++  listarString xs

quickSortServicoCategoria:: Servicos -> Servicos
quickSortServicoCategoria [] = []
quickSortServicoCategoria (s:xs) = quickSortServicoCategoria [x | x <- xs, (getCategoria x) < (getCategoria s)]
                                   ++ [s] ++
                                   quickSortServicoCategoria [x | x <- xs,(getCategoria x) >= (getCategoria s)]

quickSortServicoAvaliacao:: Servicos -> Servicos
quickSortServicoAvaliacao [] = []
quickSortServicoAvaliacao (s:xs) = quickSortServicoAvaliacao [x | x <- xs, (mediaAvaliacaoServico x) > (mediaAvaliacaoServico s)]
                                   ++ [s] ++
                                   quickSortServicoAvaliacao [x | x <- xs,(mediaAvaliacaoServico x) <= (mediaAvaliacaoServico s)]


procuraPalavras:: [String] -> [String] -> Bool
procuraPalavras [] _ = False
procuraPalavras (x:xs) palavras = if (elem x palavras) then True
                      else procuraPalavras xs palavras


validaInt:: String -> Bool
validaInt str 
        | (readMaybe str :: Maybe Int) == Nothing = False
        | (readMaybe str :: Maybe Int) /= Nothing = True

validaFloat:: String -> Bool
validaFloat str 
        | (readMaybe str :: Maybe Float) == Nothing = False
        | (readMaybe str :: Maybe Float) /= Nothing = True








limpaTela :: IO()
limpaTela = do
    _ <- System.Process.system "clear"
    return ()

pausa1s :: IO()
pausa1s = do
    _ <- System.Process.system "sleep 1s"
    return ()

pausa2s :: IO()
pausa2s = do
    _ <- System.Process.system "sleep 2s"
    return ()

pausa3s :: IO()
pausa3s = do
    _ <- System.Process.system "sleep 3s"
    return ()

pausa5s :: IO()
pausa5s = do
    _ <- System.Process.system "sleep 5s"
    return ()