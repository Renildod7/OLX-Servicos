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


-- Verifica se um cliente ou um profissional esta cadastrado no sistema
verificaCadastro:: String -> [(String,String)] -> Bool
verificaCadastro _ [] = False
verificaCadastro email (x:xs)
        | ((fst x) == email) = True
        | otherwise = verificaCadastro email xs


-- Verifica se o email e senha informados estao corretos
autenticaUsuario:: String -> String -> [(String,String)] -> Bool
autenticaUsuario _ _ [] = False
autenticaUsuario email senha (x:xs)
        | (x ==(email,senha)) = True
        | otherwise = autenticaUsuario email senha xs


-- Lista as String de uma lista uma abaixo da outra
listarString:: [String] -> String
listarString [] = ""
listarString (x:[]) = x 
listarString (x:xs) = x ++"\n" ++  listarString xs


-- Ordena os servicos pela categoria
quickSortServicoCategoria:: Servicos -> Servicos
quickSortServicoCategoria [] = []
quickSortServicoCategoria (s:xs) = quickSortServicoCategoria [x | x <- xs, (getCategoria x) < (getCategoria s)]
                                   ++ [s] ++
                                   quickSortServicoCategoria [x | x <- xs,(getCategoria x) >= (getCategoria s)]


-- ordena os servicos pela avaliacao
quickSortServicoAvaliacao:: Servicos -> Servicos
quickSortServicoAvaliacao [] = []
quickSortServicoAvaliacao (s:xs) = quickSortServicoAvaliacao [x | x <- xs, (mediaAvaliacaoServico x) > (mediaAvaliacaoServico s)]
                                   ++ [s] ++
                                   quickSortServicoAvaliacao [x | x <- xs,(mediaAvaliacaoServico x) <= (mediaAvaliacaoServico s)]


-- Verifica se uma das Strings da primeira lista esta na segunda lista
procuraPalavras:: [String] -> [String] -> Bool
procuraPalavras [] _ = False
procuraPalavras (x:xs) palavras = if (elem x palavras) then True
                      else procuraPalavras xs palavras


-- Verifica se a String pode ser convertida para Int
validaInt:: String -> Bool
validaInt str 
        | (readMaybe str :: Maybe Int) == Nothing = False
        | (readMaybe str :: Maybe Int) /= Nothing = True

-- Verifica se a String pode ser convertida para Float
validaFloat:: String -> Bool
validaFloat str 
        | (readMaybe str :: Maybe Float) == Nothing = False
        | (readMaybe str :: Maybe Float) /= Nothing = True


-- Remove os dados apresentados na tela
limpaTela :: IO()
limpaTela = do
    _ <- System.Process.system "clear"
    return ()


-- Faz o sistema parar por 1 segundo
pausa1s :: IO()
pausa1s = do
    _ <- System.Process.system "sleep 1s"
    return ()


-- Faz o sistema parar por 2 segundos
pausa2s :: IO()
pausa2s = do
    _ <- System.Process.system "sleep 2s"
    return ()


-- Faz o sistema parar por 3 segundos
pausa3s :: IO()
pausa3s = do
    _ <- System.Process.system "sleep 3s"
    return ()


-- Faz o sistema parar por 5 segundos
pausa5s :: IO()
pausa5s = do
    _ <- System.Process.system "sleep 5s"
    return ()