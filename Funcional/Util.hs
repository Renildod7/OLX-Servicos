module Util
(   verificaCadastro,
    autenticaUsuario,
    listar,
    quickSortServicoCategoria,
    quickSortServicoAvaliacao,
    asd,
    limpaTela
) where

import Dados
import Data.Char
import qualified System.Process

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

listar:: [String] -> String
listar [] = ""
listar (x:[]) = x 
listar (x:xs) = x ++"\n" ++  listar xs

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


limpaTela :: IO()
limpaTela = do
    _ <- System.Process.system "clear"
    return ()


asd:: [String] -> [String] -> Bool
asd [] _ = False
asd (x:xs) palavras = if (elem x palavras) then True
                      else asd xs palavras


