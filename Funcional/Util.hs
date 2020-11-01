module Util
(   verificaCadastro,
    autenticaUsuario,
    listar,
    quickSortServicoCategoria
) where

import Dados

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
