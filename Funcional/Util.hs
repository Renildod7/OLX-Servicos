module Util
(   verificaCadastro,
    autenticaUsuario
) where

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