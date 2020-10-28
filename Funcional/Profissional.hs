module Profissional
(   menuProfissional
) where

import System.IO
import Dados
import Util


menuProfissional:: Dados -> IO()
menuProfissional dados = do
    putStrLn "\nJá possui um cadastro?"
    input <- getLine
    if (input == "sim") then loginProfissional dados
    else if (input == "não") then cadastraProfissional dados
    else do
        putStrLn "Não entendi o que você quis dizer"
        menuProfissional dados

menuProfissionalAutenticado:: Dados -> IO()
menuProfissionalAutenticado dados = do
    putStrLn "\n\nmenuProfissionalAutenticado ainda n implementado\n"

cadastraProfissional:: Dados ->IO()
cadastraProfissional dados@(clientes, profissionais) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (verificaCadastro email profissionais ) then do 
        putStrLn "cliente ja cadastrado"

    else do
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (clientes,((email,senha):profissionais)))
        hClose arq
        putStrLn "Cadastro realizado"
        menuProfissionalAutenticado (clientes,((email,senha):profissionais))

loginProfissional:: Dados -> IO()
loginProfissional dados@(clientes,profissionais) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (autenticaUsuario email senha profissionais) then
        menuProfissionalAutenticado dados
    else do
        putStrLn "E-mail ou senha incorretos"
        menuProfissional dados

