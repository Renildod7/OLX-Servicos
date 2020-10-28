module Cliente
(   menuCliente
) where

import System.IO
import Dados
import Util

menuCliente:: Dados -> IO()
menuCliente dados = do
    putStrLn "\nJá possui um cadastro?"
    input <- getLine
    if (input == "sim") then loginCliente dados
    else if (input == "não") then cadastraCliente dados
    else do
        putStrLn "Não entendi o que você quis dizer"
        menuCliente dados

menuClienteAutenticado::Dados -> IO()
menuClienteAutenticado dados = do
    putStrLn "\n\nmenuClienteAutenticado ainda n implementado\n"

cadastraCliente:: Dados ->IO()
cadastraCliente dados@(clientes, profissionais) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (verificaCadastro email clientes ) then do 
        putStrLn "cliente ja cadastrado"

    
    else do
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (((email,senha):clientes),profissionais))
        hClose arq
        putStrLn "Cadastro realizado"
        menuClienteAutenticado (((email,senha):clientes),profissionais)

loginCliente:: Dados -> IO()
loginCliente dados@(clientes, profissionais) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (autenticaUsuario email senha clientes) then
        menuClienteAutenticado clientes
    else do
        putStrLn "E-mail ou senha incorretos"
        menuCliente dados
