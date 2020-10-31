import System.IO
import Data.Char
import Dados
import Util

main = do 
    inicio

inicio:: IO()
inicio = do
    arq3 <- openFile arquivoDados ReadMode
    dados <- hGetLine arq3
    hClose arq3
    putStrLn "\nBem vindo a OLX Serviços"
    menuCadastro (read dados)



menuCadastro:: Dados -> IO()
menuCadastro dados = do
    putStrLn "\nComo Poderiamos lhe ajudar"
    input <- getLine
    let palavras = [map toLower x | x <- (words input)]
    if ((elem "cadastrar" palavras) && (elem "cliente" palavras)) then cadastraCliente dados
    else if ((elem "cadastrar" palavras) && (elem "profissional" palavras)) then cadastraProfissional dados
    else if ((elem "login" palavras) && (elem "cliente" palavras)) then loginCliente dados
    else if ((elem "login" palavras) && (elem "profissional" palavras)) then loginProfissional dados
    else do
        putStrLn "Não entendi, poderia repetir?"
        menuCadastro dados






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
cadastraCliente dados@(clientes, profissionais,servicos) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    putStr "Nome: "
    nome <- getLine

    putStr "Endereco: "
    endereco <- getLine

    putStr "Telefone: "
    telefone <- getLine

    if (verificaCadastro email (getListaEmailSenhaCliente clientes) ) then do 
        putStrLn "cliente ja cadastrado"

    else do
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (((email,senha,nome,endereco,telefone):clientes),profissionais,servicos))
        hClose arq
        putStrLn "Cadastro realizado"
        menuClienteAutenticado (((email,senha,nome,endereco,telefone):clientes),profissionais,servicos)

loginCliente:: Dados -> IO()
loginCliente dados@(clientes,_,_) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (autenticaUsuario email senha (getListaEmailSenhaCliente clientes)) then
        menuClienteAutenticado dados
    else do
        putStrLn "E-mail ou senha incorretos"
        menuCliente dados



menuProfissionalAutenticado:: Dados -> Profissional-> IO()
menuProfissionalAutenticado dados@(_,profissionais,servicos) profissional@(email,_,nome,_,_) = do
    putStr "Bem Vindo "
    putStr nome
    putStrLn " !"
    putStrLn "Como posso ajuda-lo?\n"

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]

    if ((elem "cadastrar" palavras) && (elem "serviço" palavras)) then cadastraServico dados email
    else do
        putStrLn "Não entendi, poderia repetir?"
        menuProfissionalAutenticado dados profissional




    

cadastraProfissional:: Dados ->IO()
cadastraProfissional dados@(clientes, profissionais,servicos) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    putStr "Nome: "
    nome <- getLine

    putStr "Endereco: "
    endereco <- getLine

    putStr "Telefone: "
    telefone <- getLine

    if (verificaCadastro email (getListaEmailSenhaProfissional profissionais) ) then do 
        putStrLn "cliente ja cadastrado"

    else do
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (clientes,((email,senha,nome,endereco,telefone):profissionais),servicos))
        hClose arq
        putStrLn "Cadastro realizado"
        menuProfissionalAutenticado (clientes,((email,senha,nome,endereco,telefone):profissionais),servicos) (getProfissional profissionais email senha)


loginProfissional:: Dados -> IO()
loginProfissional dados@(_,profissionais,_) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (autenticaUsuario email senha (getListaEmailSenhaProfissional profissionais)) then
        menuProfissionalAutenticado dados (getProfissional profissionais email senha)
    else do
        putStrLn "E-mail ou senha incorretos"
        menuCadastro dados


cadastraServico:: Dados -> Email -> IO()
cadastraServico dados@(clientes,profissionais,servicos) emailP = do
    
    putStrLn "Informe os seguintes dados para realizar o cadastro\n"

    putStr "Descrição: "
    descricao <- getLine

    putStr "Preço: "
    preco <- getLine

    putStrLn "\nEscolha uma dentre as seguintes categorias"
    putStrLn "<pressione Enter para seguir>"
    getLine
    putStrLn (listar especialidades)
    putStrLn ""
    
    putStr "Categoria: "
    categoria <- getLine
    
    arq <- openFile arquivoDados WriteMode
    hPutStrLn arq (show (clientes,profissionais,((categoria,descricao,(read preco),emailP):servicos)))
    hClose arq

    putStr "asdasdasdasdasds"

