import System.IO

type Clientes = [Cliente]
type Cliente = (Email,Senha)
type Email = String
type Senha = String

type Profissionais = [Profissional]
type Profissional = (Email,Senha)




inicio:: IO()
inicio = do
    arq1 <- openFile "clientes.txt" ReadMode
    clientes <- hGetLine arq1
    hClose arq1

    arq2 <- openFile "profissionais.txt" ReadMode
    profissionais <- hGetLine arq2
    hClose arq2

    menu (read clientes) (read profissionais)

menu:: Clientes -> Profissionais -> IO()
menu clientes profissionais = do
    putStrLn "\nBem vindo a OLX Serviços"
    putStrLn "Gostaria de contratar ou oferecer um seviço?"
    input <- getLine
    if input == "contratar" then menuCliente clientes profissionais
    else if input == "oferecer" then menuProfissional clientes profissionais
    else if input == "q" then putStrLn "Adeus"
    else do
        putStrLn "Não entendi o que você quis dizer" 
        menu clientes profissionais

menuCliente:: Clientes -> Profissionais -> IO()
menuCliente clientes profissionais = do
    putStrLn "\nJá possui um cadastro?"
    input <- getLine
    if (input == "sim") then loginCliente clientes profissionais
    else if (input == "não") then cadastraCliente clientes profissionais
    else do
        putStrLn "Não entendi o que você quis dizer"
        menuCliente clientes profissionais

menuClienteAutenticado::Clientes -> IO()
menuClienteAutenticado clientes = do
    putStrLn "\n\nmenuClienteAutenticado ainda n implementado\n"

cadastraCliente:: Clientes -> Profissionais ->IO()
cadastraCliente clientes profissionais = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (verificaCadastro email clientes ) then do 
        putStrLn "cliente ja cadastrado"
        menu clientes profissionais
    
    else do
        arq <- openFile "clientes.txt" WriteMode
        hPutStrLn arq (show ((email,senha):clientes))
        hClose arq
        putStrLn "Cadastro realizado"
        menuClienteAutenticado clientes

verificaCadastro:: String -> Clientes -> Bool
verificaCadastro _ [] = False
verificaCadastro email (x:xs)
        | ((fst x) == email) = True
        | otherwise = verificaCadastro email xs

loginCliente:: Clientes -> Profissionais -> IO()
loginCliente clientes profissionais = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (autenticaCliente email senha clientes) then
        menuClienteAutenticado clientes
    else do
        putStrLn "E-mail ou senha incorretos"
        menuCliente clientes profissionais


autenticaCliente:: String -> String -> Clientes -> Bool
autenticaCliente _ _ [] = False
autenticaCliente email senha (x:xs)
        | (x ==(email,senha)) = True
        | otherwise = autenticaCliente email senha xs

cadastraProfissional:: Clientes -> Profissionais ->IO()
cadastraProfissional clientes profissionais = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (verificaCadastro email profissionais ) then do 
        putStrLn "cliente ja cadastrado"
        menu clientes profissionais
    
    else do
        arq <- openFile "profissionais.txt" WriteMode
        hPutStrLn arq (show ((email,senha):profissionais))
        hClose arq
        putStrLn "Cadastro realizado"
        menuProfissionalAutenticado profissionais

menuProfissional:: Clientes -> Profissionais -> IO()
menuProfissional clientes profissionais = do
    putStrLn "\nJá possui um cadastro?"
    input <- getLine
    if (input == "sim") then loginProfissional clientes profissionais
    else if (input == "não") then cadastraProfissional clientes profissionais
    else do
        putStrLn "Não entendi o que você quis dizer"
        menuProfissional clientes profissionais

loginProfissional:: Clientes -> Profissionais -> IO()
loginProfissional clientes profissionais = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (autenticaCliente email senha profissionais) then
        menuProfissionalAutenticado profissionais
    else do
        putStrLn "E-mail ou senha incorretos"
        menuProfissional clientes profissionais

menuProfissionalAutenticado:: Profissionais -> IO()
menuProfissionalAutenticado profissionais = do
    putStrLn "\n\nmenuProfissionalAutenticado ainda n implementado\n"