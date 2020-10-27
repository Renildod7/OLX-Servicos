import System.IO

menu:: IO()
menu = do
    putStrLn "\nBem vindo a OLX Serviços"
    putStrLn "Gostaria de contratar ou oferecer um seviço?"
    input <- getLine
    if input == "contratar" then menuCliente
    else if input == "q" then putStrLn "Adeus"
    else menu

menuCliente:: IO()
menuCliente = do
    putStrLn "\nJá possui um cadastro?"
    input <- getLine
    if (input == "sim") then loginCliente
    else if (input == "não") then cadastraCliente
    else do
        putStrLn "Não entendi o que você quis dizer"
        menuCliente

menuClienteAutenticado:: IO()
menuClienteAutenticado = do
    putStrLn "\n\nmenuClienteAutenticado ainda n implementado\n"

cadastraCliente:: IO()
cadastraCliente = do
    putStr "Usuário: "
    usuario <- getLine

    putStr "Senha: "
    senha <- getLine

    arq <- openFile "dados.txt" ReadMode
    dados <- hGetLine arq
    hClose arq

    if (clienteCadastrado usuario (read dados) ) then do 
        putStrLn "cliente ja cadastrado"
        menu
    
    else do
        arq <- openFile "dados.txt" WriteMode
        hPutStrLn arq (show ((usuario,senha):(read dados)))
        hClose arq
        putStrLn "Cadastro realizado"
        menuClienteAutenticado

clienteCadastrado:: String -> [(String,String)] -> Bool
clienteCadastrado _ [] = False
clienteCadastrado usuario (x:xs)
        | ((fst x) == usuario) = True
        | otherwise = clienteCadastrado usuario xs

loginCliente:: IO()
loginCliente = do
    putStr "Usuário: "
    usuario <- getLine

    putStr "Senha: "
    senha <- getLine

    arq <- openFile "dados.txt" ReadMode
    dados <- hGetLine arq
    hClose arq

    if (autenticaCliente usuario senha (read dados)) then
        menuClienteAutenticado
    else do
        putStrLn "Usuário ou senha incorretos"
        menu


autenticaCliente:: String -> String -> [(String,String)] -> Bool
autenticaCliente _ _ [] = False
autenticaCliente usuario senha (x:xs)
        | (x ==(usuario,senha)) = True
        | otherwise = autenticaCliente usuario senha xs