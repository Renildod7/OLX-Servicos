import System.IO
import Dados
import Cliente
import Profissional

main = do 
    inicio

inicio:: IO()
inicio = do
    arq3 <- openFile arquivoDados ReadMode
    dados <- hGetLine arq3
    hClose arq3
    menu (read dados)

menu:: Dados -> IO()
menu dados = do
    putStrLn "\nBem vindo a OLX Serviços"
    putStrLn "Gostaria de contratar ou oferecer um seviço?"
    input <- getLine
    if input == "contratar" then menuC dados
    else if input == "oferecer" then menuProfissional dados
    else if input == "q" then putStrLn "Adeus"
    else do
        putStrLn "Não entendi o que você quis dizer" 
        menu dados

menuC:: Dados -> IO()
menuC dados = do
    menuCliente dados
    menu dados