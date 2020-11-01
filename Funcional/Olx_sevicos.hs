import System.IO
import Data.Char
import Dados
import Util

main = do 
    inicio

inicio:: IO()
inicio = do
    arq <- openFile arquivoDados ReadMode
    dados <- hGetLine arq
    hClose arq
    putStrLn "\nBem vindo a OLX Serviços"
    menuCadastro (read dados)



menuCadastro:: Dados -> IO()
menuCadastro dados = do
    putStrLn "\nComo poderiamos lhe ajudar?"
    input <- getLine
    let palavras = [map toLower x | x <- (words input)]
    if ((elem "cadastrar" palavras) && (elem "cliente" palavras)) then cadastraCliente dados
    else if ((elem "cadastrar" palavras) && (elem "profissional" palavras)) then cadastraProfissional dados
    else if ((elem "login" palavras) && (elem "cliente" palavras)) then loginCliente dados
    else if ((elem "login" palavras) && (elem "profissional" palavras)) then loginProfissional dados
    else if (elem "ajuda" palavras) then ajuda dados
                                    
    else do
        putStrLn "Não entendi, poderia repetir?"
        putStrLn "Caso precise de ajuda digite Ajuda."
        menuCadastro dados

ajuda:: Dados -> IO()
ajuda dados = do
    putStrLn ""
    putStrLn "Informe a ação que quer realizar seguido do tipo de usuario."
    putStrLn "É possivel se cadastrar e logar, como cliente ou como profissional."
    putStr "Por exemplo: "
    putStr (show "Eu gostaria de me cadastrar como um cliente")
    putStrLn "."
    menuCadastro dados


menuClienteAutenticado::Dados -> Cliente -> IO()
menuClienteAutenticado dados@(clientes,_,_,_) cliente@(_,_,nome,_,_) = do

    putStr "\nBem Vindo "
    putStr nome
    putStrLn " !"
    putStrLn "Como posso ajuda-lo?\n"

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]
    if ((elem "listar" palavras) && (elem "serviços" palavras)) then menuClienteAutenticado dados cliente
    else if ((elem "contratar" palavras) && (elem "serviço" palavras)) then contratarServico dados cliente
    else if (elem "ajuda" palavras) then ajudaCliente dados cliente
    else do
        putStrLn "Não entendi, poderia repetir?"
        putStrLn "Caso precise de ajuda digite Ajuda."
        menuCadastro dados






-- Ajuda cliente não implementada.



ajudaCliente:: Dados -> Cliente -> IO()
ajudaCliente dados@(clientes,_,_,_) cliente = do
    putStrLn ""
    putStrLn "Informe a ação que quer realizar seguido do tipo de usuario."
    putStrLn "É possivel se cadastrar e logar, como cliente ou como profissional."
    putStr "Por exemplo: "
    putStr (show "Eu gostaria de me cadastrar como um cliente")
    putStrLn "."
    menuClienteAutenticado dados cliente


contratarServico:: Dados -> Cliente -> IO()
contratarServico dados cliente = do 

    putStrLn "Gostaria de escolher uma categoria de serviços especifica ou listar todos os serviços disponiveis?"

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]
    if ((elem "listar" palavras) && (elem "todos" palavras)) then contratarServicoListagem dados cliente
    else if ((elem "categoria" palavras) && (elem "especifica" palavras)) then contratarServico dados cliente
    else do
        putStrLn "Não entendi, poderia repetir?"
        contratarServico dados cliente

contratarServicoListagem:: Dados -> Cliente -> IO()
contratarServicoListagem dados@(clientes,profissionais,servicos,(atPendentes,atConfirmados,atRecusados,atConcluidos)) cliente@(emailC,_,_,_,_) = do

    let servicosOrd = quickSortServicoCategoria servicos

    if servicosOrd == [] then putStrLn "Desculpe, não possuimos nenhum serviço disponivel no momento."
    else do
        putStrLn "Estes são todos os serviços que temos disponiveis no momento."
        putStrLn (listarTodosServicos servicosOrd)
        putStrLn "Caso queira contratar algum deles informe o número do serviço correspondente."
        putStrLn "Caso não queira constratar nenhum deles digite sair"

        input <- getLine
        let servico = head (drop ((read input)-1) servicosOrd)
        
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (clientes,profissionais,servicos,(((emailC,servico):atPendentes),atConfirmados,atRecusados,atConcluidos)))
        hClose arq
        putStrLn "Cadastro realizado"
        menuClienteAutenticado (clientes,profissionais,servicos,(((emailC,servico):atPendentes),atConfirmados,atRecusados,atConcluidos)) cliente

        






cadastraCliente:: Dados ->IO()
cadastraCliente dados@(clientes,profissionais,servicos,atendimentos) = do
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
        hPutStrLn arq (show (((email,senha,nome,endereco,telefone):clientes),profissionais,servicos,atendimentos))
        hClose arq
        putStrLn "Cadastro realizado"
        menuClienteAutenticado (((email,senha,nome,endereco,telefone):clientes),profissionais,servicos,atendimentos) (getCliente clientes email senha)

loginCliente:: Dados -> IO()
loginCliente dados@(clientes,_,_,_) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (autenticaUsuario email senha (getListaEmailSenhaCliente clientes)) then
        menuClienteAutenticado dados (getCliente clientes email senha)
    else do
        putStrLn "E-mail ou senha incorretos"
        menuCadastro dados













menuProfissionalAutenticado:: Dados -> Profissional-> IO()
menuProfissionalAutenticado dados@(_,profissionais,servicos,_) profissional@(email,_,nome,_,_) = do
    putStr "Bem Vindo "
    putStr nome
    putStrLn " !"
    putStrLn "Como posso ajuda-lo?\n"

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]

    if ((elem "cadastrar" palavras) && (elem "serviço" palavras)) then cadastraServico dados email nome
    else do
        putStrLn "Não entendi, poderia repetir?"
        menuProfissionalAutenticado dados profissional




    

cadastraProfissional:: Dados ->IO()
cadastraProfissional dados@(clientes, profissionais,servicos,atendimentos) = do
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
        putStrLn "Profissional já cadastrado"

    else do
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (clientes,((email,senha,nome,endereco,telefone):profissionais),servicos,atendimentos))
        hClose arq
        putStrLn "Cadastro realizado"
        menuProfissionalAutenticado (clientes,((email,senha,nome,endereco,telefone):profissionais),servicos,atendimentos) (getProfissional profissionais email senha)


loginProfissional:: Dados -> IO()
loginProfissional dados@(_,profissionais,_,_) = do
    putStr "E-mail: "
    email <- getLine

    putStr "Senha: "
    senha <- getLine

    if (autenticaUsuario email senha (getListaEmailSenhaProfissional profissionais)) then
        menuProfissionalAutenticado dados (getProfissional profissionais email senha)
    else do
        putStrLn "E-mail ou senha incorretos"
        menuCadastro dados


cadastraServico:: Dados -> EmailP -> NomeP -> IO()
cadastraServico dados@(clientes,profissionais,servicos,atendimentos) emailP nomeP = do
    
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
    hPutStrLn arq (show (clientes,profissionais,((categoria,descricao,(read preco),emailP,nomeP):servicos),atendimentos))
    hClose arq

    putStr "Serviço cadastrado com sucesso!"

