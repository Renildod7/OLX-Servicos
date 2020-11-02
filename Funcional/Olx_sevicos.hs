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
    limpaTela
    putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
    putStrLn "⎪                          Bem vindo a OLX Servicos                            ⎪"
    putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"

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

    limpaTela
    putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
    putStrLn "⎪         Informe a ação que quer realizar seguido do tipo de usuario.         ⎪"
    putStrLn "⎪      É possivel se cadastrar e logar, como cliente ou como profissional.     ⎪"
    putStr   "⎪          Por exemplo: "
    putStr (show "Eu gostaria de me cadastrar como um cliente") 
    putStrLn "          ⎪"
    putStrLn "⎪                                                                              ⎪"
    putStrLn "⎪      Para contratar ou oferecer um serviço é necessário ter um cadastro.     ⎪"
    putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"

    menuCadastro dados














































menuClienteAutenticado::Dados -> Cliente -> IO()
menuClienteAutenticado dados@(clientes,_,_,_) cliente@(_,_,nome,_,_) = do


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


cadastraCliente:: Dados ->IO()
cadastraCliente dados@(clientes,profissionais,servicos,atendimentos) = do

    limpaTela
    putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
    putStrLn "⎪           Informe os seguintes dados para para realizar o cadastro           ⎪"
    putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"


    putStrLn "\nNome: "
    nome <- getLine

    putStrLn "\nE-mail: "
    email <- getLine

    putStrLn "\nSenha: "
    senha <- getLine

    putStrLn "\nEndereco: "
    endereco <- getLine

    putStrLn "\nTelefone: "
    telefone <- getLine

    if (verificaCadastro email (getListaEmailSenhaCliente clientes) ) then do 
            putStrLn "\n"
            putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
            putStrLn "⎪                            Cliente já cadastrado                             ⎪"
            putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"
            menuCadastro dados


    else do
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (((email,senha,nome,endereco,telefone):clientes),profissionais,servicos,atendimentos))
        hClose arq
        putStrLn "Cadastro realizado"
        menuClienteAutenticado (((email,senha,nome,endereco,telefone):clientes),profissionais,servicos,atendimentos) (getCliente clientes email senha)

loginCliente:: Dados -> IO()
loginCliente dados@(clientes,_,_,_) = do
   
    putStrLn "\nE-mail: "
    email <- getLine

    putStrLn "\nSenha: "
    senha <- getLine

    if (autenticaUsuario email senha (getListaEmailSenhaCliente clientes)) then do
        limpaTela
        putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
        putStrLn "⎪                          Cliente logado com sucesso                          ⎪"
        putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"
        menuClienteAutenticado dados (getCliente clientes email senha)
    else do
        putStrLn "E-mail ou senha incorretos"
        menuCadastro dados















































menuProfissionalAutenticado:: Dados -> Profissional-> IO()
menuProfissionalAutenticado dados@(_,profissionais,servicos,_) profissional@(emailP,_,nomeP,_,_) = do
    putStr "Bem Vindo "
    putStr nomeP
    putStrLn " !"
    putStrLn "Como posso ajuda-lo?\n"

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]

    if ((elem "cadastrar" palavras) && (elem "serviço" palavras)) then cadastraServico dados profissional
    else if ((elem "listar" palavras) && (elem "atendimentos" palavras) && (elem "pendentes" palavras)) then atendimentosPendentes dados profissional
    else do
        putStrLn "Não entendi, poderia repetir?"
        menuProfissionalAutenticado dados profissional

cadastraProfissional:: Dados ->IO()
cadastraProfissional dados@(clientes, profissionais,servicos,atendimentos) = do

    limpaTela
    putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
    putStrLn "⎪           Informe os seguintes dados para para realizar o cadastro           ⎪"
    putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"

    putStrLn "\nE-mail: "
    email <- getLine

    putStrLn "\nSenha: "
    senha <- getLine

    putStrLn "\nNome: "
    nome <- getLine

    putStrLn "\nEndereco: "
    endereco <- getLine

    putStrLn "\nTelefone: "
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
    putStrLn "\nE-mail: "
    email <- getLine

    putStrLn "\nSenha: "
    senha <- getLine

    if (autenticaUsuario email senha (getListaEmailSenhaProfissional profissionais)) then do
        limpaTela
        putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
        putStrLn "⎪                        Profissional logado com sucesso                       ⎪"
        putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"
        menuProfissionalAutenticado dados (getProfissional profissionais email senha)
    else do
        putStrLn "E-mail ou senha incorretos"
        menuCadastro dados


































































cadastraServico:: Dados -> Profissional -> IO()
cadastraServico dados@(clientes,profissionais,servicos,atendimentos) profissional@(emailP,_,nomeP,_,_) = do
    
    limpaTela
    putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
    putStrLn "⎪           Informe os seguintes dados para para realizar o cadastro           ⎪"
    putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"

    putStrLn "\nDescrição: "
    descricao <- getLine

    putStrLn "\nPreço: "
    preco <- getLine

    putStrLn "\nEscolha uma dentre as seguintes categorias"
    putStrLn "<pressione Enter para seguir>"
    getLine
    putStrLn (listar especialidades)
    putStrLn ""
    
    putStrLn "\nCategoria: "
    categoria <- getLine
    
    arq <- openFile arquivoDados WriteMode
    hPutStrLn arq (show (clientes,profissionais,((categoria,descricao,(read preco),emailP,nomeP):servicos),atendimentos))
    hClose arq

    putStrLn "Serviço cadastrado com sucesso!"
    menuProfissionalAutenticado (clientes,profissionais,((categoria,descricao,(read preco),emailP,nomeP):servicos),atendimentos) profissional


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
contratarServicoListagem dados@(clientes,profissionais,servicos,(atPendentes,atAceitos,atRecusados,atConcluidos)) cliente@(emailC,_,nomeC,_,_) = do

    let servicosOrd = quickSortServicoCategoria servicos

    if servicosOrd == [] then putStrLn "Desculpe, não possuimos nenhum serviço disponivel no momento."
    else do
        putStrLn "Estes são todos os serviços que temos disponiveis no momento."
        putStrLn (listarServicos servicosOrd)
        putStrLn "Caso queira contratar algum deles informe o número do serviço correspondente."
        putStrLn "Caso não queira constratar nenhum deles digite sair"

        input <- getLine
        let servico = head (drop ((read input)-1) servicosOrd)
        
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (clientes,profissionais,servicos,(((emailC,nomeC,servico):atPendentes),atAceitos,atRecusados,atConcluidos)))
        hClose arq
        putStrLn "Solicitação concluida"
        menuClienteAutenticado (clientes,profissionais,servicos,(((emailC,nomeC,servico):atPendentes),atAceitos,atRecusados,atConcluidos)) cliente

        


atendimentosPendentes:: Dados -> Profissional -> IO()
atendimentosPendentes dados@(clientes,profissionais,servicos,(atPendentes,atAceitos,atRecusados,atConcluidos)) profissional@(emailP,_,_,_,_) = do
 
    limpaTela
    putStrLn "⎧⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎺⎫"
    putStrLn "⎪             Estes são todos os atendimentos pendentes no momento             ⎪" 
    putStrLn "⎩⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎭"
    putStrLn (listarAtPendentes (getAtPendentesProfissional atPendentes emailP))

    putStrLn "Para aceitar ou recusar um atendimento, informe a ação desejada." 

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]

    if (elem "aceitar" palavras) then do
        putStrLn "Indique o número do atendimento que deseja aceitar"
        input <- getLine
        
        let atendimento = head (drop ((read input)-1) (getAtPendentesProfissional atPendentes emailP))
        let novoAtPendentes = removeAtPendente atendimento atPendentes

        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (clientes,profissionais,servicos,(novoAtPendentes,(atendimento:atAceitos),atRecusados,atConcluidos)))
        hClose arq

        atendimentosPendentes (clientes,profissionais,servicos,(novoAtPendentes,(atendimento:atAceitos),atRecusados,atConcluidos)) profissional
    
    else if (elem "recusar" palavras) then do
        

        putStrLn "Indique o número do atendimento que deseja aceitar"
        input <- getLine
        
        let atendimento = head (drop ((read input)-1) (getAtPendentesProfissional atPendentes emailP))
        let novoAtPendentes = removeAtPendente atendimento atPendentes

        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (clientes,profissionais,servicos,(novoAtPendentes,atAceitos,(atendimento:atRecusados),atConcluidos)))
        hClose arq

        atendimentosPendentes (clientes,profissionais,servicos,(novoAtPendentes,atAceitos,(atendimento:atRecusados),atConcluidos)) profissional
    


    else if (elem "voltar" palavras) then do
        limpaTela
        menuProfissionalAutenticado dados profissional
    else do
        putStrLn "Não entendi, poderia repetir?"
        menuProfissionalAutenticado dados profissional