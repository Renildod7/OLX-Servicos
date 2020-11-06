import System.IO
import Data.Char
import Dados
import Util
import Constantes
import Template

main = do 
    inicio

inicio:: IO()
inicio = do
    arq <- openFile arquivoDados ReadMode
    dados <- hGetLine arq
    hClose arq

    limpaTela
    exibeBemVindoOLX

    menu (read dados)


-- OK
menu:: Dados -> IO()
menu dados = do
    


    putStrLn "\nComo posso ajudar?\n"
    input <- getLine
    let palavras = [map toLower x | x <- (words input)]
    if ((procuraPalavras cadastrar palavras) && (procuraPalavras cliente palavras)) then cadastraCliente dados
    else if ((procuraPalavras cadastrar palavras) && (procuraPalavras profissional palavras)) then cadastraProfissional dados
    else if ((procuraPalavras login palavras) && (procuraPalavras cliente palavras)) then loginCliente dados
    else if ((procuraPalavras login palavras) && (procuraPalavras profissional palavras)) then loginProfissional dados
    else if (elem "ajuda" palavras) then ajuda dados
    else if (elem "sair" palavras) then sair
                                    
    else do
        putStrLn "\nNão entendi, poderia repetir?"
        putStrLn "Caso precise de ajuda digite Ajuda."
        pausa1s
        menu dados
--OK
ajuda:: Dados -> IO()
ajuda dados = do

    limpaTela
    exibeAjudaPrincipal
    menu dados


sair:: IO()
sair = do
    limpaTela
    msgSair
    pausa5s
    limpaTela


--OK
voltaMenuPrincipal:: Dados -> IO()
voltaMenuPrincipal dados = do
    pausa1s
    limpaTela
    exibeBemVindoOLX
    menu dados





































--OK
menuClienteAutenticado::Dados -> Cliente -> IO()
menuClienteAutenticado dados@(clientes,_,_,_) cliente@(_,_,nome,_,_) = do

    putStrLn "\nComo posso ajudar?\n"

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]
    if ((procuraPalavras listar palavras) && (procuraPalavras servico palavras)) then listarServicosCliente dados cliente
    else if ((procuraPalavras contratar palavras) && (procuraPalavras servico palavras)) then contratarServico dados cliente
    else if ((procuraPalavras concluir palavras) && (procuraPalavras atendimento palavras)) then concluirAtendimento dados cliente        
    else if (elem "ajuda" palavras) then ajudaCliente dados cliente
    else if (elem "sair" palavras) then voltaMenuPrincipal dados
    else do
        putStrLn "\nNão entendi, poderia repetir?"
        putStrLn "Caso precise de ajuda digite Ajuda."
        pausa1s
        menuClienteAutenticado dados cliente


--OK
ajudaCliente:: Dados -> Cliente -> IO()
ajudaCliente dados cliente = do
   
    limpaTela
    exibeAjudaCliente
    menuClienteAutenticado dados cliente

--OK
cadastraCliente:: Dados ->IO()
cadastraCliente dados@(clientes,profissionais,servicos,atendimentos) = do

    limpaTela
    exibeInformeDadosCadastro


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

        pausa1s
        limpaTela
        exibeClienteJaCadastrado
        voltaMenuPrincipal dados


    else do
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (((email,senha,nome,endereco,telefone):clientes),profissionais,servicos,atendimentos))
        hClose arq

        pausa1s
        limpaTela
        exibeCadastroRealizado
        pausa2s
        limpaTela
        exibeBemVindoMenuCliente
        menuClienteAutenticado (((email,senha,nome,endereco,telefone):clientes),profissionais,servicos,atendimentos) (getCliente ((email,senha,nome,endereco,telefone):clientes) email senha)

--OK
loginCliente:: Dados -> IO()
loginCliente dados@(clientes,_,_,_) = do
   
    putStrLn "\nE-mail: "
    email <- getLine

    putStrLn "\nSenha: "
    senha <- getLine

    if (autenticaUsuario email senha (getListaEmailSenhaCliente clientes)) then do
        pausa1s
        limpaTela
        exibeClienteLogado
        pausa2s
        limpaTela
        exibeBemVindoMenuCliente
        menuClienteAutenticado dados (getCliente clientes email senha)
    else do
        pausa1s
        limpaTela
        exibeEmailSenhaIncorretos
        pausa2s
        limpaTela
        exibeBemVindoOLX
        menu dados














































--OK
menuProfissionalAutenticado:: Dados -> Profissional-> IO()
menuProfissionalAutenticado dados@(_,profissionais,servicos,_) profissional@(emailP,_,nomeP,_,_) = do

    putStrLn "\nComo posso ajudar?\n"

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]

    if ((procuraPalavras cadastrar palavras) && (procuraPalavras servico palavras)) then cadastraServico dados profissional
    else if ((procuraPalavras listar palavras) && (procuraPalavras atendimento palavras) && (procuraPalavras pendentes palavras)) then atendimentosPendentes dados profissional
    else if (procuraPalavras faturamento palavras) then faturamentoServico dados profissional
    else if (elem "ajuda" palavras) then ajudaProfissional dados profissional
    else if (elem "sair" palavras) then voltaMenuPrincipal dados
    else do
        putStrLn "\nNão entendi, poderia repetir?"
        putStrLn "Caso precise de ajuda digite Ajuda."
        pausa1s
        menuProfissionalAutenticado dados profissional

--OK
ajudaProfissional:: Dados -> Profissional -> IO()
ajudaProfissional dados profissional = do
    
    limpaTela
    exibeAjudaProfissional
    menuProfissionalAutenticado dados profissional

--OK
cadastraProfissional:: Dados ->IO()
cadastraProfissional dados@(clientes, profissionais,servicos,atendimentos) = do

    limpaTela
    exibeInformeDadosCadastro

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

    if (verificaCadastro email (getListaEmailSenhaProfissional profissionais)) then do 
        pausa1s
        limpaTela
        exibeProfissionalJaCadastrado
        voltaMenuPrincipal dados

    else do
        arq <- openFile arquivoDados WriteMode
        hPutStrLn arq (show (clientes,((email,senha,nome,endereco,telefone):profissionais),servicos,atendimentos))
        hClose arq

        pausa1s
        limpaTela
        exibeCadastroRealizado
        pausa2s
        limpaTela
        exibeBemVindoMenuProfissional
        menuProfissionalAutenticado (clientes,((email,senha,nome,endereco,telefone):profissionais),servicos,atendimentos) (getProfissional profissionais email senha)



--OK
loginProfissional:: Dados -> IO()
loginProfissional dados@(_,profissionais,_,_) = do
    putStrLn "\nE-mail: "
    email <- getLine

    putStrLn "\nSenha: "
    senha <- getLine

    if (autenticaUsuario email senha (getListaEmailSenhaProfissional profissionais)) then do
        pausa1s
        limpaTela
        exibeProfissionalLogado
        pausa2s
        limpaTela
        exibeBemVindoMenuProfissional
        menuProfissionalAutenticado dados (getProfissional profissionais email senha)
    else do
        pausa1s
        limpaTela
        exibeEmailSenhaIncorretos
        pausa2s
        limpaTela
        exibeBemVindoOLX
        menu dados

































































--OK MAIS OU MENOS
cadastraServico:: Dados -> Profissional -> IO()
cadastraServico dados@(clientes,profissionais,servicos,atendimentos) profissional@(emailP,_,nomeP,_,_) = do
    
    limpaTela
    exibeInformeDadosCadastro

    putStrLn "\nDescrição: "
    descricao <- getLine

    putStrLn "\nPreço: "
    preco <- getLine

    if (validaFloat preco) then do 


        putStrLn "\nEscolha uma dentre as seguintes categorias\n"
        pausa1s
        putStrLn (listarString categorias)        
        putStrLn "\nCategoria: "
        categoria <- getLine
        
        if (elem categoria categorias) then do
            arq <- openFile arquivoDados WriteMode
            hPutStrLn arq (show (clientes,profissionais,((categoria,descricao,(read preco),emailP,nomeP,[]):servicos),atendimentos))
            hClose arq

            pausa1s
            limpaTela
            exibeCadastroRealizado
            pausa2s
            limpaTela
            exibeBemVindoMenuProfissional
            menuProfissionalAutenticado (clientes,profissionais,((categoria,descricao,(read preco),emailP,nomeP,[]):servicos),atendimentos) profissional
        else do
            putStrLn "\nCategoria informada inválida"
            pausa3s
            cadastraServico dados profissional
    else do
        putStrLn "\nPreço informado inválido"
        pausa3s
        cadastraServico dados profissional



--OK
contratarServico:: Dados -> Cliente -> IO()
contratarServico dados@(_,_,servico,_) cliente = do 

    if (servico == []) then do
        pausa1s
        limpaTela
        exibeNaoExistemServicos
        pausa2s
        limpaTela
        exibeBemVindoMenuCliente
        menuClienteAutenticado dados cliente

    else do
        putStrLn "\nGostaria de escolher uma categoria de serviços ou listar todos"
        putStrLn "os serviços disponiveis?\n"

        input <- getLine
        let palavras = [map toLower x | x <- (words input)]
        if ((procuraPalavras listar palavras) && (procuraPalavras todos palavras)) then contratarServicoListagem dados cliente
        else if (procuraPalavras categoria palavras) then contratarServicoCategoria dados cliente
        else if (procuraPalavras voltar palavras) then do
            pausa1s
            limpaTela
            exibeBemVindoMenuCliente
            menuClienteAutenticado dados cliente
        else do
            putStrLn "\nNão entendi, poderia repetir?"
            putStrLn "\nCaso queira sair digite voltar"
            pausa1s
            contratarServico dados cliente


--OK
contratarServicoListagem:: Dados -> Cliente -> IO()
contratarServicoListagem dados@(clientes,profissionais,servicos,(atPendentes,atAceitos,atRecusados,atConcluidos)) cliente@(emailC,_,nomeC,_,_) = do

    let servicosOrd = quickSortServicoCategoria servicos

    if servicosOrd == [] then do 
        limpaTela
        exibeNaoPossuiServico

    else do
        limpaTela
        exibeTodosServicosDisponiveis
        putStrLn ""
        putStrLn (listarServicos servicosOrd)
        putStrLn "\nCaso queira contratar algum deles informe o número do serviço correspondente."
        putStrLn "Caso não queira contratar nenhum deles digite 0.\n"

        input <- getLine

        if ((validaInt input) ) then
            if ((read input) == 0 ) then do
                pausa1s
                limpaTela
                exibeBemVindoMenuCliente
                menuClienteAutenticado dados cliente
            else if (((read input) > 0) && ((read  input) <= (length servicosOrd))) then do 
                
                let servico = head (drop ((read input)-1) servicosOrd)
                
                arq <- openFile arquivoDados WriteMode
                hPutStrLn arq (show (clientes,profissionais,servicos,(((emailC,nomeC,servico):atPendentes),atAceitos,atRecusados,atConcluidos)))
                hClose arq

                pausa1s
                limpaTela
                exibeSolicitacaoConcluida
                pausa2s
                limpaTela
                exibeBemVindoMenuCliente
                menuClienteAutenticado (clientes,profissionais,servicos,(((emailC,nomeC,servico):atPendentes),atAceitos,atRecusados,atConcluidos)) cliente
            else do
                putStrLn "\nOpção inválida"
                pausa2s
                contratarServicoListagem dados cliente
        else do
            putStrLn "\nOpção inválida"
            pausa2s
            contratarServicoListagem dados cliente

-- OK
contratarServicoCategoria:: Dados -> Cliente -> IO()
contratarServicoCategoria dados@(clientes,profissionais,servicos,(atPendentes,atAceitos,atRecusados,atConcluidos)) cliente@(emailC,_,nomeC,_,_) = do

    putStrLn "\nInforme a categoria desejada entre as seguintes:\n"

    pausa1s
    putStrLn (listarString categorias)
    putStrLn ""

    categ <- getLine

    if (elem (map toLower categ) voltar) then contratarServico dados cliente

    else if (elem categ categorias) then do

            let servicosCategoria = quickSortServicoAvaliacao (getServicosCategoria servicos categ)

            if (servicosCategoria == []) then do 

                pausa1s
                limpaTela
                exibeNaoPossuiServicosAvaliadosCategoria
                pausa2s
                limpaTela
                exibeBemVindoMenuCliente
                menuClienteAutenticado dados cliente

            else do 

                pausa1s
                limpaTela
                exibeServicosAvaliadosCategoria
                pausa1s
                putStrLn (listarServicos (take 2 servicosCategoria))

                putStrLn "Gostaria de contratar algum desses serviços, ou gostaria de listar todos"
                putStrLn "desta categoria?\n"

                input <- getLine
                let palavras = [map toLower x | x <- (words input)]

                if (procuraPalavras contratar palavras) then do
                    putStrLn "\nInforme o número do serviço que gostaria de contratar\n"
                    putStrLn "Caso não queira contratar nenhum deles digite 0\n"
                    
                    num  <- getLine 

                    if ((validaInt num)) then
                        if ((read num) == 0) then contratarServico dados cliente
                        else if (((read num) > 0) && ((read  num) <= (length servicosCategoria))) then do 
                            
                            let servico = head (drop ((read num)-1) servicosCategoria)
                            
                            arq <- openFile arquivoDados WriteMode
                            hPutStrLn arq (show (clientes,profissionais,servicos,(((emailC,nomeC,servico):atPendentes),atAceitos,atRecusados,atConcluidos)))
                            hClose arq


                            pausa1s
                            limpaTela
                            exibeSolicitacaoConcluida
                            pausa2s
                            limpaTela
                            exibeBemVindoMenuCliente
                            menuClienteAutenticado (clientes,profissionais,servicos,(((emailC,nomeC,servico):atPendentes),atAceitos,atRecusados,atConcluidos)) cliente
                        else do
                            putStrLn "\nOpção inválida"
                            pausa2s
                            contratarServico dados cliente
                    else do
                        putStrLn "\nOpção inválida"
                        pausa2s
                        contratarServico dados cliente                            

                else if ((procuraPalavras listar palavras) && (procuraPalavras todos palavras)) then do
                   
                    pausa1s
                    limpaTela
                    exibeTodosServicosCategoria
                    putStrLn (listarServicos  servicosCategoria)

                    putStrLn "Informe o número do serviço que gostaria de contratar\n"
                    putStrLn "Caso não queira contratar nenhum deles digite 0\n"
                   
                    num  <- getLine 
                    if ((validaInt num)) then
                        if ((read num) == 0) then contratarServico dados cliente
                        else if (((read num) > 0) && ((read  num) <= (length servicosCategoria))) then do 

                            let servico = head (drop ((read num)-1) servicosCategoria)
                            
                            arq <- openFile arquivoDados WriteMode
                            hPutStrLn arq (show (clientes,profissionais,servicos,(((emailC,nomeC,servico):atPendentes),atAceitos,atRecusados,atConcluidos)))
                            hClose arq

                            pausa1s
                            limpaTela
                            exibeSolicitacaoConcluida
                            pausa2s
                            limpaTela
                            exibeBemVindoMenuCliente
                            menuClienteAutenticado (clientes,profissionais,servicos,(((emailC,nomeC,servico):atPendentes),atAceitos,atRecusados,atConcluidos)) cliente
                        
                        else do
                            putStrLn "\nOpção inválida"
                            pausa2s
                            contratarServico dados cliente
                    else do
                        putStrLn "\nOpção inválida"
                        pausa2s
                        contratarServico dados cliente 

                else do
                    putStrLn "\nOpção inválida"
                    pausa1s
                    contratarServicoCategoria dados cliente
    else do 
        putStrLn "\nOpção inválida"
        pausa1s
        contratarServicoCategoria dados cliente



--OK
listarServicosCliente:: Dados -> Cliente -> IO()
listarServicosCliente dados@(_,_,_,(atPendentes,atAceitos,atRecusados,atConcluidos)) cliente@(emailC,_,_,_,_) = do


    putStrLn "\nQual tipo de atendimento você gostaria de ver?"
    putStrLn "\nPendentes\nAceitos\nRecusados\nConcluidos\n"

    input <- getLine
    let palavras = [map toLower x | x <- (words input)]
    
    if (elem "pendentes" palavras) then do
        let pendentes = getAtNConcluidos atPendentes emailC
        if (pendentes == []) then do
            
            pausa1s
            putStrLn "\nVocê não possui atendimentos pendentes"
            pausa2s
            listarServicosCliente dados cliente
        else do
            pausa1s
            limpaTela
            exibeTodosAtPendentes
            pausa1s
            putStrLn (listarAtAceitos pendentes)
            pausa1s
--            limpaTela
            listarServicosCliente dados cliente



    else if (elem "aceitos" palavras) then do
        let aceitos = getAtNConcluidos atAceitos emailC
        if (aceitos == []) then do
            
            pausa1s
            putStrLn "\nVocê não possui atendimentos aceitos"
            pausa2s
            listarServicosCliente dados cliente
        else do
            pausa1s
            limpaTela
            exibeTodosAtAceitos
            pausa1s
            putStrLn (listarAtAceitos aceitos)
            pausa1s
            menuClienteAutenticado dados cliente

    else if (elem "recusados" palavras) then do
        let recusados = getAtNConcluidos atRecusados emailC
        if (recusados == []) then do
            
            pausa1s
            putStrLn "\nVocê não possui atendimentos recusados"
            pausa2s
            listarServicosCliente dados cliente
        else do
            pausa1s
            limpaTela
            exibeTodosAtAceitos
            pausa1s
            putStrLn (listarAtAceitos recusados)
            pausa1s
            menuClienteAutenticado dados cliente


    else if (elem "concluidos" palavras) then do
        let concluidos = getAtConcluidos atConcluidos emailC
        if (concluidos == []) then do
            
            pausa1s
            putStrLn "\nVocê não possui atendimentos concluidos"
            pausa2s
            listarServicosCliente dados cliente
        else do
            pausa1s
            limpaTela
            exibeTodosAtAceitos
            pausa1s
            putStrLn (listarAtConcluidos concluidos)
            pausa1s
            menuClienteAutenticado dados cliente
    else if (procuraPalavras voltar palavras) then do
        pausa1s
        limpaTela
        exibeBemVindoMenuCliente
        menuClienteAutenticado dados cliente
    else do
        putStrLn "\nOpção inválida"
        putStrLn "Caso queria voltar digite voltar"
        pausa1s
        
        listarServicosCliente dados cliente



--OK
faturamentoServico:: Dados -> Profissional -> IO()
faturamentoServico dados@(_,_,servicos,(_,_,_,atConcluidos)) profissional@(emailP,_,_,_,_) = do

    let servicosP = getServicosProfissional servicos emailP
    let atConcluidosP = getAtConcluidosProfissional atConcluidos emailP
    pausa1s
    putStrLn (getFaturamentoProfissional servicosP atConcluidosP)

    menuProfissionalAutenticado dados profissional











































atendimentosPendentes:: Dados -> Profissional -> IO()
atendimentosPendentes dados@(clientes,profissionais,servicos,(atPendentes,atAceitos,atRecusados,atConcluidos)) profissional@(emailP,_,_,_,_) = do
    
    let atendimentoPendentes = getAtPendentesProfissional atPendentes emailP

    if (atendimentoPendentes == []) then do 
        pausa1s
        putStrLn "\nNão existem atendimentos pendentes"
        pausa1s
        menuProfissionalAutenticado dados profissional

    else do

        limpaTela
        exibeTodosAtPendentesP
        pausa1s
        putStrLn (listarAtNaoConcluidos atendimentoPendentes)

        putStrLn "Para aceitar ou recusar um atendimento, informe a ação desejada\n" 

        input <- getLine
        let palavras = [map toLower x | x <- (words input)]

        if (elem "aceitar" palavras) then do
            putStrLn "\nIndique o número do atendimento que deseja aceitar\n"
            num <- getLine
    

            if ((validaInt num)) then
                if (((read num) > 0) && ((read  num) <= (length atendimentoPendentes))) then do 

                    let atendimento = head (drop ((read num)-1) (getAtPendentesProfissional atPendentes emailP))
                    let novoAtPendentes = removeAtendimentoNC atendimento atPendentes

                    arq <- openFile arquivoDados WriteMode
                    hPutStrLn arq (show (clientes,profissionais,servicos,(novoAtPendentes,(atendimento:atAceitos),atRecusados,atConcluidos)))
                    hClose arq

                    pausa1s
                    limpaTela
                    exibeAtendimentoAceito
                    pausa2s
                    limpaTela
                    exibeBemVindoMenuProfissional
                    menuProfissionalAutenticado (clientes,profissionais,servicos,(novoAtPendentes,(atendimento:atAceitos),atRecusados,atConcluidos)) profissional
        
                else do
                    putStrLn "\nOpção inválida"
                    pausa2s
                    atendimentosPendentes dados profissional
            else do
                putStrLn "\nOpção inválida"
                pausa2s
                atendimentosPendentes dados profissional 


        else if (elem "recusar" palavras) then do
            

            putStrLn "\nIndique o número do atendimento que deseja aceitar\n"
            num <- getLine

            if ((validaInt num)) then
                if (((read num) > 0) && ((read  num) <= (length atendimentoPendentes))) then do 


                    let atendimento = head (drop ((read num)-1) (getAtPendentesProfissional atPendentes emailP))
                    let novoAtPendentes = removeAtendimentoNC atendimento atPendentes

                    arq <- openFile arquivoDados WriteMode
                    hPutStrLn arq (show (clientes,profissionais,servicos,(novoAtPendentes,atAceitos,(atendimento:atRecusados),atConcluidos)))
                    hClose arq

                    pausa1s
                    limpaTela
                    exibeAtendimentoRecusado
                    pausa2s
                    limpaTela
                    exibeBemVindoMenuProfissional
                    menuProfissionalAutenticado (clientes,profissionais,servicos,(novoAtPendentes,atAceitos,(atendimento:atRecusados),atConcluidos)) profissional
                else do
                    putStrLn "\nOpção inválida"
                    pausa2s
                    atendimentosPendentes dados profissional
            else do
                putStrLn "\nOpção inválida"
                pausa2s
                atendimentosPendentes dados profissional


        else if (elem "voltar" palavras) then do
            pausa1s
            limpaTela
            exibeBemVindoMenuProfissional
            menuProfissionalAutenticado dados profissional
        else do
            putStrLn "\nNão entendi, poderia repetir?"
            putStrLn "Caso queira voltar digite voltar"
            pausa2s
            atendimentosPendentes dados profissional



concluirAtendimento:: Dados -> Cliente -> IO()
concluirAtendimento dados@(clientes,profissionais,servicos,(atPendentes,atAceitos,atRecusados,atConcluidos)) cliente@(emailC,_,_,_,_) = do

    let atAceiosCliente = getAtNConcluidos atAceitos emailC

    if (atAceiosCliente == []) then do
        pausa1s
        limpaTela
        exibeNaoExistemAtAceitos
        pausa2s
        limpaTela
        exibeBemVindoMenuCliente
        menuClienteAutenticado dados cliente

    else do

        limpaTela
        exibeTodosAtPendentesP
        pausa1s
        putStrLn (listarAtAceitos atAceiosCliente)


        if(atAceiosCliente == []) then do
            putStrLn "\nVocê não possui atendimentos para concluir"
            pausa1s
            limpaTela
            exibeBemVindoMenuCliente
            menuClienteAutenticado dados cliente
        else do

            putStrLn "\nPara finalizar um atendimento, indique seu número\n" 

            input <- getLine

            putStrLn "\nAvalie seu atendimento com um numero de 0 a 10\n"

            avaliacao <- getLine

            if (validaFloat avaliacao) then do 
                let atendimento@
                        (emailC,nomeC,servico@
                                     (categoria,descricao,preco,emailP,nomeP,avaliacoes)) = head (drop ((read input)-1) atAceiosCliente)
                let novoAtAceitos = removeAtendimentoNC atendimento atAceitos
                let novoServicos = removeServico servico servicos
                

                arq <- openFile arquivoDados WriteMode
                hPutStrLn arq (show (clientes,profissionais,((categoria,descricao,preco,emailP,nomeP,((read avaliacao):avaliacoes)):novoServicos),(atPendentes,novoAtAceitos,atRecusados,((emailC,nomeC,servico,(read avaliacao)):atConcluidos))))
                hClose arq

                pausa1s
                limpaTela
                exibeAtendimentoConcluido
                pausa2s
                limpaTela
                exibeBemVindoMenuCliente
                menuClienteAutenticado (clientes,profissionais,((categoria,descricao,preco,emailP,nomeP,((read avaliacao):avaliacoes)):novoServicos),(atPendentes,novoAtAceitos,atRecusados,((emailC,nomeC,servico,(read avaliacao)):atConcluidos))) cliente

            else do
                putStrLn "\nAvaliação inválida"
                pausa1s
                limpaTela
                exibeBemVindoMenuCliente
                menuClienteAutenticado dados cliente

