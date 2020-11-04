module Dados (
    Dados(..),
    Clientes(..),
    Cliente(..),
    Profissionais(..),
    Profissional(..),
    Servicos(..),
    NomeC(..),
    NomeP(..),
    EmailC(..),
    EmailP(..),
    getListaEmailSenhaCliente,
    getListaEmailSenhaProfissional,
    getProfissional,
    getCliente,
    listarServicos,
    getCategoria,
    getServicosProfissional,
    getAtPendentesProfissional,
    listarAtNaoConcluidos,
    removeAtendimentoNC,
    removeServico,
    listarAtAceitos,
    mediaAvaliacaoServico,
    formataMediaAvaliacao,
    getServicosCategoria,
    getAtNConcluidos,
    getAtConcluidos,
    listarAtConcluidos,
    getAtConcluidosProfissional,
    qtdConclusoesServico,
    getFaturamentoProfissional,
    arquivoDados,
    especialidades
) where

import Data.Char

arquivoDados = "dados.txt"
especialidades = ["Arte","Informática","Segurança"]

type Dados = (Clientes,Profissionais,Servicos,Atendimentos)

type Clientes = [Cliente]
type Cliente = (EmailC,Senha,NomeC,Endereco,Telefone)
type EmailC = String
type NomeC = String

type Profissionais = [Profissional]
type Profissional = (EmailP,Senha,NomeP,Endereco,Telefone)
type EmailP = String
type NomeP = String

type Senha = String
type Endereco = String
type Telefone = String

type Servicos = [Servico]
type Servico = (Categoria, Descricao, Preco, EmailP, NomeP, Avaliacoes)
type Categoria = String
type Preco = Float
type Descricao = String
type Avaliacoes = [Avaliacao]
type Avaliacao = Float

type Atendimentos = (AtPendentes,AtAceitos,AtRecusados,AtConcluidos) 

type AtPendentes = [AtPendente]
type AtAceitos = [AtAceito]
type AtRecusados = [AtRecusado]
type AtConcluidos = [AtConcluido]

type AtPendente = (EmailC,NomeC,Servico)
type AtAceito = (EmailC,NomeC,Servico)
type AtRecusado = (EmailC,NomeC,Servico)
type AtConcluido = (EmailC,NomeC,Servico,Avaliacao)





getEmailSenhaCliente:: Cliente -> (String,String)
getEmailSenhaCliente (email,senha,_,_,_) = (email,senha)

getListaEmailSenhaCliente:: Clientes -> [(String,String)]
getListaEmailSenhaCliente [] = []
getListaEmailSenhaCliente (x:xs) = [(getEmailSenhaCliente x)] ++ getListaEmailSenhaCliente xs


getEmailSenhaProfissional:: Profissional -> (String,String)
getEmailSenhaProfissional (email,senha,_,_,_) = (email,senha)

getListaEmailSenhaProfissional:: Profissionais -> [(String,String)]
getListaEmailSenhaProfissional [] = []
getListaEmailSenhaProfissional (x:xs) = [(getEmailSenhaProfissional x)] ++ getListaEmailSenhaProfissional xs

getProfissional:: Profissionais -> EmailP -> Senha -> Profissional
getProfissional [] _ _ = ("","","","","")
getProfissional (x:xs) email senha = if getEmailSenhaProfissional x == (email,senha) then x
                                     else getProfissional xs email senha

getCliente:: Clientes -> EmailC -> Senha -> Cliente
getCliente [] _ _ = ("","","","","")
getCliente (x:xs) email senha = if getEmailSenhaCliente x == (email,senha) then x
                                     else getCliente xs email senha




listarServicos:: Servicos -> String
listarServicos servicos = listarServicosAux servicos 1

listarServicosAux:: Servicos -> Int -> String
listarServicosAux [] _ = ""
listarServicosAux ((categoria,descricao,preco,emailP,nomeP,avaliacoes):[]) n = "\nNúmero: " ++ (show n) ++
                                                                  "\nCategoria: " ++ categoria ++ 
                                                                  "\nDescrição: " ++ descricao ++ 
                                                                  "\nAvaliação: " ++ formataMediaAvaliacao (mediaAvaliacoes avaliacoes) ++ " ⭐" ++
                                                                  "\nPreço: R$ " ++ (show preco) ++ 
                                                                  "\nProfissional: " ++ nomeP ++ 
                                                                  "\nEmail: " ++ emailP ++ "\n"
listarServicosAux ((categoria,descricao,preco,emailP,nomeP,avaliacoes):xs) n = "\nNúmero: " ++ (show n) ++
                                                                  "\nCategoria: " ++ categoria ++ 
                                                                  "\nDescrição: " ++ descricao ++ 
                                                                  "\nAvaliação: " ++ formataMediaAvaliacao (mediaAvaliacoes avaliacoes) ++ " ⭐" ++
                                                                  "\nPreço: R$ " ++ (show preco) ++ 
                                                                  "\nProfissional: " ++ nomeP ++ 
                                                                  "\nEmail para contato: " ++ emailP ++
                                                                  "\n" ++ listarServicosAux xs (n+1)


-- Juntar com o sort

getCategoria:: Servico -> Categoria
getCategoria (categoria,_,_,_,_,_) = categoria


getEmailPServico:: Servico -> EmailP
getEmailPServico (_,_,_,emailP,_,_) = emailP





getServicosProfissional:: Servicos -> EmailP -> Servicos
getServicosProfissional [] _ = []
getServicosProfissional _ "" = []
getServicosProfissional servicos emailP = [x | x <- servicos, (getEmailPServico x) == emailP]



getAtPendentesProfissional:: AtPendentes -> EmailP -> AtPendentes
getAtPendentesProfissional [] _ = []
getAtPendentesProfissional _ "" = []
getAtPendentesProfissional atPendentes@((_,_,s):t) emailP = [x | x <- atPendentes, (getEmailPServico s) == emailP]



listarAtNaoConcluidos:: AtPendentes -> String
listarAtNaoConcluidos atPendentes = listarAtNaoConcluidosAux atPendentes 1

listarAtNaoConcluidosAux:: AtPendentes -> Int -> String
listarAtNaoConcluidosAux [] _ = ""
listarAtNaoConcluidosAux ((emailC,nomeC,(categoria,descricao,preco,emailP,nomeP,avaliacoes)):[]) n = "\nNúmero: " ++ (show n) ++
                                                                                                     "\nCategoria: " ++ categoria ++ 
                                                                                                     "\nDescrição: " ++ descricao ++ 
                                                                                                     "\nAvaliação: " ++ formataMediaAvaliacao (mediaAvaliacoes avaliacoes) ++ " ⭐" ++
                                                                                                     "\nPreço: R$ " ++ (show preco) ++ 
                                                                                                     "\nCliente: " ++ nomeC ++ 
                                                                                                     "\nEmail: " ++ emailC ++ "\n"
listarAtNaoConcluidosAux ((emailC,nomeC,(categoria,descricao,preco,emailP,nomeP,avaliacoes)):xs) n = "\nNúmero: " ++ (show n) ++
                                                                                                     "\nCategoria: " ++ categoria ++ 
                                                                                                     "\nDescrição: " ++ descricao ++ 
                                                                                                     "\nAvaliação: " ++ formataMediaAvaliacao (mediaAvaliacoes avaliacoes) ++ " ⭐" ++
                                                                                                     "\nPreço: R$ " ++ (show preco) ++ 
                                                                                                     "\nCliente: " ++ nomeC ++ 
                                                                                                     "\nEmail: " ++ emailC ++
                                                                                                     "\n" ++ listarAtNaoConcluidosAux xs (n+1)



removeAtendimentoNC:: AtPendente->AtPendentes->AtPendentes
removeAtendimentoNC at [] = []
removeAtendimentoNC at (x:xs)
        | at==x = xs
        | otherwise = x:(removeAtendimentoNC at xs)


{-
getAtAceitosCliente:: AtAceitos -> EmailC -> AtAceitos
getAtAceitosCliente [] _ = []
getAtAceitosCliente _ "" = []
getAtAceitosCliente atAceitos@(a@(eC,_,_):[]) emailC = if (eC == emailC) then [a]
                                                        else []
getAtAceitosCliente atAceitos@(a@(eC,_,_):t) emailC = if (eC == emailC) then [a] ++ getAtAceitosCliente t emailC
                                                        else [] ++ getAtAceitosCliente t emailC

-}





removeServico:: Servico->Servicos->Servicos
removeServico s [] = []
removeServico s (x:xs)
        | s==x = xs
        | otherwise = x:(removeServico s xs)





listarAtAceitos:: AtPendentes -> String
listarAtAceitos atPendentes = listarAtAceitosAux atPendentes 1

listarAtAceitosAux:: AtPendentes -> Int -> String
listarAtAceitosAux [] _ = ""
listarAtAceitosAux ((emailC,nomeC,(categoria,descricao,preco,emailP,nomeP,avaliacoes)):[]) n = "\nNúmero: " ++ (show n) ++
                                                                                       "\nCategoria: " ++ categoria ++ 
                                                                                      "\nDescrição: " ++ descricao ++
                                                                                      "\nAvaliação: " ++ formataMediaAvaliacao (mediaAvaliacoes avaliacoes) ++ " ⭐" ++
                                                                                        "\nPreço: R$ " ++ (show preco) ++ 
                                                                                        "\nProfissional: " ++ nomeP ++ 
                                                                                        "\nEmail: " ++ emailP ++ "\n"
listarAtAceitosAux ((emailC,nomeC,(categoria,descricao,preco,emailP,nomeP,avaliacoes)):xs) n = "\nNúmero: " ++ (show n) ++
                                                                                        "\nCategoria: " ++ categoria ++ 
                                                                                        "\nDescrição: " ++ descricao ++
                                                                                        "\nAvaliação: " ++ formataMediaAvaliacao (mediaAvaliacoes avaliacoes) ++ " ⭐" ++  
                                                                                        "\nPreço: R$ " ++ (show preco) ++ 
                                                                                        "\nProfissional: " ++ nomeP ++ 
                                                                                        "\nEmail: " ++ emailP ++
                                                                                        "\n" ++ listarAtAceitosAux xs (n+1)





mediaAvaliacaoServico:: Servico -> Float
mediaAvaliacaoServico (_,_,_,_,_,[]) = 0.0
mediaAvaliacaoServico (_,_,_,_,_,avaliacoes) = ((sum avaliacoes) / (realToFrac (length avaliacoes)))


mediaAvaliacoes:: Avaliacoes -> Avaliacao
mediaAvaliacoes [] = 0.0
mediaAvaliacoes avaliacoes = ((sum avaliacoes) / (realToFrac (length avaliacoes)))

formataMediaAvaliacao:: Float -> String
formataMediaAvaliacao media = if (media == 10) then show media
                              else take 3 (show media)



getServicosCategoria:: Servicos -> Categoria -> Servicos
getServicosCategoria servicos categoria = [x | x <- servicos, (map toLower (getCategoria x)) == (map toLower categoria)]



getAtNConcluidos:: [(EmailC,NomeC,Servico)] -> EmailC -> [(EmailC,NomeC,Servico)]
getAtNConcluidos atendimentos@(x:xs) emailC = [x | x <- atendimentos, (getEmailCAtNConcluido x) == emailC]

getEmailCAtNConcluido:: (EmailC,NomeC,Servico) -> EmailC
getEmailCAtNConcluido (emailC,_,_) = emailC




getAtConcluidos:: AtConcluidos -> EmailC -> AtConcluidos
getAtConcluidos atendimentos@(x:xs) emailC = [x | x <- atendimentos, (getEmailCAtConcluido x) == emailC]

getEmailCAtConcluido:: (EmailC,NomeC,Servico,Avaliacao) -> EmailC
getEmailCAtConcluido (emailC,_,_,_) = emailC




listarAtConcluidos:: AtConcluidos -> String
listarAtConcluidos atPendentes = listarAtConcluidosAux atPendentes 1

listarAtConcluidosAux:: AtConcluidos -> Int -> String
listarAtConcluidosAux [] _ = ""
listarAtConcluidosAux ((emailC,nomeC,(categoria,descricao,preco,emailP,nomeP,_),avaliacao):[]) n = "\nNúmero: " ++ (show n) ++
                                                                                                            "\nCategoria: " ++ categoria ++ 
                                                                                                            "\nDescrição: " ++ descricao ++
                                                                                                            "\nPreço: R$ " ++ (show preco) ++ 
                                                                                                            "\nProfissional: " ++ nomeP ++ 
                                                                                                            "\nEmail: " ++ emailP ++
                                                                                                            "\nAvaliação Atendimento: " ++ (show avaliacao) ++ " ⭐"
listarAtConcluidosAux ((emailC,nomeC,(categoria,descricao,preco,emailP,nomeP,_),avaliacao):xs) n = "\nNúmero: " ++ (show n) ++
                                                                                                            "\nCategoria: " ++ categoria ++ 
                                                                                                            "\nDescrição: " ++ descricao ++
                                                                                                            "\nPreço: R$ " ++ (show preco) ++ 
                                                                                                            "\nProfissional: " ++ nomeP ++ 
                                                                                                            "\nEmail: " ++ emailP ++
                                                                                                            "\nAvaliação Atendimento: " ++ (show avaliacao) ++ " ⭐" ++
                                                                                                              listarAtConcluidosAux xs (n+1)


getAtConcluidosProfissional:: AtConcluidos -> EmailP -> AtConcluidos
getAtConcluidosProfissional atendimentos@(x:xs) emailP = [x | x <- atendimentos, (getEmailPAtConcluido x) == emailP]

getEmailPAtConcluido:: (EmailC,NomeC,Servico,Avaliacao) -> EmailC
getEmailPAtConcluido (_,_,(_,_,_,emailP,_,_),_) = emailP



getFaturamentoProfissional:: Servicos -> AtConcluidos -> String
getFaturamentoProfissional servicos atPendentes = getFaturamentoProfissionalAux servicos atPendentes 1

getFaturamentoProfissionalAux:: Servicos -> AtConcluidos -> Int -> String
getFaturamentoProfissionalAux (servico@(categoria,descricao,preco,emailP,nomeP,_):[]) atConcluidos n = "\nNúmero: " ++ (show n)  ++
                                                                                                     "\nCategoria: " ++ categoria ++
                                                                                                     "\nDescricao: " ++ descricao ++
                                                                                                     "\nPreço: R$ " ++ (show preco) ++
                                                                                                     "\nFaturamento: R$ " ++ (show ((realToFrac (qtdConclusoesServico servico atConcluidos)) * preco))



getFaturamentoProfissionalAux (servico@(categoria,descricao,preco,emailP,nomeP,_):xs) atConcluidos n = "\nNúmero: " ++ (show n) ++
                                                                                                      "\nCategoria: " ++ categoria ++
                                                                                                    "\nDescricao: " ++ descricao ++
                                                                                                    "\nPreço: R$ " ++ (show preco) ++
                                                                                                    "\nFaturamento: R$ " ++ (show ((realToFrac (qtdConclusoesServico servico atConcluidos)) * preco)) ++
                                                                                                    "\n" ++ getFaturamentoProfissionalAux xs atConcluidos (n+1)


qtdConclusoesServico:: Servico -> AtConcluidos -> Int
qtdConclusoesServico servico atConcluidos = length [x | x <- atConcluidos, comparaServico servico (getServicoAtConcluido x)]


comparaServico:: Servico -> Servico -> Bool
comparaServico (categoria1,descricao1,preco1,emailP1,nomeP1,_) (categoria2,descricao2,preco2,emailP2,nomeP2,_) = 
                                                        ((categoria1 == categoria2) &&
                                                         (descricao1 == descricao2) &&
                                                         (preco1 == preco2) &&
                                                         (emailP1 == emailP2) &&
                                                         (nomeP1 == nomeP2))

getServicoAtConcluido:: AtConcluido -> Servico
getServicoAtConcluido (_,_,s,_) = s