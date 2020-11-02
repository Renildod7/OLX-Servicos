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
    listarAtPendentes,
    removeAtPendente,
    arquivoDados,
    especialidades
) where

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
type Servico = (Categoria, Descricao, Preco, EmailP, NomeP)
type Categoria = String
type Preco = Float
type Descricao = String
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
listarServicosAux ((categoria,descricao,preco,email,nome):[]) n = "\nNúmero: " ++ (show n) ++
                                                                  "\nCategoria: " ++ categoria ++ 
                                                                  "\nDescrição: " ++ descricao ++ 
                                                                  "\nA partir de: R$ " ++ (show preco) ++ 
                                                                  "\nProfissional: " ++ nome ++ 
                                                                  "\nEmail: " ++ email ++ "\n"
listarServicosAux ((categoria,descricao,preco,email,nome):xs) n = "\nNúmero: " ++ (show n) ++
                                                                  "\nCategoria: " ++ categoria ++ 
                                                                  "\nDescrição: " ++ descricao ++ 
                                                                  "\nA partir de: R$ " ++ (show preco) ++ 
                                                                  "\nProfissional: " ++ nome ++ 
                                                                  "\nEmail para contato: " ++ email ++
                                                                  "\n" ++ listarServicosAux xs (n+1)


-- Juntar com o sort

getCategoria:: Servico -> Categoria
getCategoria (categoria,_,_,_,_) = categoria


getEmailPServico:: Servico -> EmailP
getEmailPServico (_,_,_,emailP,_) = emailP





getServicosProfissional:: Servicos -> EmailP -> Servicos
getServicosProfissional [] _ = []
getServicosProfissional _ "" = []
getServicosProfissional servicos emailP = [x | x <- servicos, (getEmailPServico x) == emailP]



getAtPendentesProfissional:: AtPendentes -> EmailP -> AtPendentes
getAtPendentesProfissional [] _ = []
getAtPendentesProfissional _ "" = []
getAtPendentesProfissional atPendentes@((_,_,s):t) emailP = [x | x <- atPendentes, (getEmailPServico s) == emailP]



listarAtPendentes:: AtPendentes -> String
listarAtPendentes atPendentes = listarAtPendentesAux atPendentes 1

listarAtPendentesAux:: AtPendentes -> Int -> String
listarAtPendentesAux [] _ = ""
listarAtPendentesAux ((emailC,nomeC,(categoria,descricao,preco,email,nome)):[]) n = "\nNúmero: " ++ (show n) ++
                                                                              "\nCategoria: " ++ categoria ++ 
                                                                              "\nDescrição: " ++ descricao ++ 
                                                                              "\nA partir de: R$ " ++ (show preco) ++ 
                                                                              "\nCliente: " ++ nomeC ++ 
                                                                              "\nEmail: " ++ emailC ++ "\n"
listarAtPendentesAux ((emailC,nomeC,(categoria,descricao,preco,email,nome)):xs) n = "\nNúmero: " ++ (show n) ++
                                                                              "\nCategoria: " ++ categoria ++ 
                                                                              "\nDescrição: " ++ descricao ++ 
                                                                              "\nA partir de: R$ " ++ (show preco) ++ 
                                                                              "\nCliente: " ++ nomeC ++ 
                                                                              "\nEmail: " ++ emailC ++
                                                                              "\n" ++ listarAtPendentesAux xs (n+1)



removeAtPendente:: AtPendente->AtPendentes->AtPendentes
removeAtPendente at [] = []
removeAtPendente at (x:xs)
        | at==x = xs
        | otherwise = x:(removeAtPendente at xs)
