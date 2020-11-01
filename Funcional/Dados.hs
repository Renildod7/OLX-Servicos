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
    listarTodosServicos,
    getCategoria,
    arquivoDados,
    especialidades
) where

import Data.Ord

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

type Atendimentos = (AtPendentes,AtConfirmados,AtRecusados,AtConcluidos) 

type AtPendentes = [AtPendente]
type AtConfirmados = [AtConfirmado]
type AtRecusados = [AtRecusado]
type AtConcluidos = [AtConcluido]

type AtPendente = (EmailC,Servico)
type AtConfirmado = (EmailC,Servico)
type AtRecusado = (EmailC,Servico)
type AtConcluido = (EmailC,Servico,Avaliacao)





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




listarTodosServicos:: Servicos -> String
listarTodosServicos servicos = listarTodosServicosAux servicos 1

listarTodosServicosAux:: Servicos -> Int -> String
listarTodosServicosAux [] _ = ""
listarTodosServicosAux ((categoria,descricao,preco,email,nome):[]) n = "\nNúmero: " ++ (show n) ++
                                                                  "\nCategoria: " ++ categoria ++ 
                                                                  "\nDescrição: " ++ descricao ++ 
                                                                  "\nA partir de: R$ " ++ (show preco) ++ 
                                                                  "\nProfissional: " ++ nome ++ 
                                                                  "\nEmail para contato: " ++ email ++ "\n"
listarTodosServicosAux ((categoria,descricao,preco,email,nome):xs) n = "\nNúmero: " ++ (show n) ++
                                                                  "\nCategoria: " ++ categoria ++ 
                                                                  "\nDescrição: " ++ descricao ++ 
                                                                  "\nA partir de: R$ " ++ (show preco) ++ 
                                                                  "\nProfissional: " ++ nome ++ 
                                                                  "\nEmail para contato: " ++ email ++
                                                                  "\n" ++ listarTodosServicosAux xs (n+1)
getCategoria:: Servico -> String
getCategoria (categoria,_,_,_,_) = categoria





{-
remove:: (Ord a) => a->[a]->[a]
remove a [] = []
remove a (x:xs)
| a==x = xs
| otherwise = x:(remove a xs)
-}