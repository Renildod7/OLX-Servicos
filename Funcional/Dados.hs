module Dados (
    Dados(..),
    Clientes(..),
    Profissionais(..),
    Profissional(..),
    Servicos(..),
    Nome(..),
    Email(..),
    getClientes,
    getProfissionais,
    getServicos,
    getListaEmailSenhaCliente,
    getListaEmailSenhaProfissional,
    getProfissional,
    arquivoDados,
    especialidades
) where

arquivoDados = "dados.txt"
especialidades = ["Arte","Informática","Segurança"]

type Dados = (Clientes,Profissionais,Servicos)

type Clientes = [Cliente]
type Cliente = (Email,Senha,Nome,Endereco,Telefone)
type Profissionais = [Profissional]
type Profissional = (Email,Senha,Nome,Endereco,Telefone)
type Email = String
type Senha = String
type Nome = String
type Endereco = String
type Telefone = String

type Servicos = [Servico]
type Servico = (Categoria, Descricao, Preco, Email)
type Categoria = String
type Preco = Float
type Descricao = String
type Avaliacao = Float


getClientes:: Dados -> Clientes
getClientes (clientes,_,_) = clientes

getProfissionais:: Dados -> Profissionais
getProfissionais (_,profissionais,_) = profissionais

getServicos:: Dados -> Servicos
getServicos (_,_,servicos) = servicos

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

getProfissional:: Profissionais -> Email -> Senha -> Profissional
getProfissional [] _ _ = ("","","","","")
getProfissional (x:xs) email senha = if getEmailSenhaProfissional x == (email,senha) then x
                                     else getProfissional xs email senha



