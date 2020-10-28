module Dados (
    Dados(..),
    Clientes(..),
    Profissionais(..),
    getClientes,
    getProfissionais,
    arquivoDados
) where

type Dados = (Clientes,Profissionais)
type Clientes = [Cliente]
type Cliente = (Email,Senha)
type Profissionais = [Profissional]
type Profissional = (Email,Senha)
type Email = String
type Senha = String

getClientes:: Dados -> Clientes
getClientes (clientes,_) = clientes

getProfissionais:: Dados -> Profissionais
getProfissionais (_,profissionais) = profissionais

arquivoDados = "dados.txt"
