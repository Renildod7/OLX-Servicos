:- dynamic cliente/5.

cliente(email,senha,nome,endereço,telefone).
cliente(emial2,senha2,nome2,endereço2,telefone2).
cliente(nnnnn,nnnnn,nnnnn,nnnnn,nnnnn).


?- retract(cliente(emial2,senha2,nome2,endereço2,telefone2)).