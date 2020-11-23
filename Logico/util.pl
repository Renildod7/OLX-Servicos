:- module(util,[
    limpaTela/0,
    pausa1/0,
    pausa2/0,
    pausa5/0,
    lerEntrada/1,
    splitEspaco/2,
    geraCliente/6,
    geraProfissional/6
    ]).

limpaTela:-
    tty_clear.

pausa1 :-
    sleep(1).

pausa2 :-
    sleep(2).

pausa5 :-
    sleep(5).



lerEntrada(Entrada):-
    read_line_to_codes(user_input,Entradaascii),
    string_to_atom(Entradaascii,EntradaString),
    string_lower(EntradaString,Entrada).

splitEspaco(String,Lista):- 
    split_string(String," ","\s\t\n",Lista).

geraCliente(Nome,Email,Senha,Endereco,Telefone,NovoCliente):-
    string_concat("cliente(",Email,X1),
    string_concat(X1,",",X2),
    string_concat(X2,Senha,X3),
    string_concat(X3,",",X4),
    string_concat(X4,Nome,X5),
    string_concat(X5,",",X6),
    string_concat(X6,Endereco,X7),
    string_concat(X7,",",X8),
    string_concat(X8,Telefone,X9),
    string_concat(X9,").",NovoCliente).

geraProfissional(Nome,Email,Senha,Endereco,Telefone,NovoProfissonal):-
    string_concat("profissional(",Email,X1),
    string_concat(X1,",",X2),
    string_concat(X2,Senha,X3),
    string_concat(X3,",",X4),
    string_concat(X4,Nome,X5),
    string_concat(X5,",",X6),
    string_concat(X6,Endereco,X7),
    string_concat(X7,",",X8),
    string_concat(X8,Telefone,X9),
    string_concat(X9,").",NovoProfissonal).
