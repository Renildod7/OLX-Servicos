:- module(util,[
    limpaTela/0,
    pausa1/0,
    pausa2/0,
    pausa5/0,
    lerEntrada/1,
    splitEspaco/2,
    geraCliente/6,
    geraProfissional/6,
    geraServico/6,
    listaCategorias/1,
    getTodosServicos/1,
    listarServicos/2
    ]).

limpaTela:-
    tty_clear.

pausa1 :-
    sleep(1).

pausa2 :-
    sleep(2).

pausa5 :-
    sleep(5).

listaCategorias([]).
listaCategorias([H|T]):- writeln(H), listaCategorias(T).

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


geraServico(Categoria,Descricao,Preco,EmailP,NomeP,NovoServico):-
    string_concat("servico(",Categoria,X1),
    string_concat(X1,",[",X2),
    string_concat(X2,Descricao,X3),
    string_concat(X3,"],",X4),
    string_concat(X4,Preco,X5),
    string_concat(X5,",",X6),
    string_concat(X6,EmailP,X7),
    string_concat(X7,",",X8),
    string_concat(X8,NomeP,X9),
    string_concat(X9,").",NovoServico).


somaAvaliacoes([],0).
somaAvaliacoes([[H|_]|T],R):- somaAvaliacoes(T,G), R is H+G.

adiciona(X,Y,[X|Y]).

getTodosServicos(R):-getTodosServicosAux([],R),!.

getTodosServicosAux(Lista,R):-
    L = [],
    servico(Categoria,Descricao,Preco,EmailP,NomeP),
    adiciona(NomeP,L,L2),
    adiciona(EmailP,L2,L3),
    adiciona(Preco,L3,L4),
    adiciona(Descricao,L4,L5),
    adiciona(Categoria,L5,L6),

    \+ member(L6,Lista),
    adiciona(L6,Lista,Lista2),
    getTodosServicosAux(Lista2,R);
    R = Lista.

getAvaliacoes(Categoria,Descricao,Preco,EmailP,NomeP,R):-
    getAvaliacoesAux(Categoria,Descricao,Preco,EmailP,NomeP,[],R),!.

getAvaliacoesAux(Categoria,Descricao,Preco,EmailP,NomeP,Lista,R):-
    L = [],
    main:avaliacao(Categoria,Descricao,Preco,EmailP,NomeP,Avaliacao,Num),
    adiciona(Num,L,L2),
    adiciona(Avaliacao,L2,L3),
    \+ member(L3,Lista),
    adiciona(L3,Lista,Lista2),
    getAvaliacoesAux(Categoria,Descricao,Preco,EmailP,NomeP,Lista2,R);
    R = Lista.

listarServicos(ListaServicos,R):- listarServicosAux(ListaServicos,"",1,R).

listarServicosAux([],String,_,R):- R = String,!.
listarServicosAux([H|T],String,N,R):-
    getAtributosServico(H,Categoria,Descricao,Preco,EmailP,NomeP),
    getAvaliacoes(Categoria,Descricao,Preco,EmailP,NomeP,AvaliacoesL),
    somaAvaliacoes(AvaliacoesL,SomaAvali),
    length(AvaliacoesL,QtdAvali),
    QtdAvali =\= 0 -> 
                        MediaAvaliacoes is SomaAvali/QtdAvali,
                        geraStringServico(N,Categoria,Descricao,MediaAvaliacoes,Preco,NomeP,EmailP,StringServico),
                        string_concat(String,StringServico,NovaString),
                        N2 is N+1,
                        listarServicosAux(T,NovaString,N2,R);

                        getAtributosServico(H,Categoria,Descricao,Preco,EmailP,NomeP),
                        geraStringServico(N,Categoria,Descricao,0.0,Preco,NomeP,EmailP,StringServico),
                        string_concat(String,StringServico,NovaString),
                        N2 is N+1,
                        listarServicosAux(T,NovaString,N2,R).

getAtributosServico(Lista,Categoria,Descricao,Preco,EmailP,NomeP):-
    nth0(0,Lista,Categoria),
    nth0(1,Lista,Descricao),
    nth0(2,Lista,Preco),
    nth0(3,Lista,EmailP),
    nth0(4,Lista,NomeP).

geraStringServico(N,Categoria,Descricao,MediaAvaliacoes,Preco,NomeP,EmailP,String):-
    format(atom(String),
        '\nNúmero: ~w\nCategoria: ~w\nDescrição: ~w\nAvaliação: ~1f ⭐\nPreço: R$ ~w\nProfissional: ~w\nEmail: ~w\n',
        [N,Categoria,Descricao,MediaAvaliacoes,Preco,NomeP,EmailP]).