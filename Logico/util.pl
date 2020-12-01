:- module(util,[
    limpaTela/0,
    pausa1/0,
    pausa2/0,
    pausa5/0,
    getNomeCliente/2,
    lerEntrada/1,
    lerEntradaNum/1,
    lerCategoria/1,
    splitEspaco/2,
    geraCliente/6,
    geraProfissional/6,
    geraServico/6,
    geraAtendimentoPendente/4,
    listaCategorias/1,
    getTodosServicos/1,
    listarServicos/2,
    getCategorias/1,
    verificaPreco/1,
    getServicosCategoria/2,
    getDoisMelhorAvaliados/2,
    getAtPendentesProfissional/2,
    listarAtPendentesProfissional/2,
    geraRetractAtPendente/2,
    geraAtAceito/2,
    geraAtRecusado/2,
    getAtAceitosCliente/2,
    listarAtAceitosCliente/2
    ]).

limpaTela:-
    tty_clear.

pausa1 :-
    sleep(1).

pausa2 :-
    sleep(2).

pausa5 :-
    sleep(5).

getNomeCliente(EmailC,NomeC):-
    cliente(EmailC,_,NomeC,_,_).

listaCategorias([]).
listaCategorias([H|T]):- writeln(H), listaCategorias(T).

lerCategoria(Entrada):-
    read_line_to_codes(user_input,Entradaascii),
    string_to_atom(Entradaascii,Entrada).


lerEntrada(Entrada):-
    read_line_to_codes(user_input,Entradaascii),
    string_to_atom(Entradaascii,EntradaString),
    string_lower(EntradaString,Entrada).

lerEntradaNum(EntradaNum):-
    read_line_to_codes(user_input,Entradaascii),
    string_to_atom(Entradaascii,EntradaString),
    atom_number(EntradaString,EntradaNum).

verificaPreco(Preco):- integer(Preco);float(Preco).

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
    main:servico(Categoria,Descricao,Preco,EmailP,NomeP),
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
                        listarServicosAux2(T,String,N,R,StringServico);

                        getAtributosServico(H,Categoria,Descricao,Preco,EmailP,NomeP),
                        geraStringServico(N,Categoria,Descricao,0.0,Preco,NomeP,EmailP,StringServico),
                        listarServicosAux2(T,String,N,R,StringServico).

listarServicosAux2(T,String,N,R,StringServico):-
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
    formataDescricao(Descricao,NovaDescricao),
    format(atom(String),
        '\nNúmero: ~w\nCategoria: ~w\nDescrição: ~w\nAvaliação: ~1f ⭐\nPreço: R$ ~w\nProfissional: ~w\nEmail: ~w\n',
        [N,Categoria,NovaDescricao,MediaAvaliacoes,Preco,NomeP,EmailP]).

formataDescricao(Descricao,R):-
    formataDescricaoAux(Descricao,"",R).
    

formataDescricaoAux([],String,R):- R = String.
formataDescricaoAux([H|T],String,R):-
    string_concat(String,H,NovaString),
    string_concat(NovaString," ",NovaString2),
    formataDescricaoAux(T,NovaString2,R).


geraAtendimentoPendente(EmailC,NomeC,Lista,NovoServico):- geraAtendimentoPendenteAux(EmailC,NomeC,Lista,1,NovoServico).

geraAtendimentoPendenteAux(EmailC,NomeC,Lista,N,NovoServico):-
    getAtributosServico(Lista,Categoria,Descricao,Preco,EmailP,NomeP),
    format(atom(Servico),
        'atPendente(~w,~w,~w,~w,~w,~w,~w,~w).',
        [EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,N]),
    \+ main:atPendente(EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,N),
    NovoServico = Servico;
    N2 is N +1,
    geraAtendimentoPendenteAux(EmailC,NomeC,Lista,N2,NovoServico).


getCategorias(R):- getCategoriasAux([],R),!.

getCategoriasAux(Lista,R):-
    main:categoria(Categoria),

    \+ member(Categoria,Lista),
    adiciona(Categoria,Lista,Lista2),
    getCategoriasAux(Lista2,R);
    R = Lista.





getServicosCategoria(Categoria,R):- getServicosCategoriaAux(Categoria,[],R),!.

getServicosCategoriaAux(Categoria,Lista,R):-
    L = [],
    main:servico(Categoria,Descricao,Preco,EmailP,NomeP),
    adiciona(NomeP,L,L2),
    adiciona(EmailP,L2,L3),
    adiciona(Preco,L3,L4),
    adiciona(Descricao,L4,L5),
    adiciona(Categoria,L5,L6),

    \+ member(L6,Lista),
    adiciona(L6,Lista,Lista2),
    getServicosCategoriaAux(Categoria,Lista2,R);
    R = Lista.


getDoisMelhorAvaliados(Lista,R):- getDoisMelhorAvaliadosAux(Lista,[],R).

getDoisMelhorAvaliadosAux([H,X|_],L,R):-
    adiciona(H,L,L2),
    adiciona(X,L2,L3),
    R = L3.
getDoisMelhorAvaliadosAux([H|_],L,R):-
    adiciona(H,L,L2),
    R = L2.



getAtPendentesProfissional(Email,R):- getAtPendentesProfissionalAux(Email,[],R),!.

getAtPendentesProfissionalAux(Email,Lista,R):-
    L = [],
    main:atPendente(EmailC,NomeC,Categoria,Descricao,Preco,Email,Nome,N),
    adiciona(N,L,L2),
    adiciona(Nome,L2,L3),
    adiciona(Email,L3,L4),
    adiciona(Preco,L4,L5),
    adiciona(Descricao,L5,L6),
    adiciona(Categoria,L6,L7),
    adiciona(NomeC,L7,L8),
    adiciona(EmailC,L8,L9),

    \+ member(L9,Lista),
    adiciona(L9,Lista,Lista2),
    getAtPendentesProfissionalAux(Email,Lista2,R);
    R = Lista.





listarAtPendentesProfissional(Lista,R):- listarAtPendentesProfissionalAux(Lista,"",1,R).

listarAtPendentesProfissionalAux([],String,_,R):- R = String,!.
listarAtPendentesProfissionalAux([H|T],String,N,R):-
    getAtributosAtPendente(H,EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,_),
    getAvaliacoes(Categoria,Descricao,Preco,EmailP,NomeP,AvaliacoesL),
    somaAvaliacoes(AvaliacoesL,SomaAvali),
    length(AvaliacoesL,QtdAvali),
    QtdAvali =\= 0 -> 
                        MediaAvaliacoes is SomaAvali/QtdAvali,
                        geraStringAtPendente(N,EmailC,NomeC,Categoria,Descricao,MediaAvaliacoes,Preco,StringAtPendente),
                        listarAtPendentesProfissionalAux2(T,String,N,R,StringAtPendente);

                        getAtributosAtPendente(H,EmailC,NomeC,Categoria,Descricao,Preco,_,_,_),
                        geraStringAtPendente(N,EmailC,NomeC,Categoria,Descricao,0.0,Preco,StringAtPendente),
                        listarAtPendentesProfissionalAux2(T,String,N,R,StringAtPendente).

listarAtPendentesProfissionalAux2(T,String,N,R,StringAtPendente):-
    string_concat(String,StringAtPendente,NovaString),
    N2 is N+1,
    listarAtPendentesProfissionalAux(T,NovaString,N2,R).




getAtributosAtPendente(Lista,EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,Numero):-
    nth0(0,Lista,EmailC),
    nth0(1,Lista,NomeC),
    nth0(2,Lista,Categoria),
    nth0(3,Lista,Descricao),
    nth0(4,Lista,Preco),
    nth0(5,Lista,EmailP),
    nth0(6,Lista,NomeP),
    nth0(7,Lista,Numero).


geraStringAtPendente(N,EmailC,NomeC,Categoria,Descricao,MediaAvaliacoes,Preco,String):-
    formataDescricao(Descricao,NovaDescricao),
    format(atom(String),
        '\nNúmero: ~w\nCategoria: ~w\nDescrição: ~w\nAvaliação: ~1f ⭐\nPreço: R$ ~w\nCliente: ~w\nEmail: ~w\n',
        [N,Categoria,NovaDescricao,MediaAvaliacoes,Preco,NomeC,EmailC]).



geraRetractAtPendente(AtPend,RetractAtPend):-
    getAtributosAtPendente(AtPend,EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,Numero),
    format(atom(RetractAtPend),
        '?- retract(atPendente(~w,~w,~w,~w,~w,~w,~w,~w)).',
        [EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,Numero]).




geraAtAceito(AtPend,AtAceito):- geraAtAceitoAux(AtPend,1,AtAceito).

geraAtAceitoAux(AtPend,N,AtAceito):-
    getAtributosAtPendente(AtPend,EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,_),
    format(atom(AtA),
        'atAceito(~w,~w,~w,~w,~w,~w,~w,~w).',
        [EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,N]),
    \+ main:atAceito(EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,N),
    AtAceito = AtA;
    N2 is N +1,
    geraAtAceitoAux(AtPend,N2,AtAceito).



geraAtRecusado(AtPend,AtRecusado):- geraAtRecusadoAux(AtPend,1,AtRecusado).

geraAtRecusadoAux(AtPend,N,AtRecusado):-
    getAtributosAtPendente(AtPend,EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,_),
    format(atom(AtR),
        'atRecusado(~w,~w,~w,~w,~w,~w,~w,~w).',
        [EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,N]),
    \+ main:atRecusado(EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,N),
    AtRecusado = AtR;
    N2 is N +1,
    geraAtRecusadoAux(AtPend,N2,AtRecusado).





getAtAceitosCliente(EmailC,R):- getAtAceitosClienteAux(EmailC,[],R),!.

getAtAceitosClienteAux(EmailC,Lista,R):-
    L = [],
    main:atAceito(EmailC,NomeC,Categoria,Descricao,Preco,Email,Nome,N),
    adiciona(N,L,L2),
    adiciona(Nome,L2,L3),
    adiciona(Email,L3,L4),
    adiciona(Preco,L4,L5),
    adiciona(Descricao,L5,L6),
    adiciona(Categoria,L6,L7),
    adiciona(NomeC,L7,L8),
    adiciona(EmailC,L8,L9),

    \+ member(L9,Lista),
    adiciona(L9,Lista,Lista2),
    getAtAceitosClienteAux(Email,Lista2,R);
    R = Lista.




listarAtAceitosCliente(Lista,R):- listarAtAceitosClienteAux(Lista,"",1,R).

listarAtAceitosClienteAux([],String,_,R):- R = String,!.
listarAtAceitosClienteAux([H|T],String,N,R):-
    getAtributosAtAceito(H,_,_,Categoria,Descricao,Preco,EmailP,NomeP,_),
    getAvaliacoes(Categoria,Descricao,Preco,EmailP,NomeP,AvaliacoesL),
    somaAvaliacoes(AvaliacoesL,SomaAvali),
    length(AvaliacoesL,QtdAvali),
    QtdAvali =\= 0 -> 
                        MediaAvaliacoes is SomaAvali/QtdAvali,
                        geraStringAtAceito(N,EmailP,NomeP,Categoria,Descricao,MediaAvaliacoes,Preco,StringAtAceito),
                        listarAtAceitosClienteAux2(T,String,N,R,StringAtAceito);

                        getAtributosAtAceito(H,_,_,Categoria,Descricao,Preco,EmailP,NomeP,_),
                        geraStringAtAceito(N,EmailP,NomeP,Categoria,Descricao,0.0,Preco,StringAtAceito),
                        listarAtAceitosClienteAux2(T,String,N,R,StringAtAceito).

listarAtAceitosClienteAux2(T,String,N,R,StringAtAceito):-
    string_concat(String,StringAtAceito,NovaString),
    N2 is N+1,
    listarAtAceitosClienteAux(T,NovaString,N2,R).




geraStringAtAceito(N,EmailP,NomeP,Categoria,Descricao,MediaAvaliacoes,Preco,String):-
    formataDescricao(Descricao,NovaDescricao),
    format(atom(String),
        '\nNúmero: ~w\nCategoria: ~w\nDescrição: ~w\nAvaliação: ~1f ⭐\nPreço: R$ ~w\nProfissional: ~w\nEmail: ~w\n',
        [N,Categoria,NovaDescricao,MediaAvaliacoes,Preco,NomeP,EmailP]).


getAtributosAtAceito(Lista,EmailC,NomeC,Categoria,Descricao,Preco,EmailP,NomeP,Numero):-
    nth0(0,Lista,EmailC),
    nth0(1,Lista,NomeC),
    nth0(2,Lista,Categoria),
    nth0(3,Lista,Descricao),
    nth0(4,Lista,Preco),
    nth0(5,Lista,EmailP),
    nth0(6,Lista,NomeP),
    nth0(7,Lista,Numero).
