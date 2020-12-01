:- use_module('./template.pl').
:- use_module('./util.pl').

inicio:-
    carrega,
    limpaTela,
    exibeBemVindoOLX,
    menu.

carrega:-
    [clientes],
    [profissionais],
    [categorias],
    [servicos],
    [avaliacoes],
    [atPendentes],
    [atAceitos],
    [atRecusados].

menu:-
    nl,
    writeln('Como posso ajudar?'),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoMenuPrincpal(EntradaLista).

opcaoMenuPrincpal(Palavras):-
    (member("cadastrar",Palavras),member("cliente",Palavras)) -> cadastrarCliente,!;
    (member("login",Palavras),member("cliente",Palavras)) -> loginCliente,!;
    (member("cadastrar",Palavras),member("profissional",Palavras)) -> cadastrarProfissional,!;
    (member("login",Palavras),member("profissional",Palavras)) -> loginProfissional,!;
    (member("sair",Palavras)) -> sair,!;
    (member("ajuda",Palavras)) -> ajuda,!;
    writeln("\nNão entendi, poderia repetir?"),
    writeln("Caso precise de ajuda digite Ajuda."),
    pausa1,
    menu.

sair:-
    limpaTela,
    msgSair,
%    pausa5,
    limpaTela.

ajuda:-
    limpaTela,
    exibeAjudaPrincipal,
    menu.

voltaMenuPrincipal:-
    pausa1,
    limpaTela,
    exibeBemVindoOLX,
    menu.



cadastrarCliente:-
    limpaTela,
    exibeInformeDadosCadastro,
    nl,
    write("Nome: "),
    lerEntrada(Nome),nl,
    write("E-mail: "),
    lerEntrada(Email),nl,
    write("Senha: "),
    lerEntrada(Senha),nl,
    write("Endereço: "),
    lerEntrada(Endereco),nl,
    write("Telefone: "),
    lerEntrada(Telefone),
    string_to_atom(Email,EmailA),
    \+ cliente(EmailA,_,_,_,_) -> 
    geraCliente(Nome,Email,Senha,Endereco,Telefone,NovoCliente),
    append('clientes.pl'),
    writeln(NovoCliente),
    told,
    carrega,
    pausa1,
    limpaTela,
    exibeCadastroRealizado,
    pausa2,
    limpaTela,
    voltaMenuPrincipal,!;

    pausa1,
    limpaTela,
    exibeClienteJaCadastrado,
    pausa1,
    voltaMenuPrincipal.




loginCliente:-
    nl,
    write("E-mail: "),
    lerEntrada(Email),nl,
    string_to_atom(Email,EmailA),
    write("Senha: "),
    lerEntrada(Senha),nl,
    string_to_atom(Senha,SenhaA),

    (cliente(EmailA,SenhaA,_,_,_))  -> pausa1,
                                    limpaTela,
                                    exibeClienteLogado,
                                    pausa2,
                                    limpaTela,
                                    exibeBemVindoMenuCliente,
                                    menuClienteAutenticado(EmailA),!;

                                    pausa1,
                                    limpaTela,
                                    exibeEmailSenhaIncorretos,
                                    pausa2,
                                    limpaTela,
                                    exibeBemVindoOLX,
                                    menu.



menuClienteAutenticado(EmailC):-
    nl,
    writeln('Como posso ajudar?'),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoMenuCliente(EntradaLista,EmailC).

opcaoMenuCliente(Palavras,EmailC):-
    (member("contratar",Palavras),member("servico",Palavras)) -> contratarServico(EmailC),!;
    (member("concluir",Palavras),member("atendimento",Palavras)) -> concluirAtendimento(EmailC),!;
    (member("ajuda",Palavras)) -> ajudaCliente(EmailC),!;
    (member("sair",Palavras)) -> voltaMenuPrincipal,!;
    writeln("\nNão entendi, poderia repetir?"),
    writeln("Caso precise de ajuda digite Ajuda."),
    pausa1,
    menuClienteAutenticado(EmailC).


ajudaCliente(EmailC):-
    limpaTela,
    exibeAjudaCliente,
    menuClienteAutenticado(EmailC).

contratarServico(EmailC):-
    (getTodosServicos(TodosServicos),
    length(TodosServicos,QtdServicos),
    QtdServicos =:= 0) -> naoExistemServicosCadastrados(EmailC),!;
    nl,
    writeln("Gostaria de escolher uma categoria de serviços ou listar todos"),
    writeln("os serviços disponiveis?"),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoContratarServico(EntradaLista,EmailC).


naoExistemServicosCadastrados(EmailC):-
    pausa1,
    limpaTela,
    exibeNaoPossuiServico,
    pausa2,
    limpaTela,
    exibeBemVindoMenuCliente,
    menuClienteAutenticado(EmailC).


opcaoContratarServico(Palavras,EmailC):-
    (member("listar",Palavras),member("todos",Palavras)) -> contratarServicoListar(EmailC),!;
    (member("categoria",Palavras)) -> contratarServicoCategoria(EmailC),!;
    (member("voltar",Palavras)) -> voltarMenuCliente(EmailC),!;
    writeln("\nNão entendi, poderia repetir?"),
    writeln("Caso queira sair digite voltar"),
    pausa1,
    contratarServico(EmailC).

voltarMenuCliente(EmailC):-
    pausa1,
    limpaTela,
    exibeBemVindoMenuCliente,
    menuClienteAutenticado(EmailC).


contratarServicoListar(EmailC):-
    getTodosServicos(TodosServicos),
    listarServicos(TodosServicos,String),
    pausa1,
    limpaTela,
    exibeTodosServicosDisponiveis,
    pausa1,
    writeln(String),nl,
    writeln("Caso queira contratar algum deles informe o número do serviço correspondente."),
    writeln("Caso não queira contratar nenhum deles digite 0."),nl,
    write("-> "),
    lerEntradaNum(EntradaNum),
    length(TodosServicos, Tam),
    EntradaNum >= 0,
    EntradaNum =< Tam -> contratarServicoAux(EmailC,TodosServicos,EntradaNum),!;
                                nl,writeln("Opção inválida"),pausa2,contratarServicoListar(EmailC).



contratarServicoAux(EmailC,TodosServicos,Num):-
    Num =:= 0 -> pausa1,
                 limpaTela,
                 exibeBemVindoMenuCliente,
                 menuClienteAutenticado(EmailC),!;
 
                 I is Num - 1,
                 nth0(I,TodosServicos,ServicoL),
                 getNomeCliente(EmailC,NomeC),
                 geraAtendimentoPendente(EmailC,NomeC,ServicoL,NovoServico),
                 append('atPendentes.pl'),
                 writeln(NovoServico),
                 told,
                 carrega,
                 pausa1,
                 limpaTela,
                 exibeSolicitacaoConcluida,
                 pausa2,
                 limpaTela,
                 exibeBemVindoMenuCliente,
                 menuClienteAutenticado(EmailC).



contratarServicoCategoria(EmailC):-
    nl,
    writeln("Informe a categoria desejada entre as seguintes:"),nl,
    getCategorias(Categorias),
    pausa1,
    listaCategorias(Categorias),nl,
    write("-> "),
    lerEntrada(Categoria),
    string_to_atom(Categoria,CategoriaA),
    categoria(CategoriaA) -> contratarServicoCategoriaAux(EmailC,CategoriaA),!;
    nl,writeln("Categoria inválida"), pausa1,contratarServicoCategoria(EmailC).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Falta ordenar os servicos antes de mostrar os dois mais bem getDoisMelhorAvaliados %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
contratarServicoCategoriaAux(EmailC,Categoria):-
    getServicosCategoria(Categoria,Servicos),
    length(Servicos,Tam),
    Tam > 0,

    getDoisMelhorAvaliados(Servicos,DoisServicos),
    listarServicos(DoisServicos,String),
    pausa1,
    limpaTela,
    exibeServicosAvaliadosCategoria,
    pausa1,
    writeln(String),
    writeln("Gostaria de contratar algum desses serviços, ou gostaria de listar todos"),
    writeln("desta categoria?"),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista) ->
    opcaoContratarServicoCategoriaAux(EmailC,Categoria,Servicos,DoisServicos,EntradaLista),!;
    exibeNaoPossuiServicosAvaliadosCategoria(EmailC).

exibeNaoPossuiServicosAvaliadosCategoria(EmailC):-
    pausa1,
    limpaTela,
    exibeNaoPossuiServicosAvaliadosCategoria,
    pausa2,
    limpaTela,
    exibeBemVindoMenuCliente,
    menuClienteAutenticado(EmailC).


opcaoContratarServicoCategoriaAux(EmailC,Categoria,Servicos,DoisServicos,Palavras):-
    (member("listar",Palavras),member("todos",Palavras)) ->     pausa1,
                                                                limpaTela,
                                                                exibeTodosServicosCategoria,
                                                            contratarServicoCategoriaListarContratar(EmailC,Categoria,Servicos),!;
    (member("contratar",Palavras)) -> limpaTela,
                                      exibeServicosAvaliadosCategoria,
                                      contratarServicoCategoriaListarContratar(EmailC,Categoria,DoisServicos),!;

    (member("voltar",Palavras)) -> voltarMenuCliente(EmailC),!;
    writeln("\nNão entendi, poderia repetir?"),
    writeln("Caso queira sair digite voltar"),
    pausa1,
    contratarServicoCategoriaAux(EmailC,Categoria).





contratarServicoCategoriaListarContratar(EmailC,Categoria,Servicos):-
    listarServicos(Servicos,String),
    pausa1,
    writeln(String),nl,
    writeln("Caso queira contratar algum deles informe o número do serviço correspondente."),
    writeln("Caso não queira contratar nenhum deles digite 0."),nl,
    write("-> "),
    lerEntradaNum(EntradaNum),
    length(Servicos, Tam),
    EntradaNum >= 0,
    EntradaNum =< Tam -> contratarServicoAux(EmailC,Servicos,EntradaNum),!;
                                nl,writeln("Opção inválida"),pausa2,
                                contratarServicoCategoriaListarContratar(EmailC,Categoria,Servicos).



contratarServicoCategoriaListarContratarAux(EmailC,Servicos,Num):-
    Num =:= 0 -> pausa1,
                 limpaTela,
                 exibeBemVindoMenuCliente,
                 menuClienteAutenticado(EmailC),!;
 
                 I is Num - 1,
                 nth0(I,Servicos,ServicoL),
                 getNomeCliente(EmailC,NomeC),
                 geraAtendimentoPendente(EmailC,NomeC,ServicoL,NovoServico),
                 append('atPendentes.pl'),
                 writeln(NovoServico),
                 told,
                 carrega,
                 pausa1,
                 limpaTela,
                 exibeSolicitacaoConcluida,
                 pausa2,
                 limpaTela,
                 exibeBemVindoMenuCliente,
                 menuClienteAutenticado(EmailC).



concluirAtendimento(EmailC):-
    getAtAceitosCliente(EmailC,AtAceitos),
    length(AtAceitos,Tam),
    Tam > 0 -> limpaTela,
               exibeTodosAtPendentesP,
               pausa1,
               listarAtAceitosCliente(AtAceitos,StringAtAceitos),
               writeln(StringAtAceitos),!;
              % listarAtendimentosPendentesAux(EmailP,AtPendentes),!;

               pausa1,nl,
               writeln("Não existem atendimentos pendentes"),
               pausa1,
               menuClienteAutenticado(EmailC).







































cadastrarProfissional:-
    limpaTela,
    exibeInformeDadosCadastro,
    nl,
    write("Nome: "),
    lerEntrada(Nome),nl,
    write("E-mail: "),
    lerEntrada(Email),nl,
    write("Senha: "),
    lerEntrada(Senha),nl,
    write("Endereço: "),
    lerEntrada(Endereco),nl,
    write("Telefone: "),
    lerEntrada(Telefone),
    string_to_atom(Email,EmailA),
    \+ profissional(EmailA,_,_,_,_) -> 
    geraProfissional(Nome,Email,Senha,Endereco,Telefone,NovoProfissonal),
    append('profissionais.pl'),
    writeln(NovoProfissonal),
    told,
    carrega,
    pausa1,
    limpaTela,
    exibeCadastroRealizado,
    pausa2,
    limpaTela,
    voltaMenuPrincipal,!;

    pausa1,
    limpaTela,
    exibeProfissionalJaCadastrado,
    pausa1,
    voltaMenuPrincipal.


loginProfissional:-
    nl,
    write("E-mail: "),
    lerEntrada(Email),nl,
    string_to_atom(Email,EmailA),
    write("Senha: "),
    lerEntrada(Senha),nl,
    string_to_atom(Senha,SenhaA),

    (profissional(EmailA,SenhaA,_,_,_))  -> pausa1,
                                    limpaTela,
                                    exibeProfissionalLogado,
                                    pausa2,
                                    limpaTela,
                                    exibeBemVindoMenuProfissional,
                                    menuProfissionalAutenticado(EmailA),!;

                                    pausa1,
                                    limpaTela,
                                    exibeEmailSenhaIncorretos,
                                    pausa2,
                                    limpaTela,
                                    exibeBemVindoOLX,
                                    menu.



menuProfissionalAutenticado(EmailP):-
    nl,
    writeln('Como posso ajudar?'),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoMenuProfissional(EntradaLista,EmailP).

opcaoMenuProfissional(Palavras,EmailP):-
    (member("cadastrar",Palavras),member("servico",Palavras)) -> cadastrarServico(EmailP),!;
    (member("listar",Palavras),member("atendimentos",Palavras),member("pendentes",Palavras)) -> listarAtendimentosPendentes(EmailP),!;
    (member("ajuda",Palavras)) -> ajudaProfissional(EmailP),!;
    (member("sair",Palavras)) -> voltaMenuPrincipal,!;
    writeln("\nNão entendi, poderia repetir?"),
    writeln("Caso precise de ajuda digite Ajuda."),
    pausa1,
    menuProfissionalAutenticado(EmailP).

ajudaProfissional(EmailP):-
    limpaTela,
    exibeAjudaProfissional,
    menuProfissionalAutenticado(EmailP).

cadastrarServico(EmailP):-
    profissional(EmailP,_,NomeP,_,_),
    limpaTela,
    exibeInformeDadosCadastro,
    nl,
    write("Descrição: "),
    lerEntrada(Descricao),nl,
    splitEspaco(Descricao,DescricaoL),
    atomic_list_concat(DescricaoL,"," ,DescricaoLA),
    write("Preço: "),
    lerEntradaNum(Preco),nl,
    verificaPreco(Preco) -> cadastrarServicoAux(EmailP,DescricaoLA,Preco,EmailP,NomeP),!;
    nl,writeln("Preço invalido"),pausa1,cadastrarServico(EmailP).
    

cadastrarServicoAux(EmailP,DescricaoLA,Preco,EmailP,NomeP):-
    writeln("Escolha uma dentre as seguintes categorias"),nl,
    pausa1,
    getCategorias(Categorias),
    listaCategorias(Categorias),nl,
    write("Categoria: "),
    lerCategoria(Categoria),nl,
    member(Categoria,Categorias) -> cadastrarServicoAux2(EmailP,Categoria,DescricaoLA,Preco,EmailP,NomeP),!;
    writeln("Categoria invalida"),pausa1,cadastrarServico(EmailP).

cadastrarServicoAux2(EmailP,Categoria,Descricao,Preco,EmailP,NomeP):-
    geraServico(Categoria,Descricao,Preco,EmailP,NomeP,NovoServico),
    append('servicos.pl'),
    writeln(NovoServico),
    told,
    carrega,
    pausa1,
    limpaTela,
    exibeCadastroRealizado,
    pausa2,
    limpaTela,
    exibeBemVindoMenuProfissional,
    menuProfissionalAutenticado(EmailP).

listarAtendimentosPendentes(EmailP):-
    getAtPendentesProfissional(EmailP,AtPendentes),
    length(AtPendentes,Tam),
    Tam > 0 -> limpaTela,
               exibeTodosAtPendentesP,
               pausa1,
               listarAtPendentesProfissional(AtPendentes,StringAtPendentes),
               writeln(StringAtPendentes),
               listarAtendimentosPendentesAux(EmailP,AtPendentes),!;

               pausa1,nl,
               writeln("Não existem atendimentos pendentes"),
               pausa1,
               menuProfissionalAutenticado(EmailP).

listarAtendimentosPendentesAux(EmailP,AtPendentes):-
    writeln("Para aceitar ou recusar um atendimento, informe a ação desejada"),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoListarAtendimentosPendentesAux(EmailP,AtPendentes,EntradaLista).

voltarMenuProfissional(EmailP):-
    pausa1,
    limpaTela,
    exibeBemVindoMenuProfissional,
    menuProfissionalAutenticado(EmailP).

opcaoListarAtendimentosPendentesAux(EmailP,AtPendentes,Palavras):-
    (member("aceitar",Palavras)) -> listarAtendimentosPendentesAceitar(EmailP,AtPendentes),!;
    (member("recusar",Palavras)) -> listarAtendimentosPendentesRecusar(EmailP,AtPendentes),!;
    (member("voltar",Palavras)) -> voltarMenuProfissional(EmailP),!;
    writeln("\nNão entendi, poderia repetir?"),
    writeln("Caso queira voltar digite voltar"),
    pausa2,
    listarAtendimentosPendentes(EmailP).

listarAtendimentosPendentesAceitar(EmailP,AtPendentes):-
    nl,
    writeln("Indique o número do atendimento que deseja aceitar"),
    writeln("Caso queira voltar digite 0"),
    nl,
    write("-> "),
    lerEntradaNum(EntradaNum),
    length(AtPendentes,Tam),
    EntradaNum =< Tam,
    EntradaNum >= 0 -> listarAtendimentosPendentesAceitarAux(EmailP,AtPendentes,EntradaNum),!;

                       nl,
                       writeln("Opção inválida"),
                       pausa2,
                       listarAtendimentosPendentes(EmailP).


listarAtendimentosPendentesAceitarAux(EmailP,AtPendentes,Num):-
    Num =:= 0 -> pausa1,
                 limpaTela,
                 exibeBemVindoMenuProfissional,
                 menuProfissionalAutenticado(EmailP),!;
 
                 I is Num - 1,
                 nth0(I,AtPendentes,AtPend),
                 geraRetractAtPendente(AtPend,RetractAtPend),
                 geraAtAceito(AtPend,AtAceito),
                 append('atPendentes.pl'),
                 writeln(RetractAtPend),
                 told,
                 append('atAceitos.pl'),
                 writeln(AtAceito),
                 told,
                 carrega,
                 pausa1,
                 limpaTela,
                 exibeAtendimentoAceito,
                 pausa2,
                 limpaTela,
                 exibeBemVindoMenuProfissional,
                 menuProfissionalAutenticado(EmailP).








listarAtendimentosPendentesRecusar(EmailP,AtPendentes):-
    nl,
    writeln("Indique o número do atendimento que deseja recusar"),
    writeln("Caso queira voltar digite 0"),
    nl,
    write("-> "),
    lerEntradaNum(EntradaNum),
    length(AtPendentes,Tam),
    EntradaNum =< Tam,
    EntradaNum >= 0 -> listarAtendimentosPendentesRecusarAux(EmailP,AtPendentes,EntradaNum),!;

                       nl,
                       writeln("Opção inválida"),
                       pausa2,
                       listarAtendimentosPendentes(EmailP).


listarAtendimentosPendentesRecusarAux(EmailP,AtPendentes,Num):-
    Num =:= 0 -> pausa1,
                 limpaTela,
                 exibeBemVindoMenuProfissional,
                 menuProfissionalAutenticado(EmailP),!;
 
                 I is Num - 1,
                 nth0(I,AtPendentes,AtPend),
                 geraRetractAtPendente(AtPend,RetractAtPend),
                 geraAtRecusado(AtPend,AtRecusado),
                 append('atPendentes.pl'),
                 writeln(RetractAtPend),
                 told,
                 append('atRecusados.pl'),
                 writeln(AtRecusado),
                 told,
                 carrega,
                 pausa1,
                 limpaTela,
                 exibeAtendimentoRecusado,
                 pausa2,
                 limpaTela,
                 exibeBemVindoMenuProfissional,
                 menuProfissionalAutenticado(EmailP).


