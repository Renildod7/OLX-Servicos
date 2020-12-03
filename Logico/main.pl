:- use_module('./template.pl').
:- use_module('./util.pl').
:- use_module('./palavrasChave.pl').

:- initialization(inicio).

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
    [atPendentes],
    [atAceitos],
    [atRecusados],
    [atConcluidos].

menu:-
    nl,
    writeln('Como posso ajudar?'),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoMenuPrincpal(EntradaLista).

opcaoMenuPrincpal(Palavras):-
    (cadastrar(Palavras),cliente(Palavras)) -> cadastrarCliente,!;
    (login(Palavras),cliente(Palavras)) -> loginCliente,!;
    (cadastrar(Palavras),profissional(Palavras)) -> cadastrarProfissional,!;
    (login(Palavras),profissional(Palavras)) -> loginProfissional,!;
    (sair(Palavras)) -> sair,!;
    (ajuda(Palavras)) -> ajuda,!;
    writeln("\nNão entendi, poderia repetir?"),
    writeln("Caso precise de ajuda digite Ajuda."),
    pausa1,
    menu.

sair:-
    limpaTela,
    msgSair,
    pausa5,
    limpaTela,
    halt.

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
    (contratar(Palavras),servico(Palavras)) -> contratarServico(EmailC),!;
    (concluir(Palavras),atendimento(Palavras)) -> concluirAtendimento(EmailC),!;
    (listar(Palavras),servico(Palavras),contratados(Palavras)) -> listarServicosContratadosCliente(EmailC),!;
    (ajuda(Palavras)) -> ajudaCliente(EmailC),!;
    (sair(Palavras)) -> voltaMenuPrincipal,!;
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
    (listar(Palavras),todos(Palavras)) -> contratarServicoListar(EmailC),!;
    (categorias(Palavras)) -> contratarServicoCategoria(EmailC),!;
    (voltar(Palavras)) -> voltarMenuCliente(EmailC),!;
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

contratarServicoCategoriaAux(EmailC,Categoria):-
    getServicosCategoria(Categoria,Servicos),
    length(Servicos,Tam),
    Tam > 0,
    ordenaServicosAvaliacao(Servicos,ServicosOrd),
    getDoisMelhorAvaliados(ServicosOrd,DoisServicos),
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
    opcaoContratarServicoCategoriaAux(EmailC,Categoria,ServicosOrd,DoisServicos,EntradaLista),!;
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
    (listar(Palavras),todos(Palavras)) ->   pausa1,
                                            limpaTela,
                                            exibeTodosServicosCategoria,
                                            contratarServicoCategoriaListarContratar(EmailC,Categoria,Servicos),!;
    (contratar(Palavras)) ->  limpaTela,
                              exibeServicosAvaliadosCategoria,
                              contratarServicoCategoriaListarContratar(EmailC,Categoria,DoisServicos),!;

    (voltar(Palavras)) -> voltarMenuCliente(EmailC),!;
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
                                limpaTela,
                                exibeTodosServicosCategoria,
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
               writeln(StringAtAceitos),nl,
               concluirAtendimentoAux(EmailC,AtAceitos,Tam),!;


               pausa1,nl,
               writeln("Não existem atendimentos pendentes"),
               pausa1,
               menuClienteAutenticado(EmailC).


concluirAtendimentoAux(EmailC,AtAceitos,Tam):-
    writeln("Para finalizar um atendimento, indique seu número"),
    writeln("Caso não queira finalizar nenhum deles digite 0."),nl,
    write("-> "),
    lerEntradaNum(EntradaNum),
    EntradaNum =< Tam -> concluirAtendimentoAux2(EmailC,AtAceitos,EntradaNum),!;

                         nl,
                         writeln("Opção inválida"),
                         pausa1,
                         concluirAtendimento(EmailC).

concluirAtendimentoAux2(EmailC,AtAceitos,EntradaNum):-
    EntradaNum =:= 0 ->  pausa1,
                         limpaTela,
                         exibeBemVindoMenuCliente,
                         menuClienteAutenticado(EmailC),!;
         
                            
                         concluirAtendimentoAux3(EmailC,AtAceitos,EntradaNum).


concluirAtendimentoAux3(EmailC,AtAceitos,EntradaNum):-
    nl,
    writeln("Avalie seu atendimento com um número de 0 a 10"),
    nl,
    write("-> "),
    lerEntradaNum(Avaliacao),
    Avaliacao =< 10,
    Avaliacao >= 0 ->    I is EntradaNum - 1,
                         nth0(I,AtAceitos,AtAceit),
                         geraRetractAtAceito(AtAceit,RetractAtAceito),
                         geraAtendimentoConcluitdo(AtAceit,Avaliacao,NovoAtConcluido),
                         append('atAceitos.pl'),
                         writeln(RetractAtAceito),
                         told,
                         append('atConcluidos.pl'),
                         writeln(NovoAtConcluido),
                         told,
                         carrega,
                         pausa1,
                         limpaTela,
                         exibeAtendimentoConcluido,
                         pausa2,
                         limpaTela,
                         exibeBemVindoMenuCliente,
                         menuClienteAutenticado(EmailC),!;

                         nl,
                         writeln("Avaliação inválida"),
                         writeln("Caso não seja um número inteiro, utilize um ponto (.), para separar"),
                         writeln("a parte interia da parte fracionária. Exemplo: 2.1"),
                         pausa2,
                         concluirAtendimentoAux2(EmailC,AtAceitos,EntradaNum).


listarServicosContratadosCliente(EmailC):-
    nl,writeln("Qual tipo de atendimento você gostaria de ver?"),
    nl,writeln("Pendentes\nAceitos\nRecusados\nConcluidos"),
    nl,
    pausa1,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoListarServicosContratadosCliente(EmailC,EntradaLista).

opcaoListarServicosContratadosCliente(EmailC,Palavras):-
    (pendentes(Palavras)) -> listarServicosContratadosClientePendentes(EmailC),!;
    (aceitos(Palavras)) -> listarServicosContratadosClienteAceitos(EmailC),!;
    (recusados(Palavras)) -> listarServicosContratadosClienteRecusados(EmailC),!;
    (concluidos(Palavras)) -> listarServicosContratadosClienteConcluidos(EmailC),!;
    (voltar(Palavras)) -> voltarMenuCliente(EmailC),!;
    nl,writeln("Opção inválida"),
    writeln("Caso queira voltar digite voltar"),
    pausa1,
    listarServicosContratadosCliente(EmailC).




listarServicosContratadosClientePendentes(EmailC):-
    getServicosContratadosClientePendentes(EmailC,Pendentes),
    length(Pendentes,Tam),
    Tam > 0 -> pausa1,
               limpaTela,
               exibeTodosAtPendentes,
               pausa1,
               listaAtNaoCloncluidoCliente(Pendentes,String),
               writeln(String),
               pausa1,
               listarServicosContratadosCliente(EmailC);

               pausa1,nl,
               writeln("Você não possui atendimentos pendentes"),
               pausa1,
               listarServicosContratadosCliente(EmailC).

listarServicosContratadosClienteAceitos(EmailC):-
    getServicosContratadosClienteAceitos(EmailC,Aceitos),
    length(Aceitos,Tam),
    Tam > 0 -> pausa1,
               limpaTela,
               exibeTodosAtAceitos,
               pausa1,
               listaAtNaoCloncluidoCliente(Aceitos,String),
               writeln(String),
               pausa1,
               listarServicosContratadosCliente(EmailC);

               pausa1,nl,
               writeln("Você não possui atendimentos aceitos"),
               pausa1,
               listarServicosContratadosCliente(EmailC).

listarServicosContratadosClienteRecusados(EmailC):-
    getServicosContratadosClienteRecusados(EmailC,Recusados),
    length(Recusados,Tam),
    Tam > 0 -> pausa1,
               limpaTela,
               exibeTodosAtRecusados,
               pausa1,
               listaAtNaoCloncluidoCliente(Recusados,String),
               writeln(String),
               pausa1,
               listarServicosContratadosCliente(EmailC);

               pausa1,nl,
               writeln("Você não possui atendimentos recusados"),
               pausa1,
               listarServicosContratadosCliente(EmailC).


listarServicosContratadosClienteConcluidos(EmailC):-
    getServicosContratadosClienteConcluidos(EmailC,Concluidos),
    length(Concluidos,Tam),
    Tam > 0 -> pausa1,
               limpaTela,
               exibeTodosAtConcluidos,
               pausa1,
               listaAtCloncluidoCliente(Concluidos,String),
               writeln(String),
               pausa1,
               listarServicosContratadosCliente(EmailC);

               pausa1,nl,
               writeln("Você não possui atendimentos concluidos"),
               pausa1,
               listarServicosContratadosCliente(EmailC).


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
    (cadastrar(Palavras),servico(Palavras)) -> cadastrarServico(EmailP),!;
    (listar(Palavras),atendimentos(Palavras),pendentes(Palavras)) -> listarAtendimentosPendentes(EmailP),!;
    (faturamento(Palavras)) -> faturamentoServicos(EmailP),!;
    (ajuda(Palavras)) -> ajudaProfissional(EmailP),!;
    (sair(Palavras)) -> voltaMenuPrincipal,!;
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
    (aceitar(Palavras)) -> listarAtendimentosPendentesAceitar(EmailP,AtPendentes),!;
    (recusar(Palavras)) -> listarAtendimentosPendentesRecusar(EmailP,AtPendentes),!;
    (voltar(Palavras)) -> voltarMenuProfissional(EmailP),!;
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

faturamentoServicos(EmailP):-
    getFaturamentoProfissional(EmailP,Faturamentos),
    pausa1,
    write(Faturamentos),
    pausa1,
    menuProfissionalAutenticado(EmailP).