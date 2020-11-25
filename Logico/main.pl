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
    [avaliacoes].

menu:-
    nl,
    writeln('Como posso ajudar?'),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoMenuPrincpal(EntradaLista).

opcaoMenuPrincpal(Palavras):-
    (member("cadastrar",Palavras),member("cliente",Palavras)) -> cadastrarCliente;
    (member("login",Palavras),member("cliente",Palavras)) -> loginCliente;
    (member("cadastrar",Palavras),member("profissional",Palavras)) -> cadastrarProfissional;
    (member("login",Palavras),member("profissional",Palavras)) -> loginProfissional;
    (member("sair",Palavras)) -> sair;
    (member("ajuda",Palavras)) -> ajuda;
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
    geraCliente(Nome,Email,Senha,Endereco,Telefone,NovoCliente),
    append('clientes.pl'),
    writeln(NovoCliente),
    told,
    carrega.


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
                                    menuClienteAutenticado(EmailA);

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
    (member("contratar",Palavras),member("servico",Palavras)) -> contratarServico(EmailC);
    (member("ajuda",Palavras)) -> ajudaCliente(EmailC);
    (member("sair",Palavras)) -> voltaMenuPrincipal.


ajudaCliente(EmailC):-
    limpaTela,
    exibeAjudaCliente,
    menuClienteAutenticado(EmailC).

contratarServico(EmailC):-
    (getTodosServicos(TodosServicos),
    length(TodosServicos,QtdServicos),
    QtdServicos =:= 0) -> naoExistemServicosCadastrados(EmailC);
    
    writeln("Gostaria de escolher uma categoria de serviços ou listar todos"),
    writeln("os serviços disponiveis?").


    %getTodosServicos(TodosServicos2),
    %listarServicos(TodosServicos2,String),
    %writeln(String).

naoExistemServicosCadastrados(EmailC):-
    pausa1,
    limpaTela,
    exibeNaoExistemServicos,
    pausa2,
    limpaTela,
    exibeBemVindoMenuCliente,
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
    geraProfissional(Nome,Email,Senha,Endereco,Telefone,NovoProfissonal),
    append('profissionais.pl'),
    writeln(NovoProfissonal),
    told,
    carrega.


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
                                    menuProfissionalAutenticado(EmailA);

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
    (member("cadastrar",Palavras),member("servico",Palavras)) -> cadastrarServico(EmailP);
    (member("ajuda",Palavras)) -> ajudaProfissional(EmailP);
    (member("sair",Palavras)) -> voltaMenuPrincipal.

ajudaProfissional(EmailP):-
    limpaTela,
    exibeAjudaProfissional,
    menuProfissionalAutenticado(EmailP).

cadastrarServico(EmailP):-
    profissional(EmailP,_,NomeP,_,_),
    write(NomeP),
    limpaTela,
    exibeInformeDadosCadastro,
    nl,
    write("Descrição: "),
    lerEntrada(Descricao),nl,
    splitEspaco(Descricao,DescricaoL),
    atomic_list_concat(DescricaoL,"," ,DescricaoLA),
    write("Preço: "),
    lerEntrada(Preco),nl,
    string_to_atom(Preco,PrecoA),
    categorias(Categorias),
    listaCategorias(Categorias),nl,
    write("Categoria: "),
    lerEntrada(Categoria),nl,
    string_to_atom(Categoria,CategoriaA),
    geraServico(CategoriaA,DescricaoLA,PrecoA,EmailP,NomeP,NovoServico),
    append('servicos.pl'),
    writeln(NovoServico),
    told,
    carrega.