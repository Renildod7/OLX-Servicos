:- use_module('./template.pl').
:- use_module('./util.pl').

inicio:-
    carrega,
    limpaTela,
    exibeBemVindoOLX,
    menu.

carrega:-
    [clientes],
    [profissionais].

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



menuClienteAutenticado(Email):-
    nl,
    writeln('Como posso ajudar?'),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista).








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

menuProfissionalAutenticado(Email):-
    nl,
    writeln('Como posso ajudar?'),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista).