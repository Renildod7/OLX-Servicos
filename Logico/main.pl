:- use_module('./template.pl').
:- use_module('./util.pl').

inicio:-
    carrega,
    limpaTela,
    exibeBemVindoOLX,
    menu.

menu:-
    nl,
    writeln('Como posso ajudar?'),nl,
    write("-> "),
    lerEntrada(Entrada),
    splitEspaco(Entrada,EntradaLista),
    opcaoMenuPrincpal(EntradaLista).

opcaoMenuPrincpal(Palavras):-
    (member("cadastrar",Palavras),member("cliente",Palavras)) -> cadastrarCliente;
    (member("cadastrar",Palavras),member("profissional",Palavras)) -> cadastrarProfissional;
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

