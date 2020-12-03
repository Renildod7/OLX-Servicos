:- module(palavrasChave,[
    cadastrar/1,
    login/1,
    cliente/1,
    profissional/1,
    listar/1,
    servico/1,
    contratar/1,
    contratados/1,
    concluir/1,
    atendimento/1,
    pendentes/1,
    aceitos/1,
    recusados/1,
    concluidos/1,
    faturamento/1,
    todos/1,
    categorias/1,
    aceitar/1,
    recusar/1,
    voltar/1,
    sair/1,
    ajuda/1,
    atendimentos/1
    ]).


cadastrar(Palavras):-
    intersection(Palavras,["cadastrar","cadastro","cadastra"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

login(Palavras):-
    intersection(Palavras,["login","entrar","acessar"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

cliente(Palavras):-
    intersection(Palavras,["cliente","consumidor", "comprador"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

profissional(Palavras):-
    intersection(Palavras,["profissional","trabalhador","prestador"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

listar(Palavras):-
    intersection(Palavras,["listar","lista","mostrar","exibir"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

servico(Palavras):-
    intersection(Palavras,["serviços","serviço","servicos","servico"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

contratar(Palavras):-
    intersection(Palavras,["contratar","contrata"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

contratados(Palavras):-
    intersection(Palavras,["contratados","contratado"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

concluir(Palavras):-
    intersection(Palavras,["concluir","conclui","finalizar","finaliza","terminar","termina"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

atendimento(Palavras):-
    intersection(Palavras,["atendimento","atendimentos"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

pendentes(Palavras):-
    intersection(Palavras,["pendentes","pendente"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

aceitos(Palavras):-
    intersection(Palavras,["aceito","aceitos"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

recusados(Palavras):-
    intersection(Palavras,["recusado","recusados"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

concluidos(Palavras):-
    intersection(Palavras,["concluidos","concluido"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

faturamento(Palavras):-
    intersection(Palavras,["faturamento","faturamentos"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

todos(Palavras):-
    intersection(Palavras,["todos","todo"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

categorias(Palavras):-
    intersection(Palavras,["categoria","categorias"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

aceitar(Palavras):-
    intersection(Palavras,["aceitar","aceita","aceito","aceitos"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

recusar(Palavras):-
    intersection(Palavras,["recusar","recusa","negar"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

voltar(Palavras):-
    intersection(Palavras,["voltar","volta"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

sair(Palavras):-
    intersection(Palavras,["sair","sai"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

ajuda(Palavras):-
    intersection(Palavras,["ajuda","ajudar","ajudas"],Intersec),
    length(Intersec,Tam),
    Tam > 0.

atendimentos(Palavras):-
    intersection(Palavras,["atendimentos","atendimento"],Intersec),
    length(Intersec,Tam),
    Tam > 0.