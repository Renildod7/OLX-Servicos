:- dynamic atPendente/8.

atPendente(email,nomec1,moda,[moda],5,emailp,nomep,1).
atPendente(email,nomec1,informatica,[informatica],5,emailp,nomep,1).
atPendente(email,nomec1,construção,[construção],2,emailp,nomep,1).
atPendente(email,nomec1,saude,[saude],1,emailp,nomep,1).
?- retract(atPendente(email,nomec1,moda,[moda],5,emailp,nomep,1)).
?- retract(atPendente(email,nomec1,construção,[construção],2,emailp,nomep,1)).
?- retract(atPendente(email,nomec1,informatica,[informatica],5,emailp,nomep,1)).
