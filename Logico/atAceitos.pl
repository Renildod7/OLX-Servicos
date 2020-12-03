:- dynamic atAceito/8.

atAceito(email,nomec1,moda,[moda],5,emailp,nomep,1).
atAceito(email,nomec1,informatica,[informatica],5,emailp,nomep,1).
?- retract(atAceito(email,nomec1,moda,[moda],5,emailp,nomep,1)).
