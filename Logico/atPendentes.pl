:- dynamic atPendente/8.

atPendente(email,nome,desing,[asd,asd,dsa],2,emailp,nomep,1).
atPendente(email,nome,construção,[alguma,coisa],45,emailp,nomep,1).
?- retract(atPendente(email,nome,construção,[alguma,coisa],45,emailp,nomep,1)).
?- retract(atPendente(email,nome,desing,[asd,asd,dsa],2,emailp,nomep,1)).
