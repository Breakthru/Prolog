:- op(903,xfx,->).
:- op(902,xfy,v).
:- op(901,xfy,^).
:- op(900,fy,~).

aggiungi_in_fondo(Lista1,Lista,Ris) :- append(Lista,Lista1,Ris).

dimostra(Formula) :- tabl([~Formula],[],0),!,nl,write('formula valida'),nl,write('immetti altra formula o fine per terminare'),nl,read(X),start(X).
dimostra(Formula):- nl,write('immetti altra formula o fine per terminare'),nl,read(X),start(X).

start :- write('Benvenuto, immetti una formula ben formata seguita da un punto'),nl, read(X),start(X).

start(X):- atom(X),X=fine,!,write('Fine del programma.').
start(X) :- fbf(X),!,dimostra(X).
start(X) :- write('Non hai immesso una formula ben formata, una formula ben formata non deve contenere numeri nè variabili prolog'),nl,read(Y),start(Y).


fbf(X) :- var(X),!,fail.
fbf(X) :- atom(X).
fbf(X ^ Y) :- fbf(X),fbf(Y).
fbf(X v Y) :- fbf(X),fbf(Y).
fbf(X -> Y) :- fbf(X),fbf(Y).
fbf(~X) :- fbf(X).
fbf(perogni(X,P)):-fbf(P).
fbf(esiste(X,P)):-fbf(P).

fbf(X) :- compound(X),functor(X,N,A),fbf(A,X).
fbf(A,X) :- A>0,arg(A,X,Arg),fbf(Arg),A1 is (A-1),fbf(A1,X).
fbf(0,X).

tabl([],[],N).

/* controllo di chiusura di formule non ancora sviluppate*/

tabl([~(~X)|Xs],A,N) :- member(~X,A),!,write('ramo chiuso '),write([X|A]).
/*tabl([X|Xs],A,N) :- member(~X,Xs),!,write('ramo chiuso '),write([X|Xs]).*/

tabl([X^Y|Xs],A,N) :- member(~X,[Y|A]),!,write('ramo chiuso '),write([X,Y|A]).
tabl([X^Y|Xs],A,N) :- member(~Y,[X|A]),!,write('ramo chiuso '),write([Y,X|A]).

tabl([~(X v ~(X))|Xs],A,N) :- !,write('ramo chiuso ').
tabl([~(~(X) v X)|Xs],A,N) :- !,write('ramo chiuso ').
tabl([~(X v Y)|Xs],A,N) :- member(X,A),!,write('ramo chiuso '),write([~X,~Y|A]).
tabl([~(X v Y)|Xs],A,N) :- member(Y,A),!,write('ramo chiuso '),write([~Y,~X|A]).

tabl([~(X->Y)|Xs],A,N) :- member(Y,[X|A]),!,write('ramo chiuso '),write([~Y,X|A]).
tabl([~(X->Y)|Xs],A,N) :- member(~X,[Y|A]),!,write('ramo chiuso '),write([X,Y|A]).

tabl([X v Y|Xs],A,N) :- member(~X,A),member(~Y,A),!,write('ramo chiuso '),write([X,Y|A]).

tabl([~(X^Y)|Xs],A,N) :- member(X,A),member(Y,A),!,write('ramo chiuso '),write([~X,~Y|A]).

tabl([X->Y|Xs],A,N) :- member(X,A),member(~Y,A),!,write('ramo chiuso '),write([~X,Y|A]).

/* fine controllo chiusura senza sviluppare.*/
/* inizio sviluppo formule*/
tabl([~(~X)|Xs],A,N) :- !,ordina([X|Xs],Ris),tabl(Ris,A,N).

tabl([X^Y|Xs],A,N) :- !,ordina([X,Y|Xs],Ris),tabl(Ris,[X^Y|A],N).

tabl([~(X v Y)|Xs],A,N) :- !,ordina([~X,~Y|Xs],Ris),tabl(Ris,[~(X v Y)|A],N).

tabl([~(X->Y)|Xs],A,N) :- !,ordina([X,~Y|Xs],Ris),tabl(Ris,[~(X -> Y)|A],N).

tabl([X v Y|Xs],A,N) :- !,ordina([X|Xs],Ris1),tabl(Ris1,[X v Y|A],N),ordina([Y|Xs],Ris2),tabl(Ris2,[X v Y|A],N).

tabl([~(X^Y)|Xs],A,N) :- !,ordina([~X|Xs],Ris1),tabl(Ris1,[~(X^Y)|A],N),ordina([~Y|Xs],Ris2),tabl(Ris2,[~(X^Y)|A],N).

tabl([X->Y|Xs],A,N) :- !,ordina([Y|Xs],Ris1),tabl(Ris1,[X->Y|A],N),ordina([~X|Xs],Ris2),tabl(Ris2,[X->Y|A],N).

tabl([~(perogni(X,P))|Xs],A,N) :- !,tabl([esiste(X,~(P))|Xs],A,N).

tabl([esiste(X,P)|Xs],A,N) :- !,N1 is (N + 1),sost(X,P,P1,N1),tabl([P1|Xs],A,N1).

tabl([~(esiste(X,P))|Xs],A,N) :- !,tabl([perogni(X,~(P))|Xs],A,N).

/* la prima volta che incontra la formula gamma*/
/*se ci sono formule delta da sviluppare la svilupperò dopo*/
tabl([perogni(X,P)|Xs],A,N) :- member(esiste(Y,P1),Xs),!,append(Xs,[perogni(X,P)],R),tabl(R,A,N).
tabl([perogni(X,P)|Xs],A,N) :- member(~perogni(Y,P1),Xs),!,append(Xs,[perogni(X,P)],R),tabl(R,A,N).
/* se non ci sono formule delta devo istanziarla almeno una volta*/
tabl([perogni(X,P)|Xs],A,0) :- !,sost(X,P,P1,1),tabl([P1|Xs],[perogni(X,P,1)|A],1).
/* devo istanziarla su tutti i parametri usati finora */
tabl([perogni(X,P)|Xs],A,N) :- !,sostgamma(X,P,0,N,L),append(L,Xs,R),tabl(R,[perogni(X,P,N)|A],N).

/* formula X non è più sviluppabile*/
tabl([X|Xs],A,N) :- tabl(Xs,[X|A],N).
/* controllo chiusura degli atomi.*/
tabl([],A,N) :- chiude(A),write('RAMO CHIUSO '),write(A),nl,!.

tabl([],A,N) :- N>10,write('non lo so'),nl,!,fail.


/* tentativo di re-istanziare*/
tabl([],A,N) :- member(perogni(X,P,M),A),M<N,!,sostgamma(X,P,M,N,L),rimuovi(perogni(X,P,M),A,Ris),
		append(Ris,[perogni(X,P,N)],Ris1),tabl(L,Ris1,N).

tabl([],A,N) :- N1 is (N + 1),
		member(perogni(X,P,M),A),!,
		write('ramo con sole formule gamma, reistanzio   '),
		write(perogni(X,P)),nl,
		sostgamma(X,P,M,N1,L),
		rimuovi(perogni(X,P,M),A,Ris),
		append(Ris,[perogni(X,P,N1)],Ris1),
		tabl(L,Ris1,N1).

tabl([],A,N) :- write('non valida'),nl,write(A),nl,write('ecco un controesempio.'),nl,!,fail.

chiude(A) :- member(X,A),member(~X,A).

sost(X,P,N,N) :- P=X.
sost(X,P,P,N) :- atom(P),!,P\=X.
sost(X,P,P1,N) :- functor(P,Nome,Ari),functor(P1,Nome,Ari),sost(Ari,X,P,P1,N).

sost(A,X,P,P1,N):-      A>0,
                        arg(A,P,Arg),
                        sost(X,Arg,SostArg,N),
                        arg(A,P1,SostArg),
                        A1 is A-1,
                        sost(A1,X,P,P1,N).

sost(0,X,P,P1,N).

sostgamma(X,P,I,I,[]).

sostgamma(X,P,I,N,[P1|L]) :- N>I,
			sost(X,P,P1,N),
			N1 is (N-1),
			sostgamma(X,P,I,N1,L).

rimuovi(E,[E|List],List).

rimuovi(E,[X|List],[X|Ris]) :- E\=X,rimuovi(E,List,Ris).

/*ordina(Lista,Risultato):-ordina(Lista,[],[],Risultato).
ordina([X|Xs],Cong,Disg,Risultato) :- congiuntiva(X),!,ordina(Xs,[X|Cong],Disg,Risultato).
ordina([X|Xs],Cong,Disg,Risultato) :- ordina(Xs,Cong,[X|Disg],Risultato).
ordina([],Cong,Disg,Risultato) :- append(Cong,Disg,Risultato).
*/

ordina(X,Y) :- ordina(X,[],Y).
ordina([X|Xs],Acc,Ris) :- congiuntiva(X),!,append([X|Xs],Acc,Ris).
ordina([X|Xs],Acc,Risultato) :- ordina(Xs,[X|Acc],Risultato).
ordina([],Acc,Acc).

congiuntiva(~(~X)).
congiuntiva(X^Y).
congiuntiva(~(X v Y)).
congiuntiva(~(X->Y)).
congiuntiva(esiste(X,P)).
congiuntiva(~perogni(X,P)).
/* congiuntiva(X) :- atom(X).*/

disgiuntiva(X) :- not(congiuntiva(X)).
/*
not(X) :- X,!,fail.
not(X).
  */

