% Název projektu: Rubikova kostka
% Login: xpolan07
% Autor: Petr Polanský


:-dynamic zprac_state/2, % stavy ktere jiz byly zpracovany
 		  k_zpracovani/2. % stavy ktere se budou zpracovavat





%Reads line from stdin, terminates on LF or EOF.
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

%Tests if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).



% rozdeli radek na podseznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1

% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).


% predstavuje slozenou kostku

sloz([['5','5','5'],
	['5','5','5'],
	['5','5','5'],
	['1','1','1'],['2','2','2'],['3','3','3'],['4','4','4'],
	['1','1','1'],['2','2','2'],['3','3','3'],['4','4','4'],
	['1','1','1'],['2','2','2'],['3','3','3'],['4','4','4'],
	['6','6','6'],
	['6','6','6'],
	['6','6','6']]).


% slouzi pro overeni slozene kostky s aktualnim stavem kostky

konv([[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]],
	     	[[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]]).


% ziskame kostku ze vstupu pomoci prepripravenych funkci
% trasformujeme takto ziskanou kostku na seznam stran
% vstupni stav kostky si ulozime jako prvni ke zpracovani
% jako otce prvniho stavu pouzijeme prazdny seznam

start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		transformace(S,K),	
		
		assert(k_zpracovani(K,[])),
		solve(K,[]),
	
		halt.



% provedeme pro dany stav prohledani
% ke kazdemu stavu si ukladame i jeho otce
% otcovske stavy pote budou slouzit pro vypis cesty reseni
% stav dame ke zpracovannym
% stav odebereme ze k_zpracovani
% vybereme novy stav a zavolame rekurzivne solve

solve(Aktual,Otec):- moves(6,Aktual,Otec),				  
				  assert(zprac_state(Aktual,Otec)), 
				  (sloz(X),konv(X,Aktual) -> seznam_cesty(Aktual,Cesta), revert(Cesta, Nova), vypis_cesty(Nova), halt; true),				  
				  retract(k_zpracovani(Aktual,_)), 				  
				  k_zpracovani(Novy,Stary), 				 				 
				  solve(Novy,Stary).


% provadeni vsech pohybu pro jeden stav
% pokud novy stav je koncovym stavem, tak vypiseme cestu 

moves(0,_,_). 
moves(N,T,Otec):- move(N,T,S),
			   proti_stejnym(S,T),
			   (sloz(X),konv(X,S) -> assert(zprac_state(T,Otec)), 
			   	assert(zprac_state(S,T)), seznam_cesty(S,Cesta), revert(Cesta, Nova), vypis_cesty(Nova),halt; true), 
			   N1 is N - 1, 
		 	   moves(N1,T,Otec).



% pokud stav je uz k zpracovani nebo jiz zpracovan, tak jej preskakujeme
% jinak jej ukladame ke zpracovani

proti_stejnym(S,T):- ((zprac_state(S,_);k_zpracovani(S,_)) -> true; assert(k_zpracovani(S,T))).


% vypise cestu pomoci vsech otcu vysledneho uzlu

vypis_cesty([]).
vypis_cesty([Stav|Zbytek]):- vypis(Stav),nl,vypis_cesty(Zbytek).

% vytvori seznam stavu na ceste k cili

seznam_cesty([],_).
seznam_cesty(Stav,[Stav|Cesta]):- zprac_state(Stav, Otec), seznam_cesty(Otec, Cesta).


% obraceni seznamu

revert(In, Out):- revert(In, [], Out).
revert([],L,L).
revert([X|T],L,P):- revert(T,[X|L],P).


% A
% B C D E
% E

% transformuje na schudnejsi reprezentaci

transformace([[[A1,A2,A3]],
			[[A4,A5,A6]],
			[[A7,A8,A9]],
	    	[[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3]],
	    	[[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6]],
	    	[[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9]],
	      	[[F1,F2,F3]],
	      	[[F4,F5,F6]],
	      	[[F7,F8,F9]]],
	     	[[A1,A2,A3],
	     	[A4,A5,A6],
	     	[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	    	[F1,F2,F3],
	      	[F4,F5,F6],
	      	[F7,F8,F9]]).

% slouzi pro vypis seznamu

vypis( [[A1,A2,A3],
	    [A4,A5,A6],
	    [A7,A8,A9],
	    [B1,B2,B3],	    [C1,C2,C3],	    [D1,D2,D3],	    [E1,E2,E3],
	    [B4,B5,B6],	    [C4,C5,C6],	    [D4,D5,D6],	    [E4,E5,E6],
	    [B7,B8,B9],	    [C7,C8,C9],	    [D7,D8,D9],	    [E7,E8,E9],
	    [F1,F2,F3],
	    [F4,F5,F6],
	    [F7,F8,F9]]):- 
write(A1),write(A2), writeln(A3),
write(A4),write(A5), writeln(A6),
write(A7),write(A8), writeln(A9),
write(B1),write(B2), write(B3),write(' '),
write(C1),write(C2), write(C3),write(' '),
write(D1),write(D2), write(D3),write(' '),
write(E1),write(E2), writeln(E3),
write(B4),write(B5), write(B6),write(' '),
write(C4),write(C5), write(C6),write(' '),
write(D4),write(D5), write(D6),write(' '),
write(E4),write(E5), writeln(E6),
write(B7),write(B8), write(B9),write(' '),
write(C7),write(C8), write(C9),write(' '),
write(D7),write(D8), write(D9),write(' '),
write(E7),write(E8), writeln(E9),
write(F1),write(F2), writeln(F3),
write(F4),write(F5), writeln(F6),
write(F7),write(F8), writeln(F9).
					  


% DEFINICE POHYBU


move(1,T,S):- prvni_dolu(T,S).

move(2,T,S):- treti_nahoru(T,S).

move(3,T,S):- prvni_doleva(T,S).

move(4,T,S):- treti_doprava(T,S).

move(5,T,S):- predni_doprava(T,S).

move(6,T,S):- zadni_doleva(T,S).




prvni_dolu([[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]],

	   	   [[D9,A2,A3],
			[D6,A5,A6],
			[D3,A8,A9],
	    	[A1,B2,B3],[C1,C2,C3],[D1,D2,F7],[E7,E4,E1],
	    	[A4,B5,B6],[C4,C5,C6],[D4,D5,F4],[E8,E5,E2],
	    	[A7,B8,B9],[C7,C8,C9],[D7,D8,F1],[E9,E6,E3],
	     	[B1,F2,F3],
	     	[B4,F5,F6],
	     	[B7,F8,F9]]).	

treti_nahoru([[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]],

	   	   [[A1,A2,B3],
			[A4,A5,B6],
			[A7,A8,B9],
	    	[B1,B2,F3],[C7,C4,C1],[A9,D2,D3],[E1,E2,E3],
	    	[B4,B5,F6],[C8,C5,C2],[A6,D5,D6],[E4,E5,E6],
	    	[B7,B8,F9],[C9,C6,C3],[A3,D8,D9],[E7,E8,E9],
	     	[F1,F2,D7],
	     	[F4,F5,D4],
	     	[F7,F8,D1]]).	  


prvni_doleva([[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]],

	   	   [[A7,A4,A1],
			[A8,A5,A2],
			[A9,A6,A3],
	    	[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],[B1,B2,B3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]]).

treti_doprava([[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]],

	   	   [[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[E7,E8,E9],[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],
	     	[F7,F4,F1],
	     	[F8,F5,F2],
	     	[F9,F6,F3]]).

predni_doprava([[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]],

	       [[A1,A2,A3],
			[A4,A5,A6],
			[E9,E6,E3],
	    	[B7,B4,B1],[A7,C2,C3],[D1,D2,D3],[E1,E2,F1],
	    	[B8,B5,B2],[A8,C5,C6],[D4,D5,D6],[E4,E5,F2],
	    	[B9,B6,B3],[A9,C8,C9],[D7,D8,D9],[E7,E8,F3],
	     	[C7,C4,C1],
	     	[F4,F5,F6],
	     	[F7,F8,F9]]).

zadni_doleva([[A1,A2,A3],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,C3],[D1,D2,D3],[E1,E2,E3],
	    	[B4,B5,B6],[C4,C5,C6],[D4,D5,D6],[E4,E5,E6],
	    	[B7,B8,B9],[C7,C8,C9],[D7,D8,D9],[E7,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[F7,F8,F9]],

	       [[C3,C6,C9],
			[A4,A5,A6],
			[A7,A8,A9],
	    	[B1,B2,B3],[C1,C2,F9],[D7,D4,D1],[A3,E2,E3],
	    	[B4,B5,B6],[C4,C5,F8],[D8,D5,D2],[A2,E5,E6],
	    	[B7,B8,B9],[C7,C8,F7],[D9,D6,D3],[A1,E8,E9],
	     	[F1,F2,F3],
	     	[F4,F5,F6],
	     	[E1,E4,E7]]).
