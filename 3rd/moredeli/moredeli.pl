/*map(x,y,symbol) prev(x,y,m,cost)*/

insert_pq(State, [], [State]).
insert_pq(State, [H | T], [State, H | T]) :-
	precedes(State, H).
insert_pq(State, [H | T], [H | Tnew]) :-
	( not(precedes(State, H)) ->
		insert_pq(State, T, Tnew)
	).

precedes(q(_, _, C1), q(_, _, C2)) :-
	C1 =< C2.

exist.
existt([],_,_) :- not(exist).
existt([p(X1,Y1,_,_)|Tail],X2,Y2) :-
	( X1 =:= X2,Y1 =:= Y2 ->
		exist
	;
		existt(Tail,X2,Y2)
	).
	
checkCost([],_,_,_).
checkCost([p(X1,Y1,Cost,_)|Tail],X2,Y2,Cost2) :-	
	( X1 =:= X2,Y1 =:= Y2 ->
		Cost > Cost2 
	; 
		checkCost(Tail,X2,Y2,Cost2)
	).


modify(State, [], [State]).
modify(p(NewX,NewY,NewC,NewM), [p(X,Y,C,M)| T], [p(NewX,NewY,NewC,NewM) | T]) :-
	prec(p(NewX,NewY,NewC,NewM), p(X,Y,C,M)).
modify(State, [H | T], [H | Tnew]) :-
	( not(prec(State, H)) ->
		modify(State, T, Tnew)
	).

prec(p(NewX,NewY,_,_), p(X,Y,_,_)) :-
	NewX =:= X, NewY =:= Y.
	
	
moveRight(_,M,_,M,Qlist,Plist,Qlist,Plist).	
moveRight(X,Y,Cost,M,Qlist,Plist,NewQlist,NewPlist) :- 
	Y =\= M,
	NewX is X,
	NewY is Y+1,
	NewCost is Cost+1,
	( not(existt(Plist,NewX,NewY)) ->
		insert_pq(q(NewX,NewY,NewCost),Qlist,NewQlist),
		append([p(NewX,NewY,NewCost,'R')],Plist,NewPlist)
	;
		(checkCost(Plist,NewX,NewY,NewCost) ->
			insert_pq(q(NewX,NewY,NewCost),Qlist,NewQlist),
			modify(p(NewX,NewY,NewCost,'R'),Plist,NewPlist)
		;
			moveRight(X,M,Cost,M,Qlist,Plist,NewQlist,NewPlist)
		)
	).
	
moveLeft(_,0,_,Qlist,Plist,Qlist,Plist).	
moveLeft(X,Y,Cost,Qlist,Plist,NewQlist,NewPlist) :- 
	Y =\= 0,
	NewX is X,
	NewY is Y-1,
	NewCost is Cost+2,
	( not(existt(Plist,NewX,NewY)) ->
		insert_pq(q(NewX,NewY,NewCost),Qlist,NewQlist),
		append([p(NewX,NewY,NewCost,'L')],Plist,NewPlist)
	;
		( checkCost(Plist,NewX,NewY,NewCost) ->
			insert_pq(q(NewX,NewY,NewCost),Qlist,NewQlist),
			modify(p(NewX,NewY,NewCost,'L'),Plist,NewPlist)
		;
			moveLeft(X,0,Cost,Qlist,Plist,NewQlist,NewPlist)
		)
	).
	
moveDown(N,_,_,N,Qlist,Plist,Qlist,Plist).	
moveDown(X,Y,Cost,N,Qlist,Plist,NewQlist,NewPlist) :- 
	X =\= N,
	NewX is X+1,
	NewY is Y,
	NewCost is Cost+1,
	( not(existt(Plist,NewX,NewY)) ->
		insert_pq(q(NewX,NewY,NewCost),Qlist,NewQlist),
		append([p(NewX,NewY,NewCost,'D')],Plist,NewPlist)
	;
		( checkCost(Plist,NewX,NewY,NewCost) ->
			insert_pq(q(NewX,NewY,NewCost),Qlist,NewQlist),
			modify(p(NewX,NewY,NewCost,'D'),Plist,NewPlist)
		;
			moveDown(N,Y,Cost,N,Qlist,Plist,NewQlist,NewPlist)
		)
	).
	
moveUp(0,_,_,Qlist,Plist,Qlist,Plist).	
moveUp(X,Y,Cost,Qlist,Plist,NewQlist,NewPlist) :- 
	X =\= 0,
	NewX is X-1,
	NewY is Y,
	NewCost is Cost+3,
	( not(existt(Plist,NewX,NewY)) ->
		insert_pq(q(NewX,NewY,NewCost),Qlist,NewQlist),
		append([p(NewX,NewY,NewCost,'U')],Plist,NewPlist)
	;
		( checkCost(Plist,NewX,NewY,NewCost) ->
			insert_pq(q(NewX,NewY,NewCost),Qlist,NewQlist),
			modify(p(NewX,NewY,NewCost,'U'),Plist,NewPlist)
		;
			moveUp(0,Y,Cost,Qlist,Plist,NewQlist,NewPlist)
		)
	).

findElementxyOfMap([],_,_,_).
findElementxyOfMap([mapp(X1,Y1,S1)|_],X2,Y2,S2) :-
	X1 =:= X2,
	Y1 =:= Y2,
	S2 = S1,
	findElementxyOfMap([],X2,Y2,S2).
findElementxyOfMap([mapp(X1,Y1,_)|TailMaplist],X2,Y2,S2) :-
	X1 =\= X2, Y1 =:= Y2,
	findElementxyOfMap(TailMaplist,X2,Y2,S2).
findElementxyOfMap([mapp(X1,Y1,_)|TailMaplist],X2,Y2,S2) :-
	Y1 =\= Y2, X1 =:= X2,
	findElementxyOfMap(TailMaplist,X2,Y2,S2).
findElementxyOfMap([mapp(X1,Y1,_)|TailMaplist],X2,Y2,S2) :-
	Y1 =\= Y2, X1 =\= X2,
	findElementxyOfMap(TailMaplist,X2,Y2,S2).
	
	
findStart([],_,_).
findStart([mapp(I,J,Symbol)|_],X,Y) :-
	Symbol = 'S',
	X is I,
	Y is J.
findStart([mapp(_,_,Symbol)|TailMaplist],X,Y) :-
	not(Symbol = 'S'),
	findStart(TailMaplist,X,Y).

findE(Maplist,X,Y) :-
	findElementxyOfMap(Maplist,X,Y,S),
	S = 'E'.
	
safe(Maplist,X,Y) :-
	findElementxyOfMap(Maplist,X,Y,S),
	not(S = 'X').
	
dijkstra([],Cost,Prevlist,_,_,[],Cost,Prevlist).	
dijkstra([MaplistH|MaplistT],_,[],N,M,[],NewCost,NewPrevlist) :-
	%write('initialize'),nl,
	findStart([MaplistH|MaplistT],X,Y),
	dijkstra([MaplistH|MaplistT],0,[p(X,Y,0,'n')],N,M,[q(X,Y,0)],NewCost,NewPrevlist).
dijkstra(Maplist,_,Prevlist,N,M,[q(X,Y,C)|_],NewCost,NewPrevlist) :-
	findE(Maplist,X,Y),
	%write('foundE'),nl,
	dijkstra([],C,Prevlist,N,M,[],NewCost,NewPrevlist).
dijkstra(Maplist,_,Prevlist,N,M,[q(X,Y,C)|TailQ],NewCost,NewPrevlist) :-
	%write('mphkame'),nl,
	not(findE(Maplist,X,Y)),
	/*write('vgazw to stoixeio '),write(X),write(' '),write(Y),nl,*/
	( safe(Maplist,X,Y) ->
		moveRight(X, Y,C,M,TailQ,Prevlist,NewQlist,NewPlist),
		%write('bghke apo moveRight me Qlist = '),nl,
		moveLeft(X,Y,C,NewQlist,NewPlist,NewQlist2,NewPlist2),
		%write('bghke apo moveLeft me Qlist= '),nl,
		moveDown(X,Y,C,N,NewQlist2,NewPlist2,NewQlist3,NewPlist3),
		%write('bghke apo moveDown me Qlist = '),nl,
		moveUp(X,Y,C,NewQlist3,NewPlist3,NewQlist4,NewPlist4),
		%write('bghke apo moveUp'),nl,
		%write('kai h nea Qlist einai '),nl,
		dijkstra(Maplist,C,NewPlist4,N,M,NewQlist4,NewCost,NewPrevlist)
	; 
		%write('brhka X!'),nl,
		dijkstra(Maplist,C,Prevlist,N,M,TailQ,NewCost,NewPrevlist)
	).
	
findEnd([],_,_).
findEnd([mapp(I,J,Symbol)|_],X,Y) :-
	Symbol = 'E',
	X is I,
	Y is J.
findEnd([mapp(_,_,Symbol)|TailMaplist],X,Y) :-
	not(Symbol = 'E'),
	findEnd(TailMaplist,X,Y).
	
findMovePrevlist([],_,_,_).	
findMovePrevlist([p(X1,Y1,_,M1)|_],X2,Y2,M2) :-
	X1 =:= X2,
	Y1 =:= Y2,
	M2 = M1,
	findMovePrevlist([],X2,Y2,M2).
findMovePrevlist([p(X1,Y1,_,_)|TailPrevlist],X2,Y2,M2) :-
	X1 =\= X2, Y1 =:= Y2,
	findMovePrevlist(TailPrevlist,X2,Y2,M2).
findMovePrevlist([p(X1,Y1,_,_)|Taillist],X2,Y2,M2) :-
	Y1 =\= Y2, X1 =:= X2,
	findMovePrevlist(Taillist,X2,Y2,M2).
findMovePrevlist([p(X1,Y1,_,_)|Taillist],X2,Y2,M2) :-
	Y1 =\= Y2, X1 =\= X2,
	findMovePrevlist(Taillist,X2,Y2,M2).
	
findPath([],_,_,Solution,Solution).
findPath([Head|Tail],X,Y,Path,Solution) :-
	findMovePrevlist([Head|Tail],X,Y,S),
	( S = 'U' ->
		Nx is X+1,
		append([u],Path,NewSol),
		findPath([Head|Tail],Nx,Y,NewSol,Solution)
	; 
		( S = 'D' ->
			Nx is X-1,
			append([d],Path,NewSol),
			findPath([Head|Tail],Nx,Y,NewSol,Solution)
		;
			( S = 'R' ->
				Ny is Y-1,
				append([r],Path,NewSol),
				findPath([Head|Tail],X,Ny,NewSol,Solution)
			;
				( S = 'L' ->
					Ny is Y+1,
					append([l],Path,NewSol),
					findPath([Head|Tail],X,Ny,NewSol,Solution)
				; 
					findPath([],X,Y,Path,Solution)
				)
			)
		)
	).

	
	
moredeli(File,Cost,Solution) :-
	read_input(File,N,M,Maplist),
	NN is N-1,
	MM is M-1,
	dijkstra(Maplist,0,[],NN,MM,[],Cost,Prevlist),
	%write(Prevlist),
	findEnd(Maplist,Xend,Yend),
	findPath(Prevlist,Xend,Yend,[],Solution).


takeHead([],NewHead,NewHead).
takeHead([Head|Tail],_,NewHead) :-
	takeHead(Tail,Head,NewHead).
	
createALine([],_,M,M,NewList,NewList).
createALine([Head|Tail],I,J,M,List,NewList) :-
	append(List,[mapp(I,J,Head)],NList),
	NJ is J+1,
	createALine(Tail,I,NJ,M,NList,NewList).
	
	
createMaplist([],N,N,_,_,Maplist,Maplist).
createMaplist([Head|Tail],0,N,0,M,[],Maplist) :-
	takeHead(Head,[],NewHead),
	createALine(NewHead,0,0,M,[],List),
	createMaplist(Tail,1,N,0,M,List,Maplist).
createMaplist([Head|Tail],I,N,_,M,[Head1|Tail1],Maplist) :-
	takeHead(Head,[],NewHead),
	createALine(NewHead,I,0,M,[Head1|Tail1],Mlist),
	NI is I+1,
	createMaplist(Tail,NI,N,0,M,Mlist,Maplist).
	
read_input(File,N,M,List) :-
    open(File, read, Stream),
    read_line(Stream, NList),
	createMaplist(NList,0,N,0,M,[],List).

read_line(Stream,[]):-
	at_end_of_stream(Stream).
read_line(Stream, [X|L]) :-
    \+ at_end_of_stream(Stream),
	read_line_to_codes(Stream, Line),
    ( Line = [] -> X = []
    ; atom_codes(A, Line),
      maplist(atom_chars, [A], X),
	  read_line(Stream,L)
    ).

