read_input(File, ListA, ListB, ListC) :-
    open(File, read, Stream),
    read_line(Stream, ListA),
	%write('take input'),nl,
    read_line(Stream, ListB),
	%write('take input'),nl,
	read_line(Stream, ListC).

/*
 * An auxiliary predicate that reads a line and returns the list of
 * integers that the line contains.
 */
 
read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
    ( Line = [] -> List = []
    ; atom_codes(A, Line),
      atomic_list_concat(As, ' ', A),
      maplist(atom_number, As, List)
    ).

	
take_NKB([],N,K,B,_,N,K,B).
take_NKB([H|T],_,K1,B1,I,N,K,B) :-
	I = 0,
	N2 is H,
	I1 is I+1,
	take_NKB(T,N2,K1,B1,I1,N,K,B).
take_NKB([H|T],N1,_,B1,I,N,K,B) :-
	I =1,
	K2 is H,
	I1 is I+1,
	take_NKB(T,N1,K2,B1,I1,N,K,B).
take_NKB([H|_],N1,K1,_,I,N,K,B) :-
	I=2,
	B2 is H,
	I1 is I+1,
	take_NKB([],N1,K1,B2,I1,N,K,B).
	
checkBroken(B,_,_,B,[]).
checkBroken(I,N,N2,B,[H|T]) :-
	I<B,
	G is N2-H,
	N =\= G,
	I1 is I+1,
	checkBroken(I1,N,N2,B,T).
	
	
count(0,0,[],_,_,_,Res,Res).
count(N,_,[_|_],N2,B,BrokenList,_,Res):-
	N =< 1,
	(checkBroken(0,N,N2,B,BrokenList) ->
		count(0,0,[],N2,B,BrokenList,N,Res)
	;
		count(0,0,[],N2,B,BrokenList,0,Res)
	).
count(N,K,[H|T],N2,B,BrokenList,_,Res):-
	N > 1,
	/*write('Count N= '),write(N),nl,*/
	( checkBroken(0,N,N2,B,BrokenList) ->
		forloop(0,N,K,[H|T],[H|T],N2,B,BrokenList,0,Ges),
		R is mod(Ges,1000000009),
		count(0,0,[],N2,B,BrokenList,R,Res)
	;
		count(0,0,[],N2,B,BrokenList,0,Res)
	).
	
forloop(I,N,K,[H|T],StepList,N2,B,BrokenList,R,Res):-
	I<K,
	( H =< N ->
		N1 is N-H,
		count(N1,K,StepList,N2,B,BrokenList,0,Res1),
		R1 is R+Res1,
		I1 is I+1,
		forloop(I1,N,K,T,StepList,N2,B,BrokenList,R1,Res)
	;
		I1 is I+1,
		forloop(I1,N,K,T,StepList,N2,B,BrokenList,R,Res)
	).	
forloop(I,_,K,_,[_|_],_,_,_,R,Res):-
	I>=K,
	Res is R.

	
hopping(File,Solution) :-
	read_input(File,Line,StepList,BrokenList),
	take_NKB(Line,0,0,0,0,N,K,B),
	count(N,K,StepList,N,B,BrokenList,0,Solution).
