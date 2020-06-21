/* [ski(1,42),
 *ski(2,41), 
 *ski(3,40),
 *ski(4,39),
 *ski(5,38),
 *ski(6,37)].*/
 
read_input(File, N, List) :-
    open(File, read, Stream),
    read_line(Stream, N),
    read_line(Stream, List).

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


maxLength(SofarX, Length, SofarY) :- 
	SofarX < Length,
	SofarY is Length.
	% write('is not wrong\n').	
	
maxLength(SofarX, Length, SofarY) :- 
	SofarX >= Length,
	SofarY is SofarX.


trip([],_,Sofar, Sofar).
trip([_| OthersI], [], Sofar, Result) :- trip(OthersI, OthersI, Sofar, Result).  
trip([ski(I, Yi) | OthersI], [ski(J, Yj) | OthersJ], SofarX, Result) :-
	Yi =< Yj,
	% write('Mesa 1\n'),
	% write('Ski('),write(I),write(','),write(Yi), write(')'), nl,
	Len is J-I,
	% write(Len), nl,
	maxLength(SofarX,Len, SofarY),
	% write(SofarY), nl,
	trip([ski(I, Yi) | OthersI], OthersJ, SofarY, Result).
trip([ski(I, Yi) | OthersI], [ski(_, Yj) | OthersJ], SofarX, Result) :-
	Yi > Yj,
	% write('Mesa 2\n'),
	% write(SofarX), nl,
	trip([ski(I, Yi) | OthersI], OthersJ, SofarX, Result).


skiii(X, Result) :-
	Sofar is 0,
	trip(X, X, Sofar, Result).
	% write(Result), nl.
	
convert([],_,Skilist,Skilist).
convert([Head|Tail],0,[],Skilist) :-
	%write('fisrt!')
	convert(Tail,0,[ski(0,Head)],Skilist).
convert([Head|Tail],C,[HeadList|TailList],Skilist) :-
	Nc is C+1,
	append([HeadList|TailList],[ski(Nc,Head)],NewList),
	%write('second!')
	convert(Tail,Nc,NewList,Skilist).
	
skitrip(File,Answer) :-
	read_input(File,_,List),
	convert(List,0,[],Skilist),
	skiii(Skilist,Answer).



	
