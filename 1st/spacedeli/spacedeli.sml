fun reverse nil = nil 
  | reverse(first :: rest) = reverse rest @ [first] ;
  
fun firstPartofTuple (a,b) =a;
fun secondPartofTuple (a,b)=b;

fun FirstPartofTuple (a,b,c) = a;
fun SecondPartofTuple(a,b,c)=b;
fun ThirdPartofTuple (a,b,c)=c;

fun first_PartofTuple (a,b,c,d) = a;
fun second_PartofTuple (a,b,c,d) = b;
fun third_PartofTuple (a,b,c,d) = c;
fun fourth_PartofTuple (a,b,c,d) = d;

fun stringListtoString (nil) = ""
|   stringListtoString (x) =  (hd x) ^ stringListtoString (tl x);

fun CharListToStringList(bool,new,M,nil) = (new,M,nil)
|   CharListToStringList(bool,new,M,oldlist)=
		if (hd oldlist <> #"\n") then CharListToStringList(true,new@[implode[hd oldlist]],M+1,tl oldlist)
		else (new,M,tl oldlist);
		
fun StringList2D (nil,N,M,new) = (N,M,new)
|   StringList2D (charlist,N,M,new)=
		let
			val tuple = CharListToStringList(true,nil,0,charlist)
			val newlist = FirstPartofTuple(tuple)
			val m = SecondPartofTuple(tuple)
			val oldlist = ThirdPartofTuple(tuple)
		in
			StringList2D(oldlist,N+1,m,new@[newlist])
		end;
		
fun FindTheElementj (0,list) = (0,hd list)
|	FindTheElementj (j,list) = FindTheElementj(j-1,tl list);
		
fun FindElementijOfList (0,0,Plist)= hd (hd Plist)
|   FindElementijOfList (i,j,Plist) =
		if (i<>0) then FindElementijOfList(i-1,j,tl Plist)
		else secondPartofTuple(FindTheElementj(j,hd Plist));
		
fun CreateList (0,new) = new
|   CreateList (j,new) = CreateList(j-1,new@[("-",1000)]);
		
fun CreateArray (0,M,new) = new
|   CreateArray (N,M,new) = CreateArray(N-1,M,new@[CreateList(M,nil)]);
		
fun FindS (nil,j,m) = (false,j)
|	FindS (list,j,m) = 
		if (hd list <> m) then FindS(tl list,j+1,m)
		else (true,j);
		
fun Find_S (starti,startj,m,Plist,true)= (starti,startj)
|	Find_S (starti,startj,m,Plist,false)= 
		if (firstPartofTuple(FindS(hd Plist,0,m))) then Find_S(starti,secondPartofTuple(FindS(hd Plist,0,m)),m,Plist,true)
		else Find_S(starti+1,startj,m,tl Plist,false);
		
fun WriteToQlist(nil,i,j,cost,pizza,new,true) = new
|   WriteToQlist(nil,i,j,cost,pizza,new,false) = WriteToQlist(nil,i,j,cost,pizza,new@[(i,j,cost,pizza)],true)
|   WriteToQlist(Qlist,i,j,cost,pizza,new,bool) = 
		if (bool = false) then
			if (third_PartofTuple(hd Qlist) < cost) then WriteToQlist(tl Qlist,i,j,cost,pizza,new@[hd Qlist],false)
			else WriteToQlist(Qlist,i,j,cost,pizza,new@[(i,j,cost,pizza)],true)
		else WriteToQlist(tl Qlist,i,j,cost,pizza,new@[hd Qlist],true);

fun WriteToPreviousj(nil,j,m,cost,new,true) = new
|   WriteToPreviousj(list,j,m,cost,new,bool)=
		if (bool = false) then 
			if (j<>0) then WriteToPreviousj(tl list,j-1,m,cost,new@[hd list],false)
			else WriteToPreviousj(tl list,j,m,cost,new@[(m,cost)],true)
		else WriteToPreviousj(tl list,j,m,cost,new@[hd list],true);
		
fun WriteToPrevious(nil,i,j,m,cost,new,true) = new
|   WriteToPrevious(list,i,j,m,cost,new,bool) =
		if (bool = false) then
			if (i<>0) then WriteToPrevious (tl list,i-1,j,m,cost,new@[hd list],false)
			else 
				let
					val p = hd list
					val pnew = WriteToPreviousj(p,j,m,cost,nil,false)
				in
					WriteToPrevious(tl list,i,j,m,cost,new@[pnew],true)
				end
		else WriteToPrevious (tl list,i,j,m,cost,new@[hd list],true);
		
fun FindNextNode (Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,m) = 
	if (pizza) then 
		if (m = "W") then
			if ((secondPartofTuple(FindElementijOfList(i,j,PrevPizza))>(cost+1)) orelse (firstPartofTuple(FindElementijOfList(i,j,PrevPizza)) = "-")) then
				(WriteToQlist(Qlist,i,j,cost+1,true,nil,false),WriteToPrevious(PrevPizza,i,j,m,cost+1,nil,false),PrevNoPizza)
			else (Qlist,PrevPizza,PrevNoPizza)
		else
			if (secondPartofTuple(FindElementijOfList(i,j,PrevPizza))> cost+2 orelse firstPartofTuple(FindElementijOfList(i,j,PrevPizza)) = "-") then
				(WriteToQlist(Qlist,i,j,cost+2,true,nil,false),WriteToPrevious(PrevPizza,i,j,m,cost+2,nil,false),PrevNoPizza)
			else (Qlist,PrevPizza,PrevNoPizza)
	else
		if (secondPartofTuple(FindElementijOfList(i,j,PrevNoPizza))> cost+1 orelse firstPartofTuple(FindElementijOfList(i,j,PrevNoPizza)) = "-") then
			(WriteToQlist(Qlist,i,j,cost+1,false,nil,false),PrevPizza,WriteToPrevious(PrevNoPizza,i,j,m,cost+1,nil,false))
		else (Qlist,PrevPizza,PrevNoPizza);
		
fun Function (Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,true,true,true,true,true) = (Qlist,PrevPizza,PrevNoPizza)
|	Function (Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,checkDown,checkLeft,checkRight,checkW)=
	if (checkUp = false) then 
		if (i<>0) then 
			if (FindElementijOfList(i-1,j,Plist) <> "X") then
				let
					val tupleUp = FindNextNode(Plist,Qlist,PrevPizza,PrevNoPizza,i-1,j,cost,pizza,"U")
					val QlistUp = FirstPartofTuple(tupleUp)
					val PrevPizzaUp = SecondPartofTuple(tupleUp)
					val PrevNoPizzaUp = ThirdPartofTuple(tupleUp)
				in
					Function(Plist,QlistUp,PrevPizzaUp,PrevNoPizzaUp,i,j,cost,pizza,N,M,true,checkDown,checkLeft,checkRight,checkW)
				end
			else Function(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,true,checkDown,checkLeft,checkRight,checkW)
		else Function(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,true,checkDown,checkLeft,checkRight,checkW)

	else 
		if (checkDown = false) then
			if (i <> N-1) then 
				if (FindElementijOfList(i+1,j,Plist) <> "X") then
					let
						val tupleDown = FindNextNode(Plist,Qlist,PrevPizza,PrevNoPizza,i+1,j,cost,pizza,"D")
						val QlistD = FirstPartofTuple(tupleDown)
						val PrevPizzaD = SecondPartofTuple(tupleDown)
						val PrevNoPizzaD = ThirdPartofTuple(tupleDown)
					in
						Function(Plist,QlistD,PrevPizzaD,PrevNoPizzaD,i,j,cost,pizza,N,M,checkUp,true,checkLeft,checkRight,checkW)	
					end
				else Function(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,true,checkLeft,checkRight,checkW)
			else Function(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,true,checkLeft,checkRight,checkW)
		else
			if (checkLeft = false) then
				if (j<>0) then
					if (FindElementijOfList(i,j-1,Plist) <> "X") then
						let
							val tupleL = FindNextNode(Plist,Qlist,PrevPizza,PrevNoPizza,i,j-1,cost,pizza,"L")
							val QlistL = FirstPartofTuple(tupleL)
							val PrevPizzaL = SecondPartofTuple(tupleL)
							val PrevNoPizzaL = ThirdPartofTuple(tupleL)
						in
							Function(Plist,QlistL,PrevPizzaL,PrevNoPizzaL,i,j,cost,pizza,N,M,checkUp,checkDown,true,checkRight,checkW)
						end
					else Function(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,checkDown,true,checkRight,checkW)
				else Function(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,checkDown,true,checkRight,checkW)
			else
				if (checkRight = false) then
					if (j<>M-1) then
						if (FindElementijOfList(i,j+1,Plist)<>"X") then
							let
								val tupleR = FindNextNode(Plist,Qlist,PrevPizza,PrevNoPizza,i,j+1,cost,pizza,"R")
								val	QlistR = FirstPartofTuple(tupleR)
								val PrevPizzaR = SecondPartofTuple(tupleR)
								val PrevNoPizzaR =ThirdPartofTuple(tupleR)
							in
								Function(Plist,QlistR,PrevPizzaR,PrevNoPizzaR,i,j,cost,pizza,N,M,checkUp,checkDown,checkLeft,true,checkW)
							end
						else Function(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,checkDown,checkLeft,true,checkW)
					else Function(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,checkDown,checkLeft,true,checkW)
				else 
					if (checkW = false) then
						if (FindElementijOfList(i,j,Plist) = "W") then
							let
								val tupleW = FindNextNode(Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,not pizza,"W")
								val QlistW = FirstPartofTuple(tupleW)
								val PrevPizzaW = SecondPartofTuple(tupleW)
								val PrevNoPizzaW =ThirdPartofTuple(tupleW)
							in
								Function(Plist,QlistW,PrevPizzaW,PrevNoPizzaW,i,j,cost,pizza,N,M,checkUp,checkDown,checkLeft,checkRight,true)
							end
						else Function (Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,checkDown,checkLeft,checkRight,true)
					else Function (Plist,Qlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,checkUp,checkDown,checkLeft,checkRight,checkW);

	fun FindSolution (nil,false,Plist,Solution,PrevPizza,PrevNoPizza,N,M) = (0,PrevPizza,PrevNoPizza)
|	FindSolution (Qlist,true,Plist,Solution,PrevPizza,PrevNoPizza,N,M) = (Solution,PrevPizza,PrevNoPizza)
|	FindSolution (Qlist,false,Plist,Solution,PrevPizza,PrevNoPizza,N,M) = 
		let
			val n = hd Qlist
			val newQlist = tl Qlist
			val i = first_PartofTuple(n)
			val j = second_PartofTuple(n)
			val cost = third_PartofTuple(n) 
			val pizza = fourth_PartofTuple(n)
		in
			if (FindElementijOfList(i,j,Plist) = "E" andalso pizza ) then FindSolution(newQlist,true,Plist,cost,PrevPizza,PrevNoPizza,N,M)
			else
				if (FindElementijOfList(i,j,Plist) <> "X") then 
					let
						val tuple = Function(Plist,newQlist,PrevPizza,PrevNoPizza,i,j,cost,pizza,N,M,false,false,false,false,false)
						val newnewQlist = FirstPartofTuple(tuple)
						val newPrevPizza = SecondPartofTuple(tuple)
						val newPrevNoPizza = ThirdPartofTuple(tuple)
					in
						FindSolution (newnewQlist,false,Plist,cost,newPrevPizza,newPrevNoPizza,N,M)
					end
				else FindSolution(newQlist,false,Plist,Solution,PrevPizza,PrevNoPizza,N,M)
		end;
		
fun FindPath (PrevPizza,PrevNoPizza,i,j,pizza,new) = 
	if (pizza) then 
		let 
			val e = FindElementijOfList(i,j,PrevPizza)
			val m = firstPartofTuple(e)
		in 
		if (m = "n") then new
		else 
			if (m = "U") then FindPath (PrevPizza,PrevNoPizza,i+1,j,true,new@[m])
			else 
				if (m = "D") then FindPath (PrevPizza,PrevNoPizza,i-1,j,true,new@[m])
				else 
					if (m = "R") then FindPath (PrevPizza,PrevNoPizza,i,j-1,true,new@[m])
					else 
						if (m = "L") then FindPath (PrevPizza,PrevNoPizza,i,j+1,true,new@[m])
						else FindPath (PrevPizza,PrevNoPizza,i,j,false,new@[m])
		end
	else
		let 
			val e = FindElementijOfList(i,j,PrevNoPizza)
			val m = firstPartofTuple(e)
		in 
			if (m = "U") then FindPath (PrevPizza,PrevNoPizza,i+1,j,false,new@[m])
			else 
				if (m = "D") then FindPath (PrevPizza,PrevNoPizza,i-1,j,false,new@[m])
				else 
					if (m = "R") then FindPath (PrevPizza,PrevNoPizza,i,j-1,false,new@[m])
					else 
						if (m = "L") then FindPath (PrevPizza,PrevNoPizza,i,j+1,false,new@[m])
						else FindPath (PrevPizza,PrevNoPizza,i,j,true,new@[m])
		end;
	
fun spacedeli (infile)=
	let
		val ins = TextIO.openIn infile
		val strinput =TextIO.inputAll(ins)		(*Εχω string με ολόκληρη την είσοδο*)
		val charlist = explode(strinput)		(*Εχω λιστα απο char όμως είναι της μορφής #"."*)
		val tuple = StringList2D(charlist,0,0,nil)	(*Δημιουργω την Plist που είναι λιστα απο λιστες με strings*)
		val N = FirstPartofTuple(tuple)
		val M = SecondPartofTuple(tuple)
		val Plist = ThirdPartofTuple(tuple)
		val PrevPizza = CreateArray(N,M,nil)		(*Δημιουργω τις PrevPizza,PrevNoPizza που είναι λιστες απο λιστες με τουπλες (m,cost)*)
		val PrevNoPizza = CreateArray (N,M,nil)
		val StartTuple = Find_S (0,0,"S",Plist,false)	(*Βρίσκω που βρίσκεται το "S"*)
		val starti = firstPartofTuple(StartTuple)
		val startj = secondPartofTuple (StartTuple)
		val EndTuple = Find_S (0,0,"E",Plist,false)	(*Βρίσκω που βρίσκεται το "Ε"*)
		val endi = firstPartofTuple(EndTuple)
		val endj = secondPartofTuple(EndTuple)
		val Q = [(starti,startj,0,true)]			(*Δημιουργω την Qlist με αρχικό κόμβο (i=0,j=0,cost=0,pizza=true)*)
		val newPrevPizza = WriteToPrevious(PrevPizza,starti,startj,"n",0,nil,false)	(*Γραφω στην PrevPizza στο (i=0,j=0) την τιμή ("n",0)*)
		(*val S = Function(Plist,tl Q,newPrevPizza,PrevNoPizza,0,0,0,true,N,M,false,false,false,false,false)
		val S1 = Function (Plist,tl(FirstPartofTuple(S)),SecondPartofTuple(S),ThirdPartofTuple(S),0,1,2,true,N,M,false,false,false,false,false)
		val S2 = Function (Plist,tl(FirstPartofTuple(S1)),SecondPartofTuple(S1),ThirdPartofTuple(S1),1,0,2,true,N,M,false,false,false,false,false)
		val S3 = Function (Plist,tl(FirstPartofTuple(S2)),SecondPartofTuple(S2),ThirdPartofTuple(S2),2,0,4,true,N,M,false,false,false,false,false)
		val S4 = Function (Plist,tl(FirstPartofTuple(S3)),SecondPartofTuple(S3),ThirdPartofTuple(S3),0,2,4,true,N,M,false,false,false,false,false)
		val S5 = Function (Plist,tl(FirstPartofTuple(S4)),SecondPartofTuple(S4),ThirdPartofTuple(S4),1,1,4,true,N,M,false,false,false,false,false)
		val S6 = Function (Plist,tl(FirstPartofTuple(S5)),SecondPartofTuple(S5),ThirdPartofTuple(S5),2,0,5,false,N,M,false,false,false,false,false)
		val S7 = Function (Plist,tl(FirstPartofTuple(S6)),SecondPartofTuple(S6),ThirdPartofTuple(S6),2,1,6,false,N,M,false,false,false,false,false)
		val S8 = Function (Plist,tl(FirstPartofTuple(S7)),SecondPartofTuple(S7),ThirdPartofTuple(S7),3,0,6,false,N,M,false,false,false,false,false)
		val S9 = Function (Plist,tl(FirstPartofTuple(S8)),SecondPartofTuple(S8),ThirdPartofTuple(S8),1,0,6,false,N,M,false,false,false,false,false)
		val S10 = Function (Plist,tl(FirstPartofTuple(S9)),SecondPartofTuple(S9),ThirdPartofTuple(S9),0,3,6,true,N,M,false,false,false,false,false)
		val S11 = Function (Plist,tl(FirstPartofTuple(S10)),SecondPartofTuple(S10),ThirdPartofTuple(S10),1,2,6,true,N,M,false,false,false,false,false)
		val S12 = Function (Plist,tl(FirstPartofTuple(S11)),SecondPartofTuple(S11),ThirdPartofTuple(S11),2,1,6,true,N,M,false,false,false,false,false)
		val S13 = Function (Plist,tl(FirstPartofTuple(S12)),SecondPartofTuple(S12),ThirdPartofTuple(S12),3,0,6,true,N,M,false,false,false,false,false)
		val S14 = Function (Plist,tl(FirstPartofTuple(S13)),SecondPartofTuple(S13),ThirdPartofTuple(S13),0,0,7,false,N,M,false,false,false,false,false)
		val S15 = Function (Plist,tl(FirstPartofTuple(S14)),SecondPartofTuple(S14),ThirdPartofTuple(S14),3,1,7,false,N,M,false,false,false,false,false)
		val S16 = Function (Plist,tl(FirstPartofTuple(S15)),SecondPartofTuple(S15),ThirdPartofTuple(S15),1,1,7,false,N,M,false,false,false,false,false)
		val S17 = Function (Plist,tl(FirstPartofTuple(S16)),SecondPartofTuple(S16),ThirdPartofTuple(S16),1,2,8,false,N,M,false,false,false,false,false)
		val S18 = Function (Plist,tl(FirstPartofTuple(S17)),SecondPartofTuple(S17),ThirdPartofTuple(S17),0,1,8,false,N,M,false,false,false,false,false)
		val S19 = Function (Plist,tl(FirstPartofTuple(S18)),SecondPartofTuple(S18),ThirdPartofTuple(S18),3,1,8,true,N,M,false,false,false,false,false)
		val S20 = Function (Plist,tl(FirstPartofTuple(S19)),SecondPartofTuple(S19),ThirdPartofTuple(S19),1,3,8,true,N,M,false,false,false,false,false)
		val S21 = Function (Plist,tl(FirstPartofTuple(S20)),SecondPartofTuple(S20),ThirdPartofTuple(S20),1,3,9,false,N,M,false,false,false,false,false)
		val S22 = Function (Plist,tl(FirstPartofTuple(S21)),SecondPartofTuple(S21),ThirdPartofTuple(S21),0,2,9,false,N,M,false,false,false,false,false)
		val S23 = Function (Plist,tl(FirstPartofTuple(S22)),SecondPartofTuple(S22),ThirdPartofTuple(S22),2,3,10,false,N,M,false,false,false,false,false)*)
		
		val Solution = FindSolution(Q,false,Plist,0,newPrevPizza,PrevNoPizza,N,M)
		val ListofPath = reverse(FindPath (SecondPartofTuple(Solution),ThirdPartofTuple(Solution),endi,endj,true,nil))
		val newSolution = Int.toString(FirstPartofTuple(Solution)) ^" "
		val Sol = newSolution^stringListtoString(ListofPath)
	in
		Sol
	end;