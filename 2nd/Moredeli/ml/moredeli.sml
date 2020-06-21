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
		
fun WriteToQlist(nil,i,j,cost,new,true) = new
|   WriteToQlist(nil,i,j,cost,new,false) = WriteToQlist(nil,i,j,cost,new@[(i,j,cost)],true)
|   WriteToQlist(Qlist,i,j,cost,new,bool) = 
		if (bool = false) then
			if (ThirdPartofTuple(hd Qlist) < cost) then WriteToQlist(tl Qlist,i,j,cost,new@[hd Qlist],false)
			else WriteToQlist(Qlist,i,j,cost,new@[(i,j,cost)],true)
		else WriteToQlist(tl Qlist,i,j,cost,new@[hd Qlist],true);

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
		
fun FindNextNode (Plist,Qlist,Prevlist,i,j,cost,m) = 
	if (m = "U") then
		if (secondPartofTuple(FindElementijOfList(i,j,Prevlist))> cost+3 orelse firstPartofTuple(FindElementijOfList(i,j,Prevlist)) = "-") then
			(WriteToQlist(Qlist,i,j,cost+3,nil,false),WriteToPrevious(Prevlist,i,j,m,cost+3,nil,false))
		else (Qlist,Prevlist)
	else
		if (m = "L") then 
			if (secondPartofTuple(FindElementijOfList(i,j,Prevlist))> cost+2 orelse firstPartofTuple(FindElementijOfList(i,j,Prevlist)) = "-") then
				(WriteToQlist(Qlist,i,j,cost+2,nil,false),WriteToPrevious(Prevlist,i,j,m,cost+2,nil,false))
			else (Qlist,Prevlist)
		else 
			if (secondPartofTuple(FindElementijOfList(i,j,Prevlist))> cost+1 orelse firstPartofTuple(FindElementijOfList(i,j,Prevlist)) = "-") then
				(WriteToQlist(Qlist,i,j,cost+1,nil,false),WriteToPrevious(Prevlist,i,j,m,cost+1,nil,false))
			else (Qlist,Prevlist);
		
	
		
fun Function (Plist,Qlist,Prevlist,i,j,cost,N,M,true,true,true,true) = (Qlist,Prevlist)
|	Function (Plist,Qlist,Prevlist,i,j,cost,N,M,checkUp,checkDown,checkLeft,checkRight)=
	if (checkUp = false) then 
		if (i<>0) then 
			if (FindElementijOfList(i-1,j,Plist) <> "X") then
				let
					val tupleUp = FindNextNode(Plist,Qlist,Prevlist,i-1,j,cost,"U")
					val QlistUp = firstPartofTuple(tupleUp)
					val PrevlistUp = secondPartofTuple(tupleUp)
				in
					Function(Plist,QlistUp,PrevlistUp,i,j,cost,N,M,true,checkDown,checkLeft,checkRight)
				end
			else Function(Plist,Qlist,Prevlist,i,j,cost,N,M,true,checkDown,checkLeft,checkRight)
		else Function(Plist,Qlist,Prevlist,i,j,cost,N,M,true,checkDown,checkLeft,checkRight)

	else 
		if (checkDown = false) then
			if (i <> N-1) then 
				if (FindElementijOfList(i+1,j,Plist) <> "X") then
					let
						val tupleDown = FindNextNode(Plist,Qlist,Prevlist,i+1,j,cost,"D")
						val QlistD = firstPartofTuple(tupleDown)
						val PrevlistD = secondPartofTuple(tupleDown)
					in
						Function(Plist,QlistD,PrevlistD,i,j,cost,N,M,checkUp,true,checkLeft,checkRight)	
					end
				else Function(Plist,Qlist,Prevlist,i,j,cost,N,M,checkUp,true,checkLeft,checkRight)
			else Function(Plist,Qlist,Prevlist,i,j,cost,N,M,checkUp,true,checkLeft,checkRight)
		else
			if (checkLeft = false) then
				if (j<>0) then
					if (FindElementijOfList(i,j-1,Plist) <> "X") then
						let
							val tupleL = FindNextNode(Plist,Qlist,Prevlist,i,j-1,cost,"L")
							val QlistL = firstPartofTuple(tupleL)
							val PrevlistL = secondPartofTuple(tupleL)
						in
							Function(Plist,QlistL,PrevlistL,i,j,cost,N,M,checkUp,checkDown,true,checkRight)
						end
					else Function(Plist,Qlist,Prevlist,i,j,cost,N,M,checkUp,checkDown,true,checkRight)
				else Function(Plist,Qlist,Prevlist,i,j,cost,N,M,checkUp,checkDown,true,checkRight)
			else
				if (checkRight = false) then
					if (j<>M-1) then
						if (FindElementijOfList(i,j+1,Plist)<>"X") then
							let
								val tupleR = FindNextNode(Plist,Qlist,Prevlist,i,j+1,cost,"R")
								val	QlistR = firstPartofTuple(tupleR)
								val PrevlistR = secondPartofTuple(tupleR)
							in
								Function(Plist,QlistR,PrevlistR,i,j,cost,N,M,checkUp,checkDown,checkLeft,true)
							end
						else Function(Plist,Qlist,Prevlist,i,j,cost,N,M,checkUp,checkDown,checkLeft,true)
					else Function(Plist,Qlist,Prevlist,i,j,cost,N,M,checkUp,checkDown,checkLeft,true)
				else Function (Plist,Qlist,Prevlist,i,j,cost,N,M,checkUp,checkDown,checkLeft,checkRight);

	fun FindSolution (nil,false,Plist,Solution,Prevlist,N,M) = (0,Prevlist)
|	FindSolution (Qlist,true,Plist,Solution,Prevlist,N,M) = (Solution,Prevlist)
|	FindSolution (Qlist,false,Plist,Solution,Prevlist,N,M) = 
		let
			val n = hd Qlist
			val newQlist = tl Qlist
			val i = FirstPartofTuple(n)
			val j = SecondPartofTuple(n)
			val cost = ThirdPartofTuple(n) 
		in
			if (FindElementijOfList(i,j,Plist) = "E" ) then FindSolution(newQlist,true,Plist,cost,Prevlist,N,M)
			else
				if (FindElementijOfList(i,j,Plist) <> "X") then 
					let
						val tuple = Function(Plist,newQlist,Prevlist,i,j,cost,N,M,false,false,false,false)
						val newnewQlist = firstPartofTuple(tuple)
						val newPrevlist = secondPartofTuple(tuple)
					in
						FindSolution (newnewQlist,false,Plist,cost,newPrevlist,N,M)
					end
				else FindSolution(newQlist,false,Plist,Solution,Prevlist,N,M)
		end;
		
fun FindPath (Prevlist,i,j,new) =  
	let 
		val e = FindElementijOfList(i,j,Prevlist)
		val m = firstPartofTuple(e)
	in 
		if (m = "n") then new
		else 
			if (m = "U") then FindPath (Prevlist,i+1,j,new@[m])
			else 
				if (m = "D") then FindPath (Prevlist,i-1,j,new@[m])
				else 
					if (m = "R") then FindPath (Prevlist,i,j-1,new@[m])
					else FindPath (Prevlist,i,j+1,new@[m])
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
		val Prevlist = CreateArray(N,M,nil)		(*Δημιουργω τις PrevPizza,PrevNoPizza που είναι λιστες απο λιστες με τουπλες (m,cost)*)
		val StartTuple = Find_S (0,0,"S",Plist,false)	(*Βρίσκω που βρίσκεται το "S"*)
		val starti = firstPartofTuple(StartTuple)
		val startj = secondPartofTuple (StartTuple)
		val EndTuple = Find_S (0,0,"E",Plist,false)	(*Βρίσκω που βρίσκεται το "Ε"*)
		val endi = firstPartofTuple(EndTuple)
		val endj = secondPartofTuple(EndTuple)
		val Q = [(starti,startj,0)]			(*Δημιουργω την Qlist με αρχικό κόμβο (i=0,j=0,cost=0,pizza=true)*)
		val newPrevlist = WriteToPrevious(Prevlist,starti,startj,"n",0,nil,false)	(*Γραφω στην PrevPizza στο (i=0,j=0) την τιμή ("n",0)*)
		
		val Solution = FindSolution(Q,false,Plist,0,newPrevlist,N,M)
		val ListofPath = reverse(FindPath (secondPartofTuple(Solution),endi,endj,nil))
		val Sol = (firstPartofTuple(Solution),stringListtoString(ListofPath))
	in
		Sol
	end;