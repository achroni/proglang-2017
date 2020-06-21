fun reverse nil = nil 
  | reverse(first :: rest) = reverse rest @ [first] ;

fun stringtointlist x = map (fn x => Char.ord x -48) (explode x);

fun makeIntegers(x) = case (Int.fromString(x)) of 
				SOME a => a |
                NONE => 0 ;


fun intListtoString (nil) = ""
|   intListtoString (x) = 
    if (tl x = nil) then if (hd x = ~1) then "| "^intListtoString (tl x)
                         else Int.toString (hd x)^ intListtoString (tl x)
    else
       if (hd x = ~1) then "| "^intListtoString (tl x)
       else Int.toString (hd x)^" "^ intListtoString (tl x);
	   
fun searchforspace(nil,station) = (nil,station)
|   searchforspace(list,station) = 
    if ((hd list = #" ") orelse (hd list = #"\n")) then (tl list, station)
    else searchforspace(tl list,station^implode([hd list]));
	
fun FirstPartofTuple (a,b) = a;
fun SecondPartofTuple(a,b)=b;

fun firstPartofTuple(a,b,c) = a;
fun secondPartofTuple(a,b,c)=b;
fun thirdPartofTuple(a,b,c)=c;

fun makenewstringlist (nil,new) = new
|   makenewstringlist (list,new) =
    if (tl list = nil) then if ((hd list<> #" ") orelse (hd list <> #"\n")) then makenewstringlist(tl list,new@[implode[hd list]])
                                else makenewstringlist(tl list,new)
    else
       if ((hd list <> #"\n") andalso (hd list <> #" ")) then 
	   let
		val station = implode[hd list]
		val tuple = searchforspace(tl list,station)
	   in
		 makenewstringlist (FirstPartofTuple(tuple),new@[SecondPartofTuple(tuple)])
	   end	  
       else
	   makenewstringlist (tl list,new);

fun StringToInteger (x) = case Int.fromString (x) 
                                of SOME a => a
                                 |   NONE => 0;


fun CreateParentAndRankList(0,plist,rlist)=(plist,rlist)
|   CreateParentAndRankList(n,plist,rlist) = CreateParentAndRankList(n-1,plist@[n],rlist@[0]);
	
fun ElementiofList(i,list) = 
	if (i<>1) then ElementiofList(i-1,tl list)
	else hd list ;
	
fun ChangeiElementofListWith(i,nil,p,new) = new
|	ChangeiElementofListWith(i,list,p,new)=
		if(i<>1) then ChangeiElementofListWith(i-1,tl list,p,new@[hd list])
		else ChangeiElementofListWith(i-1,tl list,p,new@[p]);
		
fun find(i,plist)=
	let
		val p = ElementiofList(i,plist);
	in
		if (i<>p) then 
			let
				val Plist = ChangeiElementofListWith(i,plist,ElementiofList(p,plist),nil)
			in
				find(p,Plist)
			end
		else (i,plist)
	end;

fun Unconnected(p,q,plist) = 
	let
		val tupleP = find(p,plist)
		val tupleQ = find(q,SecondPartofTuple(tupleP))
	in
		if (FirstPartofTuple(tupleP) = FirstPartofTuple(tupleQ)) then (false,SecondPartofTuple(tupleQ))
		else (true,SecondPartofTuple(tupleQ))
	end;
	
fun Union(p,q,plist,rlist,count)= 
	let
		val tupleP = find(p,plist)
		val rootP = FirstPartofTuple(tupleP)
		val tupleQ = find(q,SecondPartofTuple(tupleP))
		val rootQ = FirstPartofTuple(tupleQ)
		val Plist = SecondPartofTuple(tupleQ)
	in
		if (rootP<>rootQ) then
			let
				val rankP = ElementiofList(rootP,rlist)
				val rankQ = ElementiofList(rootQ,rlist)
				val parentP = ElementiofList(rootP,Plist)
				val parentQ = ElementiofList(rootQ,Plist)
			in
				if(rankP < rankQ) then (ChangeiElementofListWith(rootP,Plist,rootQ,nil),ChangeiElementofListWith(rootQ,rlist,rankQ+rankP+1,nil),count-1)
				else (ChangeiElementofListWith(rootQ,Plist,rootP,nil),ChangeiElementofListWith(rootP,rlist,rankP+rankQ+1,nil),count-1)
			end
		else (plist,rlist,count)
	end;
	
fun AddOldRoads(nil,plist,rlist,count) = (plist,rlist,count)
|   AddOldRoads(list,plist,rlist,count) =
		let
			val TupleC = Unconnected(hd list,hd(tl list),plist)
			val Plist = SecondPartofTuple(TupleC)
		in
			if (FirstPartofTuple(TupleC)) then
				let
					val TupleUnion = Union(hd list,hd(tl list),Plist,rlist,count)
					val newPlist = firstPartofTuple(TupleUnion)
					val newRlist = secondPartofTuple(TupleUnion)
					val newcount = thirdPartofTuple(TupleUnion)
				in
					AddOldRoads(tl(tl list),newPlist,newRlist,newcount)
				end
			else AddOldRoads(tl(tl list),Plist,rlist,count)
		end;
		
fun FindSolution(0,n,plist,rlist,count)= (plist,rlist,count)
|	FindSolution(k,1,plist,rlist,count)=(plist,rlist,count)
|	FindSolution(k,n,plist,rlist,count)=
	let
		val Tuple = Unconnected(n,n-1,plist)
		val Plist = SecondPartofTuple(Tuple)
	in
		if (FirstPartofTuple(Tuple)) then
			let
				val tuple = Union(n,n-1,plist,rlist,count)
				val newPlist = firstPartofTuple(tuple)
				val Rlist = secondPartofTuple(tuple)
				val Count = thirdPartofTuple(tuple)
			in
				FindSolution(k-1,n-1,newPlist,Rlist,Count)
			end
		else FindSolution(k,n-1,Plist,rlist,count)
	end;
		
		
fun villages (infile)=
let
   val ins = TextIO.openIn infile
   val strinput =TextIO.inputAll(ins)

   val n = stringtointlist(strinput)
   val charlist =  reverse ((reverse ((explode strinput))))
   val stringlist = makenewstringlist(charlist,nil) 
   val oldlist= map StringToInteger stringlist
   val N = hd oldlist
   val M = hd(tl oldlist)
   val K = hd(tl (tl oldlist))
   val newlist = tl (tl(tl oldlist))
   val TupleList = CreateParentAndRankList(N,nil,nil)
   val PList = reverse(FirstPartofTuple(TupleList))
   val RList = reverse(SecondPartofTuple(TupleList))
   val newTupleList = AddOldRoads(newlist,PList,RList,N)
   val ParentList = firstPartofTuple(newTupleList)
   val RankList = secondPartofTuple(newTupleList)
   val count = thirdPartofTuple(newTupleList)
   val Solution = FindSolution(K,N,ParentList,RankList,count)
in
  thirdPartofTuple(Solution)
end;