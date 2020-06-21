fun reverse nil = nil 
  | reverse(first :: rest) = reverse rest @ [first] ;

fun stringtointlist x = map (fn x => Char.ord x -48) (explode x);

fun makeIntegers(x) = case (Int.fromString(x)) of 
				SOME a => a |
                                NONE => 0 ;


fun printList (a) = 
	if a= nil then print "\n"
	else if (hd a)= ~1 then (print "| "; printList(tl a))
              else ( print (Int.toString (hd a)); print " "; printList(tl a));

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

fun IntListToTupleList(nil,new,n) = new
|   IntListToTupleList(list,new,n) = 
	IntListToTupleList(tl list,new@[(n+1,hd list)],n+1);

fun createListL (nil, i, new, sizeL, min) = (new,sizeL)
|   createListL (P, i, new, sizeL, min) = 
        if (min > hd P) then createListL (tl P, i+1, new@[(i,hd P)], sizeL+1, hd P)
        else createListL (tl P, i+1, new, sizeL, min);
		
fun createListR (nil, i, new, sizeR, max) = (new,sizeR)
|   createListR (P, i, new, sizeR, max) = 
        if (max < hd P) then createListR (tl P, i-1, new@[(i,hd P)], sizeR+1, hd P)
        else createListR (tl P, i-1, new, sizeR, max);

fun Maximum (a,b) =
    if (a>b) then a
	else b;
		
fun FindPrevMax (nil,stationdown,prevMax)=(prevMax,nil)
|   FindPrevMax (R,stationdown,prevMax) = 
      if (SecondPartofTuple(stationdown) > SecondPartofTuple(hd R)) then (prevMax,R)
	   else FindPrevMax(tl R,stationdown,Maximum(prevMax,FirstPartofTuple(hd R)-FirstPartofTuple(stationdown)));
	
fun FindSolution (nil,R,prevMax)=prevMax
|   FindSolution (L,nil,prevMax)=prevMax
|   FindSolution (L,R,prevMax)= 
		let 
			val tuple = FindPrevMax(R,hd L,prevMax)
		in 
			FindSolution (tl L,SecondPartofTuple(tuple),FirstPartofTuple(tuple))
		end;
	
fun skitrip (infile)=
let
   val ins = TextIO.openIn infile
   val strinput =TextIO.inputAll(ins)

   val n = stringtointlist(strinput)
   val charlist =  reverse ((reverse ((explode strinput))))
   val stringlist = makenewstringlist(charlist,nil) 
   val oldlist= map StringToInteger stringlist
   val N = hd oldlist
   val P = tl oldlist
   val tupleL = createListL (P, 1, nil, 0, 100000000)
   val L = FirstPartofTuple (tupleL)
   val sizeL = SecondPartofTuple (tupleL)
   val tupleR = createListR (reverse P,N,nil,0,~1)
   val R = FirstPartofTuple (tupleR)
   val sizeR = SecondPartofTuple (tupleR)
   val Solution = FindSolution(L,reverse R,~1)
in
  Solution
end;