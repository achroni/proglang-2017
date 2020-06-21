import java.io.*;
import java.util.*;
import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;


public class Moredeli {

	private ArrayList<Qnode> Qlist;
	private Prevnode[][] Prevlist;
	
    public Moredeli(int n,int m, int starti, int startj, int cost) {
		Qlist = new ArrayList<Qnode>();
		Qnode node = new Qnode(starti,startj,0);
		Qlist.add(node);
   
        if (n < 0) throw new IllegalArgumentException();
        if (m < 0) throw new IllegalArgumentException();
		Prevlist = new Prevnode[n][m];
        for (int i = 0; i < n; i++) {
			for (int j = 0; j < m; j++){
				if (i==starti && j==startj) Prevlist[i][j] = new Prevnode ('n',0);
				else Prevlist[i][j] = new Prevnode ('-',Integer.MAX_VALUE);
			}
        }
    }
	
	public ArrayList<Qnode> insertQnode(ArrayList<Qnode> Qlist,Qnode node){
		int counter =0;
		boolean insert = false;
		//System.out.println("Mphka sthn insert!!!");
		if (!Qlist.iterator().hasNext()){
			Qlist.add(node);
			insert =true;
		}
		else
		{
			for (Iterator i = Qlist.iterator(); i.hasNext();)
			{
				Qnode o = (Qnode)i.next();
				if (o.cost >= node.cost){ 
					Qlist.add(counter,node); 
					//System.out.println("Pros8esa to neo kombo!!");
					insert = true;
					break;
				}
				counter++;
			}
			if (insert == false) Qlist.add(node);
		}
		return (Qlist);
	}
	
	public ArrayList<Qnode> findNextNode (ArrayList<Qnode> Qlist,int i,int j,int cost, char c){
		if (c == 'U'){
			if(Prevlist[i][j].m == '-' || Prevlist[i][j].cost > cost+3){
				Prevlist[i][j].m = c;
				Prevlist[i][j].cost = cost+3;
				Qnode no = new Qnode(i,j,cost+3);
				Qlist = insertQnode(Qlist,no);
				//System.out.println ("Kalw thn insertlist");
			}
		}
		if (c == 'L'){
			if(Prevlist[i][j].m == '-' || Prevlist[i][j].cost > cost+2){
				Prevlist[i][j].m = c;
				Prevlist[i][j].cost = cost+2;
				Qnode no = new Qnode(i,j,cost+2);
				Qlist = insertQnode(Qlist,no);
				//System.out.println ("Kalw thn insertlist");
			}
		}
		if (c == 'D' || c == 'R'){
			if(Prevlist[i][j].m == '-' || Prevlist[i][j].cost > cost+1){
				Prevlist[i][j].m = c;
				Prevlist[i][j].cost = cost+1;
				Qnode no = new Qnode(i,j,cost+1);
				Qlist = insertQnode(Qlist,no);
				//System.out.println ("Kalw thn insertlist");
			}
		}
		return (Qlist);
	}
	
	public void PrintThePath(int i,int j){
		if(Prevlist[i][j].m == 'n') return;
		
		if (Prevlist[i][j].m=='U') PrintThePath(i+1,j);
		else if (Prevlist[i][j].m=='D') PrintThePath(i-1,j);
		else if (Prevlist[i][j].m=='R') PrintThePath(i,j-1);
		else if (Prevlist[i][j].m=='L') PrintThePath(i,j+1);
		 
		System.out.print(Prevlist[i][j].m);
	}

    public static void main(String[] args) {
		File file = null;
		if (0 < args.length) {
			file = new File(args[0]);
		} else {
			System.err.println("Invalid arguments count:" + args.length);
		}
		try{
			FileInputStream file1 = new FileInputStream(file);
			int i=0,j=0;
			char [][] map = new char [10000][10000];
			int starti=0,startj=0,endi=0,endj=0;
			int m=0;
			//int n =0;
			boolean firstline = false;
			while (true) {
				int input = file1.read();
				if (input == -1 ) break;

				char c = (char) input;	
				if (c == '\n') {
					m=j;
					i++;
					j=0;
				}
				else if (c != ' '){
					map[i][j] = c;
					//System.out.println("map[" + i + "][" + j + "]=" + map[i][j]);
					if (map[i][j] == 'S'){
						starti = i;
						startj = j;
					}
					if (map[i][j] == 'E'){
						endi = i;
						endj = j;
					}
					j++;
				}
			}
			int n = i;
			//m = m-1;
			//System.out.println("n=" + n + " m=" +m);
			/*for (i=0; i<n; i++){
				for(j=0; j<m; j++){
					System.out.println(map[i][j]);
				}
				System.out.println("\n");
			}*/
			//System.out.println("endi=" + endi + " endj=" +endj);
			Moredeli d = new Moredeli(n,m,starti,startj,0);
			
			Iterator k = d.Qlist.iterator();
			boolean FoundE = false;
			int cost =0;
			while (k.hasNext() && !FoundE){
				Qnode node = (Qnode)k.next();
				cost = node.cost;
				k.remove();
				
				if (map[node.i][node.j] == 'E'){
					FoundE = true;
					break;
				}
				
				if (map[node.i][node.j] !='X'){
					if (node.i!=0) d.Qlist = d.findNextNode(d.Qlist,node.i -1, node.j, node.cost, 'U');
					if (node.j!=0) d.Qlist = d.findNextNode(d.Qlist,node.i,node.j -1,node.cost,'L');
					if (node.i!= n-1) d.Qlist = d.findNextNode(d.Qlist,node.i +1, node.j, node.cost, 'D');
					if (node.j!=m-1) d.Qlist = d.findNextNode(d.Qlist,node.i,node.j+1,node.cost,'R');
				}
				k = d.Qlist.iterator();
			}
			/*//Print Prevlist
			for (i=0; i<n; i++){
				for (j=0; j<m; j++){
					System.out.println("Prevlist[" + i + "][" + j + "].m=" + d.Prevlist[i][j].m + " Prevlist.cost=" + d.Prevlist[i][j].cost);
				}
			}*/
			System.out.print(cost+" ");
			
			d.PrintThePath(endi,endj);
		}
		catch(IOException e)  
		{
			// insert code to run when exception occurs
		}
    }
}