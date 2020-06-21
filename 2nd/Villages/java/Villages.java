import java.io.*;
import java.util.*;
import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;


public class Villages {

    private int[] parent;  // parent[i] = parent of i
    private int[] rank;   // rank[i] = rank of subtree rooted at i (never more than 31)
    private int count;     // number of components

    /**
     * Initializes an empty union–find data structure with {@code n} sites
     * {@code 0} through {@code n-1}. Each site is initially in its own 
     * component.
     *
     * @param  n the number of sites
     * @throws IllegalArgumentException if {@code n < 0}
     */
    public Villages(int n) {
        if (n < 0) throw new IllegalArgumentException();
        count = n;
        parent = new int[n+1];
        rank = new int[n+1];
        for (int i = 0; i <= n; i++) {
            parent[i] = i;
            rank[i] = 0;
        }
    }

    /**
     * Returns the component identifier for the component containing site {@code p}.
     *
     * @param  p the integer representing one site
     * @return the component identifier for the component containing site {@code p}
     * @throws IndexOutOfBoundsException unless {@code 0 <= p < n}
     */
    public int find(int p) {
        validate(p);
        while (p != parent[p]) {
            parent[p] = parent[parent[p]];    // path compression by halving
            p = parent[p];
        }
        return p;
    }

    /**
     * Returns the number of components.
     *
     * @return the number of components (between {@code 1} and {@code n})
     */
    public int count() {
        return count;
    }
  
    /**
     * Returns true if the the two sites are in the same component.
     *
     * @param  p the integer representing one site
     * @param  q the integer representing the other site
     * @return {@code true} if the two sites {@code p} and {@code q} are in the same component;
     *         {@code false} otherwise
     * @throws IndexOutOfBoundsException unless
     *         both {@code 0 <= p < n} and {@code 0 <= q < n}
     */
    public boolean connected(int p, int q) {
        return find(p) == find(q);
    }
  
    /**
     * Merges the component containing site {@code p} with the 
     * the component containing site {@code q}.
     *
     * @param  p the integer representing one site
     * @param  q the integer representing the other site
     * @throws IndexOutOfBoundsException unless
     *         both {@code 0 <= p < n} and {@code 0 <= q < n}
     */
    public void union(int p, int q) {
        int rootP = find(p);
        int rootQ = find(q);
        if (rootP == rootQ) return;

        // make root of smaller rank point to root of larger rank
        if (rank[rootP] < rank[rootQ]) {
			parent[rootP] = rootQ;
			rank[rootQ] = rank[rootQ]+rank[rootP] +1;
			rank[rootP]=0;
		}
        else if (rank[rootP] > rank[rootQ]) {
			parent[rootQ] = rootP;
			rank[rootP] = rank[rootP]+rank[rootQ] +1;
			rank[rootQ]=0;
		}
        else {
            parent[rootQ] = rootP;
            rank[rootP] = rank[rootP]+rank[rootQ] +1;
			rank[rootQ]=0;
        }
        count--;
    }

	public void PrintFunction(int n){
		int i;
		System.out.println("Parent: ");
		for (i=1; i<=n; i++){
			System.out.print(parent[i]+" ");
		}
		System.out.println("\n"+"Rank: ");
		for (i=1; i<=n; i++){
			System.out.print(rank[i]+" ");
		}
	}
    // validate that p is a valid index
    private void validate(int p) {
        int n = parent.length;
        if (p < 0 || p >= n) {
            throw new IndexOutOfBoundsException("index " + p + " is not between 0 and " + (n-1));  
        }
    }

    /**
     * Reads in a an integer {@code n} and a sequence of pairs of integers
     * (between {@code 0} and {@code n-1}) from standard input, where each integer
     * in the pair represents some site;
     * if the sites are in different components, merge the two components
     * and print the pair to standard output.
     *
     * @param args the command-line arguments
     */
    public static void main(String[] args) {
		File file = null;
		if (0 < args.length) {
			file = new File(args[0]);
		} else {
			System.err.println("Invalid arguments count:" + args.length);
		}
		try{
			Scanner scanner = new Scanner(file);
			scanner.hasNextInt();
			int n = scanner.nextInt();
			int m = scanner.nextInt();
			int k = scanner.nextInt();
			Villages uf = new Villages(n);
			while(scanner.hasNextInt())
			{
				int p = scanner.nextInt();
				int q = scanner.nextInt();
				if (uf.connected(p, q)) continue;
				uf.union(p, q);
			}
			int i=1;
			while(k>0 && i<n){
				while(i<n && uf.connected(i,i+1) ){
					i++;
				}
				if(i<n){
					uf.union(i,i+1);
					i++;
					k--;
				}
			}
			System.out.println(uf.count());
		}
		
		catch (FileNotFoundException ex)  
		{
			// insert code to run when exception occurs
		}
    }
}