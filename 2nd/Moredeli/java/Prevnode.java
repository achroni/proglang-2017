import java.io.*;
import java.util.*;
import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Prevnode {
	public char m;
	public int cost;

	public Prevnode(char m, int cost) {
		this.m = m;
		this.cost = cost;
	}

    // getter
    public int getM() { return m; }
    public int getCost() { return cost; }
    
	// setter
	public void setM(char m) { this.m = m; }
    public void setCost(int cost) { this.cost = cost; }
}