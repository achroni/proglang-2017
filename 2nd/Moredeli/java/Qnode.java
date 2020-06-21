import java.io.*;
import java.util.*;
import java.io.File;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Qnode {
	public int i;
	public int j;
	public int cost;

	public Qnode(int i,int j, int cost) {
		this.i = i;
		this.j = j;
		this.cost = cost;
	}
	

    // getter
    public int getI() { return i; }
	public int getJ() {return j;}
    public int getCost() { return cost; }
    
	// setter
	public void setI(int i) { this.i = i; }
	public void setJ(int j) {this.j = j; }
    public void setCost(int cost) { this.cost = cost; }
}