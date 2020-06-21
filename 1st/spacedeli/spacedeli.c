# include <stdio.h>
# include <stdlib.h>
#include <limits.h>
# include <string.h>
#include <stdbool.h>

struct QNode {
   int i;
   int j;
   int cost;
   bool pizza;
   struct QNode * next;
}; 

struct PrevNode{
	char m;
	int cost;
};

char P[1000][1000];
struct PrevNode PrevPizza[1000][1000], PrevNoPizza[1000][1000];
struct QNode *head,*tail;
int endi=0,endj=0;

void CreateQNode (int i, int j,int cost, bool flag){
	struct QNode* node = (struct QNode*)malloc(sizeof(struct QNode));
	node->i = i;
	node->j = j;
	node->cost = cost;
	node->pizza = flag;
	node->next=NULL;
	if (head==NULL){
		head=node;
		tail = head;
	}
	else {
		if((head!=NULL) && (cost<= head->cost))
		{  
			node->next=head;
			head=node;
		}
		else
		{
			struct QNode* q=head;
			while(q->next != NULL && q->next->cost<=cost)
			{q=q->next;}
			node->next=q->next;
			q->next=node;
		}
	}
}

void  FindNextNode (int i, int j, int cost, bool flag,char c){
	if (flag){
		if (c == 'W'){
			if(PrevPizza[i][j].m == '-' || PrevPizza[i][j].cost > cost+1){
				PrevPizza[i][j].m = c;
				PrevPizza[i][j].cost = cost+1;
				CreateQNode(i,j,cost+1,true);
			}
		}
		else{
			if(PrevPizza[i][j].m == '-' || PrevPizza[i][j].cost > cost+2){
				PrevPizza[i][j].m = c;
				PrevPizza[i][j].cost = cost+2;
				CreateQNode(i,j,cost+2,true);
			}
		}
	}
	else{
		 if (PrevNoPizza[i][j].m == '-' || PrevNoPizza[i][j].cost > cost+1){
			PrevNoPizza[i][j].m = c;
			PrevNoPizza[i][j].cost = cost+1;
			CreateQNode (i,j,cost+1,false);
		}
	}
}

int FindSolution (int starti,int startj,int M, int N){
	bool FoundE = false;
	int cost =0;
	
	CreateQNode(starti,startj,0,true);
	PrevPizza[starti][startj].m = 'n';
	PrevPizza[starti][startj].cost = 0;
	while (head != NULL && !FoundE){
		struct QNode* n = head; 
		cost = n->cost;
		head = head->next;
		
		if (P[n->i][n->j] == 'E' && n->pizza){
			endi = n->i;
			endj = n->j;
			FoundE = true;
			break;
		}
		
		if (P[n->i][n->j] =='W'){
			FindNextNode(n->i,n->j,cost,!n->pizza,'W');
		}
		
		if (P[n->i][n->j] != 'X' ){
			if (n->i!=0) FindNextNode((n->i)-1,n->j,n->cost,n->pizza,'U');
			
			if (n->j!=0) FindNextNode(n->i,(n->j)-1,n->cost,n->pizza,'L');
			
			if (n->i != N-1) FindNextNode((n->i)+1,n->j,n->cost,n->pizza,'D');
			
			if (n->j !=M-1)	FindNextNode(n->i,(n->j)+1,n->cost,n->pizza,'R');
		}
	}
	
	return(cost);
}

void PrintThePath(int i,int j,bool pizza){
	//if(PrevPizza[i][j].m == 'n') return;

	if (pizza){
		if(PrevPizza[i][j].m == 'n') return;
		if (PrevPizza[i][j].m=='U') PrintThePath(i+1,j,true);
		else if (PrevPizza[i][j].m=='D') PrintThePath(i-1,j,true);
		else if (PrevPizza[i][j].m=='R') PrintThePath(i,j-1,true);
		else if (PrevPizza[i][j].m=='L') PrintThePath(i,j+1,true);
		else if (PrevPizza[i][j].m == 'W') PrintThePath(i,j,false);
		 
		printf("%c",PrevPizza[i][j].m);
	}
	else{
		if (PrevNoPizza[i][j].m=='U') PrintThePath(i+1,j,false);
		else if (PrevNoPizza[i][j].m=='D') PrintThePath(i-1,j,false);
		else if (PrevNoPizza[i][j].m=='R') PrintThePath(i,j-1,false);
		else if (PrevNoPizza[i][j].m=='L') PrintThePath(i,j+1,false);
		else if (PrevNoPizza[i][j].m == 'W') PrintThePath(i,j,true);
		
		printf("%c",PrevNoPizza[i][j].m);
	}
	return;
}

int main (int argc, char* argv[]) {
	int i=0,j=0, M=0, N=0,starti=0,startj=0;
	FILE *fp;
	
	fp = fopen(argv[1], "r");
  
	char c;
	
	while((c=fgetc(fp))!=EOF) {
		if (c!= ' '){
			if (c == 'S'){
				starti=i;
				startj=j;
			} 
			if(c=='\n') {  i++; M=j; j=0; }
			else P[i][j++]=c;
		}
	}
	fclose(fp);
	
	N=i;
	//M=j;

	//printf("%d %d ",N,M);
	
	for (i=0; i<N; i++){
		for (j=0; j<M; j++){
			PrevPizza[i][j].m = '-';
			PrevPizza[i][j].cost=INT_MAX;
			PrevNoPizza[i][j].m = '-';
			PrevNoPizza[i][j].cost=INT_MAX;
		}
	}
	
	head =NULL;
	tail=NULL;

	
	int Solution = FindSolution(starti,startj,M,N);
	printf("%d ",Solution);
	
	/*for (i=0; i<N; i++){
		for (j=0; j<M; j++){
			printf("PrevPizza[%d][%d]=%c 			PrevNoPizza[%d][%d]=%c\n",i,j,PrevPizza[i][j].m,i,j,PrevNoPizza[i][j].m);
		}
	}*/
	
	PrintThePath (endi,endj,true);
	printf("\n");
	return 0;
}