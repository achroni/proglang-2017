# include <stdio.h>
# include <stdlib.h>
#include <limits.h>
# include <string.h>

struct Stations {
   long int index;
   long int y;
} station; 

long int P[2000000];//, R[2000000], L[2000000];

long int Maximum (long int a, long int b){
	if (a>b) return a;
	else return b;
}

long int CreateArrayL (struct Stations* L,long int P[], long int N, long int j){
	long int i,min = INT_MAX;
	j=0;
	
	for (i=0; i<N; i++){
		if (min>P[i]){
			min = P[i];
			L[j].index=i;
			L[j].y=P[i];
			j++;
		}
	}
	return(j);
}

long int CreateArrayR (struct Stations* R, long int P[], long int N, long int j){
	long int i, max = INT_MIN;
	j=0;
	
	for (i=N-1; i>=0; i--){
		if (max<P[i]){
			max = P[i];
			R[j].index=i;
			R[j].y=P[i];
			j++;
		}
	}
	return(j);
}

long int Find_Solution (struct Stations* L,long int Lsize,struct Stations* R, long int Rsize){
	int i=0,j,prevMax=INT_MIN;
	j=Rsize -1;
	while(i<Lsize && j>=0){
		//j=Rsize -1;
		while((L[i].y <= R[j].y) && j>=0){
			prevMax=Maximum(prevMax,R[j].index - L[i].index);
			j--;
		}
		i++;
	}
	
	return (prevMax);
}

int main (int argc, char* argv[]) {
	long int i=0,N,Lsize=0,Rsize=0;
	struct Stations *L,*R;
	FILE *fp;
	
   fp = fopen(argv[1], "r");
   fscanf(fp, "%ld",&N);
  
	L=(struct Stations *)malloc (N*sizeof(struct Stations));
	R=(struct Stations *)malloc(N*sizeof(struct Stations));
	
	for(i=0; i<N; i++){
		fscanf(fp,"%ld",&P[i]);
	}
	
	fclose(fp);
	//Initialize arrays
	for (i=0; i<N; i++){
		R[i].index=0;
		R[i].y=0;
		L[i].index=0;
		L[i].y=0;
	}
	
	//Create arrays
   Lsize = CreateArrayL(L,P,N, Lsize);
   Rsize = CreateArrayR(R,P,N,Rsize);
	
	/*//print arrays
	for(i=0; i<Lsize; i++){
		printf ("Li= %ld Y=%ld\n",L[i].index,L[i].y);
	}
	printf("\n");
	for(i=0; i<Rsize; i++){
		printf ("Ri= %ld Y=%ld\n",R[i].index,R[i].y);
	}*/
	 
	long int Solution = Find_Solution(L,Lsize,R,Rsize);
	printf ("%ld\n",Solution);
	free(L);
	free(R);
	return 0;
}