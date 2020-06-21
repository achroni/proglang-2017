# include <stdio.h>
# include <stdlib.h>
#include <limits.h>
# include <string.h>
#include <stdbool.h>


//Idea: To i στο δευτερο loop ουσιαστικά είναι τα απομείνοντα σκαλοπάτια και το res[i] αντιστοιχεί στους τροπούς με τους οποίους μπορώ να ανέβω τα απομείναντα σκαλοπάτια
// Οποτε απορρίπτω τις περιπτώσεις οταν πχ εστω εχω 10 σκαλοπάτια και το 2ο σκαλοπάτι είναι χαλασμένο τότε ΔΕΝ μπορώ να έχω ποτέ απομείναντα σκαλοπάτια ισα με 9 διότι αυτο σημαίνει
// οτί μπορω με βήμα 1 να πατήσω στο 2ο σκαλοπάτι.

int res[100000000];

long int mycount(int n,int k,int b,int* Step, int* X)
{
	//int res[n];
	int i,j,l;
	bool broken=false;
	res[0]=1;
	res[1]=0;
	
	for (i=0; i<k; i++){
		if(Step[i]==1) res[1]=1;
	}
	
	for(i=0; i<b; i++){
		if (n-X[i] == 1) res[1]=0;
	}
	//printf("res[0]=%d,res[1]=%d\n",res[0],res[1]);
	
	for(i=2; i<n; i++){
		res[i]=0;
		broken = false;
		for(j=0; j<k; j++){
			if(Step[j]<=i){
				for(l=0; l<b; l++){			// Απορρίπτω τις περιπτώσεις οι οποίες χρησιμοποιούν τα σπασμένα σκαλοπάτια
					if(i== n-X[l]){ 
						broken = true;
						//printf("i=%d,j=%d,l=%d broken\n",i,j,l);
					}
				}
				if(!broken)
					res[i] = (res[i] + res[i-Step[j]])%1000000009;
			}
		}
	}
	/*for(i=0; i<n; i++){
		printf("res[%d]=%d\n",i,res[i]);
	}*/
	return(res[n-1]);
}
int main (int argc, char* argv[]) {
	int N,K,B;
	FILE *fp;
	
   fp = fopen(argv[1], "r");
   fscanf(fp, "%d",&N);
  // printf("N=%d\n",N);
   fscanf(fp, "%d",&K);
  // printf("K=%d\n",K);
   fscanf(fp, "%d",&B);   
   //printf("B=%d\n",B);
   int X[B],Step[K],i;
   bool unlucky = false;
   for(i=0; i<K; i++){
	   Step[i]=0;
	   fscanf(fp,"%d",&Step[i]);   
   }
   for(i=0; i<B; i++){
	   X[i]=0;
	   fscanf(fp,"%d",&X[i]); 
	   if (X[i] == 1 || X[i] == N){
		   unlucky = true;
	   }
   }
  fclose(fp);
  
  /*for(i=0; i<K; i++){
	   printf("Step[%d]=%d \n",i,Step[i]);   
   }
    for(i=0; i<B; i++){
	   printf("X[%d]=%d\n",i,X[i]);   
   }*/
 
  long int Solution =0;
   if (!unlucky){
	Solution = mycount(N,K,B,Step,X);
   }
   printf("%ld",Solution);	
   return 0;
}