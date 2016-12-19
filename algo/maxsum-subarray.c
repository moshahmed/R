// Maximal sum sub array.
// GPL(C) moshahmed@gmail.com

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#define NN 1000

int main(int argc, char *argv[]){
  int A[NN], N=10;
  int i,j;
  int b, e;
  int sum, max_sum;

  if (argc > 2) { // usage: cmd 13 -3 -25 20 -3 -16 -23 18 20 -7 12 -5 -22 15 -4 7
    N = argc-1;
    for(i=1; i<argc; i++)
      A[i-1] = atoi(argv[i]);
  } else {
    if (argc == 2) { // usage: cmd N
      N = atoi(argv[1]);
    }
    for(i=0;i<N;i++){
      A[i]= 1 + (int ) (N * (float) rand()/(float)RAND_MAX) - N/2 ;
    }
  }

  printf("Array A=[");
  for(i=0;i<N;i++){
    printf("%3d ",A[i]);
    if (i % 12 == 11) printf("\n  ");
  }
  printf("]\n");

  b=-1;
  e=-1;
  max_sum=-INT_MAX;
  for(j=0;j<N;j++){
    sum=0;
    for(i=j;i<N;i++){
      sum+=A[i];
      if(sum>max_sum){
        max_sum=sum;
        b=j;
        e=i;
      }
    }
  }

  printf("max_sum=%d subarray A[%d..%d]=[",max_sum, b, e);
  for(i=b;i<e+1;i++){
    printf("%d ",A[i]);
  }
  printf("]\n");

  return 0;
}
