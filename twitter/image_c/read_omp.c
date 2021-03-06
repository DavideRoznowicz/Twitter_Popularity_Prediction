#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
#include <omp.h>

// module load gnu/9.3.0
// export OMP_NUM_THREADS=10
// # compiling: link header in stb folder
// gcc -fopenmp -std=gnu99 read_omp.c -o read_omp.x -Istb/ -lm
// # insida a computational node: 
// /usr/bin/time ./read_omp.x photo5798471721416766496.jpg


int main(int argc,char* argv[]) {

  int width,height,channels;
  unsigned char *img = stbi_load(argv[1], &width, &height, &channels, 0);
#pragma OPTIMIZE OFF
  FILE* f;
  FILE* f_list;
  f=fopen("color.txt","r");
  int* r=malloc(sizeof(int)*50);
  int* g=malloc(sizeof(int)*50);
  int* b=malloc(sizeof(int)*50);
  int* freq=malloc(50* sizeof(int));
  
  for(int i=0;i<50;i++) {
    int r_t,g_t,b_t;
    fscanf(f,"(%d,%d,%d)\n",&r_t,&g_t,&b_t);
    r[i]=r_t;
    g[i]=g_t;
    b[i]=b_t;
    //    printf("%d %d %d\n",r[i],g[i],b[i]);
    freq[i]=0;
  }
#pragma OPTIMIZE ON
  int index;
  double dist_t;
  int v1,v2,v3;
  double dist;

#pragma omp parallel shared(img, freq) firstprivate(dist_t, dist, v1, v2, v3, index, r, g, b) proc_bind(close)
{
  #pragma omp for reduction(+: freq[:50])  // freq reduction as a 50 dim array
  for(int i=0;i<width*height*3;i=i+3) {
    //    printf("%d %d %d \n",img[i],img[i+1],img[i+2]);
    dist=sqrt(3*255*255);
    index=0;
    for(int j=0;j<50;j++) {
      v1=img[i]-r[j];
      v2=img[i+1]-g[j];
      v3=img[i+2]-b[j];
      dist_t=sqrt(v1*v1+v2*v2+v3*v3);
      //      printf("%f\n",dist_t);
      if(dist_t<dist) {
	dist=dist_t;
	index=j;
      }
    }
    //    printf("index= %d\n", index);
    
    //#pragma omp atomic
    freq[index] += 1;
  }
  

} //end of parallel region


 
 printf("%s,",argv[1]);                                                                                                                                                                                    for(int i=0;i<50;i++) {                                                                                                                                                                                     printf("%9.9f",freq[i]/(double)(width*height));                                                                                                                                                           if(i<49)                                                                                                                                                                                                    printf(",");                                                                                                                                                                                          }                                                                                                                                                                                                         printf("\n");                                                                                                                                                                                                  
 
  return 0;
}
