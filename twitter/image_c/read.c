#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
int main(int argc,char* argv[]) {
  int width,height,channels;
  unsigned char *img = stbi_load(argv[1], &width, &height, &channels, 0);
  printf("immagine letta correttamente\n");
  FILE* f;
  FILE* f_list;
  f=fopen("color.txt","r");
  int* r=malloc(sizeof(int)*50);
  int* g=malloc(sizeof(int)*50);
  int* b=malloc(sizeof(int)*50);
  int* freq=malloc(sizeof(int)*50);
  
  for(int i=0;i<50;i++) {
    int r_t,g_t,b_t;
    fscanf(f,"(%d,%d,%d)\n",&r_t,&g_t,&b_t);
    r[i]=r_t;
    g[i]=g_t;
    b[i]=b_t;
    //    printf("%d %d %d\n",r[i],g[i],b[i]);
    freq[i]=0;
  }
  int index;
  printf("inizio calcolo");

  for(int i=0;i<width*height*3;i=i+3) {
    //    printf("%d %d %d \n",img[i],img[i+1],img[i+2]);
    double dist=sqrt(3*255*255);
    index=0;
    for(int j=0;j<50;j++) {
      double dist_t;
      int v1,v2,v3;
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
    freq[index]++; 
  }
  printf("%s,",argv[1]);
  for(int i=0;i<50;i++) {
    printf("%9.9f",freq[i]/(double)(width*height));
    if(i<49)
      printf(",");
  }

  printf("\n");
  
  
  
  return 0;
  }
