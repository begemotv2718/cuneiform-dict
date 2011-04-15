#include <stdio.h>
#include <unistd.h>
#include "speldict.h"
#include <string.h>
#include <stdlib.h>
#define uchar unsigned char
int main(int argc, char **argv){
  TDictHeaderMask dictheader;
  if(argc<4){
    fprintf(stderr,"Usage writeheader <treelength> <tailslength> <ruleslength> <hushlength>\n");
    exit(1);
  }
  memset(&dictheader,0,sizeof(dictheader));
  strncpy(&(dictheader.sign),"CTCDict",8);
  strncpy(&(dictheader.cpuType),"Intel",8);
  strncpy(&dictheader.language, "RUS",8);
  strncpy(&dictheader.version,"03.03",8);
/*
  strncpy(&dictheader.treeLength, argv[1],8);
  strncpy(&dictheader.tailsLength, argv[2],8);
  strncpy(&dictheader.rulesLength, argv[3],8);
  strncpy(&dictheader.hushLength, argv[4],8);
  strncpy(&dictheader.abcSize, "32",8);
*/
  snprintf(&dictheader.treeLength,8,"%07d",atoi(argv[1]));
  snprintf(&dictheader.tailsLength,8,"%07d",atoi(argv[2]));
  snprintf(&dictheader.rulesLength,8,"%07d",atoi(argv[3]));
  snprintf(&dictheader.hushLength,8,"%07d",atoi(argv[4]));  
  snprintf(&dictheader.abcSize,8,"%07d",32);  
  uchar i;
  int j=0;
  for(i=128; i<=159; i++)
  {
    dictheader.abcUpper[j]=i;
    j++;
  }
  j=0;
  for(i=160;i<=175;i++)
  {
    dictheader.abcLower[j]=i;
    j++;
  }
  for(i=224;i<=239; i++)
  {
    dictheader.abcLower[j]=i;
    j++;
  }
 
  fwrite(&dictheader,sizeof(dictheader),1,stdout); 

}


