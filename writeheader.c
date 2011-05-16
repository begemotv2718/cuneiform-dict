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
  strncpy((dictheader.sign),"CTCDict",8);
  strncpy((dictheader.cpuType),"Intel",8);
  strncpy(dictheader.language, "BEL",8);
  strncpy(dictheader.version,"03.03",8);
/*
  strncpy(&dictheader.treeLength, argv[1],8);
  strncpy(&dictheader.tailsLength, argv[2],8);
  strncpy(&dictheader.rulesLength, argv[3],8);
  strncpy(&dictheader.hushLength, argv[4],8);
  strncpy(&dictheader.abcSize, "32",8);
*/
  snprintf(dictheader.treeLength,8,"%07d",atoi(argv[1])); //length of the tree file
  snprintf(dictheader.tailsLength,8,"%07d",atoi(argv[2])); //pkdendings length
  snprintf(dictheader.rulesLength,8,"%07d",atoi(argv[3])); //Vartable length
  snprintf(dictheader.hushLength,8,"%07d",atoi(argv[4])); //WTF, whateva 
  snprintf(dictheader.abcSize,8,"%07d",33);  
  uchar i;
  int j=0;
  uchar abcLower[] = {0xa0/*а*/, 0xa1/*б*/, 0xa2/*в*/, 0xa3/*г*/, 0xa4/*д*/, 0xa5/*е*/, 0xf1/*ё*/, 0xa6/*ж*/,
            0xa7/*з*/, 0xf7/*і*/, 0xa9/*й*/, 0xaa/*к*/, 0xab/*л*/, 0xac/*м*/, 0xad/*н*/, 0xae/*о*/, 0xaf/*п*/,
            0xe0/*р*/, 0xe1/*с*/, 0xe2/*т*/, 0xe3/*у*/, 0xf8/*ў*/, 0xe4/*ф*/, 0xe5/*х*/, 0xe6/*ц*/, 0xe7/*ч*/, 
            0xe8/*ш*/, 0xeb/*ы*/, 0xec/*ь*/, 0xed/*э*/, 0xee/*ю*/, 0xef/*я*/, 0x27/*'*/,0}; 
  uchar abcUpper[] ={0x80/*А*/, 0x81/*Б*/, 0x82/*В*/, 0x83/*Г*/, 0x84/*Д*/, 0x85/*Е*/, 0xf0/*Ё*/, 0x86/*Ж*/,
            0x87/*З*/, 0xf6/*І*/, 0x89/*Й*/, 0x8a/*К*/, 0x8b/*Л*/, 0x8c/*М*/, 0x8d/*Н*/, 0x8e/*О*/, 0x8f/*П*/,
            0x90/*Р*/, 0x91/*С*/, 0x92/*Т*/, 0x93/*У*/, 0xf9/*Ў*/, 0x94/*Ф*/, 0x95/*Х*/, 0x96/*Ц*/, 0x97/*Ч*/,
            0x98/*Ш*/, 0x9b/*Ы*/, 0x9c/*Ь*/, 0x9d/*Э*/, 0x9e/*Ю*/, 0x9f/*Я*/, 0x27/*'*/,0};
  strncpy((dictheader.abcUpper),abcUpper,sizeof(abcUpper));
  strncpy((dictheader.abcLower),abcLower,sizeof(abcLower));
   
  fwrite(&dictheader,sizeof(dictheader),1,stdout); 

}


