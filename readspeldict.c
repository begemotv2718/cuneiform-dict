#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include "speldict.h"

/*************************************************************************/
/*              Macros & constants insted of Bit Fields.                 */
/*************************************************************************/
/* -- Pointer tree node head fields. -------------------------------- */

#define VERTV_SIZE      1
#define VERTV_CONT(p)   ((uint16_t)((*(p))&0x01))       /* Boolean */
#define VERTV_NOTERM(p) ((uint16_t)((*(p))&0x02))       /* Boolean */
#define VERTV_KEY(p)    ((char)((*(p)) >> 2))       /* char */

/* -- Positional tree node fields. ---------------------------------- */

#define VERTP_SIZE      3
#define VERTP_CONT      VERTV_CONT                  /* Boolean */
#define VERTP_NOTERM    VERTV_NOTERM                /* Boolean */
#define VERTP_EXIST(p)  ((uint16_t)((*(p))&0x04))       /* Boolean */
#define VERTP_SHIFT0(p) ((uint16_t)((*(p)) >> 3))       /* uint16_t */
#define VERTP_SHIFT1(p) ((uint16_t)(*(p+1)))            /* uint16_t */
#define VERTP_SHIFT2(p) ((uint16_t)(*(p+2)))            /* uint16_t */

/* -- Pointer tree node postfics fields. ---------------------------- */

#define POSTFICS_SIZE       2
#define POSTFICS_CONT       VERTV_CONT              /* Boolean */
#define POSTFICS_TAIL(p)    ((uint16_t)((*(p))&0x02))   /* Boolean */
#define POSTFICS_ACCNT(p)   ((uint16_t)((*(p))&0x04))   /* Boolean */
#define POSTFICS_ENTER0(p)  ((uint16_t)((*(p)) >> 3))   /* uint16_t    */
#define POSTFICS_ENTER1(p)  ((uint16_t)(*(p+1)))        /* uint16_t    */

/* -- Pointer tree node account fields. ----------------------------- */

#define ACCOUNT_SIZE        1
#define ACCOUNT_CONT        VERTV_CONT              /* Boolean */
#define ACCOUNT_TAIL        POSTFICS_TAIL           /* Boolean */
#define ACCOUNT_ACCNT       POSTFICS_ACCNT          /* Boolean */
#define ACCOUNT_WRDTERM(p)  ((uint16_t)((*(p))&0x08))   /* Boolean */
#define ACCOUNT_FREQ(p)     ((uint16_t)((*(p)) >> 5))   /* uint16_t */

/* -- Pointer tree relative shift to the next node in level. -------- */

/* ADDR_SIZE not a valid value!!!                              */
/* ADDR may be interpreted differently in the current context. */

#define ADDR_CONT      VERTV_CONT                   /* Boolean */
#define ADDR_TAIL      POSTFICS_TAIL                /* Boolean */
#define ADDR_LTH(p)    ((uint16_t)((*(p))&0x04))        /* Boolean */
#define ADDR_SHIFT0(p) ((uint16_t)((*(p)) >> 3))        /* uint16_t */
#define ADDR_LTH2(p)   ((uint16_t)((*((p)+1))&0x01))    /* uint16_t */
#define ADDR_SHIFT2(p) ((uint16_t)((*((p)+1)) >> 1))    /* uint16_t */
#define ADDR_SHIFT3(p) ((uint16_t)(*((p)+2)))           /* uint16_t */

/* -- Tailset element fields. --------------------------------------- */

#define TAILSET_SIZE 1
#define TAILSET_CH(p)       ((char)((*(p))&0x3F))   /* char */
#define TAILSET_TAILEND(p)  ((uint16_t)((*(p))&0x80))   /* Boolean */



extern int16_t search(KEYTYPE *word, int16_t *wordsize, 
		struct dict_state * dict);

void translate(uchar *word, uchar *table)
{
  uchar *ptr;
  ptr=word;
  while(*ptr)
  *ptr++=table[*ptr];
}

int offset1(uchar ch, struct dict_state *dict)
{
  return (1+(dict->abcSize+1)*VERTP_SIZE*ch);
}

int offset2(uchar ch1, uchar ch2, struct dict_state *dict)
{
  return (1+(dict->abcSize+1)*VERTP_SIZE*ch1+(ch2+1)*VERTP_SIZE);
}
  

int main(int argc, char **argv){
	TDictState dict_state;
        PTDictState dict = &dict_state;
	PTDictHeaderMask dictHdr;
	uint32_t treeLength, tailsLength;
	uint32_t rulesLength, hushLength;
        uchar *buf;
        buf=malloc(1024*1024*20*sizeof(uchar));
        int fd;
        fd=open(argv[1],(O_RDONLY ));
        size_t size=read(fd,buf,1024*1024*20);
        close(fd);

        dictHdr=(PTDictHeaderMask)buf;
	treeLength = strtoul(dictHdr->treeLength, NULL, 10);
	tailsLength = strtoul(dictHdr->tailsLength, NULL, 10);
	rulesLength = strtoul(dictHdr->rulesLength, NULL, 10);
	hushLength = strtoul(dictHdr->hushLength, NULL, 10);

	/* -- Get alphabet size. -- */
	size = strtoul(dictHdr->abcSize, NULL, 10);
	if (size > 64) {
		exit(-1);
	} else {
		dict->abcSize = (uint16_t) size;
	}

	dict->root = (uchar *) dictHdr + sizeof(TDictHeaderMask);
	dict->tailset_root = (uchar *) dict->root + treeLength;
	dict->vartable = (PTTailVar)((uchar *) dict->tailset_root + tailsLength);
	dict->table = (PTShiftType)((uchar *) dict->vartable + rulesLength);
        
        int i;
        int j;

        uchar translate_table[256];
        memset(translate_table,0,256);
        for(i=0; i<dict->abcSize; i++){
          translate_table[(uchar)(dictHdr->abcLower[i])]=i;
          translate_table[(uchar)(dictHdr->abcUpper[i])]=i;
        }        

        FILE *infile;
        infile=fopen(argv[2],"r");
        while(!feof(infile))
        {
          uchar input[256];
          fscanf(infile,"%s",input);
          printf("Input: %s here\n",input);
          uchar *ptr;
          ptr=input;
          while(*ptr)
          {
            if(*ptr==13 || *ptr==10)
              *ptr=0;
             ptr++;
          }
          int16_t len;
          len = strlen(input);
          translate(input,translate_table);
          for(i=0;i<len; i++){
            printf("Input char %d\n",input[i]);
          }
          len--;
          printf("Search result %d\n",search(input,&len,dict)); 
        }
        fclose(infile);
        printf("offset1(10)=%d",offset1(10,dict));
        printf("offset2(10,14)=%d",offset2(10,14,dict));
/*Print leading level info*/
        int pdepth=VERTV_KEY(dict->root);
        printf("Vertex P depth: %d\n",pdepth);
        if(pdepth<2)
          exit(-1);
        int offset;
        uchar * ndptr;
        ndptr=(uchar*)(dict->root+1);

        printf("Abcsize %d\n",dict->abcSize);
        printf("Alphabet: %s\n",dictHdr->abcLower);
        printf("Alphabet: %s\n",&(dictHdr->abcLower[1]));
        for(i=0;i<dict->abcSize;i++)
        {
          ndptr=(uchar*)(dict->root+offset1((uchar)i,dict));
          if(VERTP_EXIST(ndptr))
          {
            printf("%c\n",dictHdr->abcLower[i]);
            if(!VERTP_NOTERM(ndptr)){ printf("Word! ");}
          }
          for(j=0;j<dict->abcSize;j++)
          {
            ndptr=(uchar*)(dict->root+offset2((uchar)i,(uchar)j,dict));
            if(VERTP_EXIST(ndptr))
            {
              if(!VERTP_NOTERM(ndptr)){ printf("Word! ");}
              printf("%c%c\n",dictHdr->abcLower[i],dictHdr->abcLower[j]);
            }
          }
        }
              



        for(i=0;i<rulesLength/sizeof(TTailVar);i++){
          uint32_t mask=dict->vartable[i].tailmask;
          int tableoffset=dict->vartable[i].tablenum;
          int maxlen = dict->vartable[i].maxtaillth;
          uchar* ptr=(uchar*)(dict->tailset_root+dict->table[tableoffset]);
          printf("Analizing table %d maxlen %d\n\n",i,maxlen);
          for(;mask !=0; mask>>=1){
             if((mask&0x01)){
               while(!TAILSET_TAILEND(ptr)){ 
                 printf("%c",dictHdr->abcLower[TAILSET_CH(ptr)]);
                 ptr++;
               };
               printf("%c",dictHdr->abcLower[TAILSET_CH(ptr)]);
               printf("\n");
             }else{
               while(!TAILSET_TAILEND(ptr))
                 ptr++;
             }
             ptr++;
          }
        }

        free(buf);
} 

