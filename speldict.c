/*
 Copyright (c) 1993-2008, Cognitive Technologies
 All rights reserved.
*/

#include <unistd.h>
#include <stdlib.h>
#include "speldict.h"
#include <stdio.h>
#define OK 1
#define FAIL 0
extern const uchar abcUpper[];
extern const uchar abcLower[];

/* ------ Export section. ------------------------------------------- */
/* -- Code -- */

int16_t search(KEYTYPE *word, int16_t *wordsize, 
		struct dict_state * dict);

/*************************************************************************/
/*                Locals section.                                        */
/*************************************************************************/
/* -- Code -- */

static int16_t
		analyse(int16_t * account, int16_t * wordsize,  int16_t * found);
static int16_t next_level(KEYTYPE ch);
static int16_t test_tail(uchar * ptr, int16_t * accounter, int16_t * tailmaxl,
		 int16_t * tailfound);

static TShiftType brother2(uchar * ptr);

static int16_t comp_tail(int16_t varnum, int16_t rest, int16_t cnt, KEYTYPE * wptr,
		int16_t * tailmaxl,  int16_t * tailfound);

/* -- Macro -- */

#define HAVE_BLANK 2

/* -- Data -- */

static PTDictState dictInfo; /* dictionary control structure  */

static KEYTYPE *wptr; /* pointer to working word       */
static int16_t rest; /* lth of word rest              */
static int16_t cnt; /* character counter             */
static TShiftType lthorshift; /* lth of curr VERTV or shift to */

static int16_t poslevel; /* current level in pos. tree if indpos==1  */
static int16_t indpos; /* 1-ndptr points to pos. tree,0-otherwise  */
static int16_t maxlevel; /* max num of level in current pos. tree    */

static uchar *ndptr; /* pointer to current vertex in  */

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

/*************************************************************************/
/*                                                                       */
/*    Function search() sifts input word through the dictionary.         */
/*                                                                       */
/*************************************************************************/

int16_t search(KEYTYPE *word, int16_t *wordsize, 
		struct dict_state * dict) {
	int16_t found = 0;
	int16_t state; /* state of search in dictionary */
	int16_t account = 0; /* word statistic                */
	int16_t full_size = *wordsize; /* ATAL, save word size          */

	dictInfo = dict; /* initial values of prossess    */
	ndptr = dict->root;
	indpos = 1;
	wptr = word;
	poslevel = 0;
	maxlevel = VERTV_KEY(ndptr);

	for (cnt = 0, rest = *wordsize; rest >= 0; ++cnt, --rest) {
		if (!next_level(word[cnt])) {
			if (!found) {
				*wordsize = cnt;
				goto Fail;
			} else
				goto Success;
		}
		state = analyse(&account, wordsize, &found);
		if (state != 0) { /* -- word found or break is in need -- */
			if (((state == 1) && (!found)) || state == 2)
				break;
		}
	}

	if (state == 1 || found)
		goto Success;
	else
		goto Fail;

	Fail: return FAIL; /* <==> return (0)        */

	Success: return account; /* return word statistic  */

}

/*************************************************************************/
/*  The function next_level() cnanges the global pointer *ndptr so that  */
/*  *ndptr pointing to vertex of next level of dictionary tree with the  */
/*  key equals functiuon's parameter ch. If the vertex exists, function  */
/*  returns 1, otherwise 0 is returned.                                  */
/*************************************************************************/

int16_t next_level(KEYTYPE ch) {
	TShiftType shift;

        printf("next_level called with %d as argument\n",ch);

	Begin: if (indpos)
		if (poslevel != maxlevel) {
			if (maxlevel > 2) {
				register uint16_t i;
				for (i = maxlevel - poslevel - 1, shift = 1L; i > 0; --i) {
					shift *= dictInfo->abcSize;
					shift++;
				}
			} else if (poslevel == 0) {
				shift = dictInfo->abcSize + 1L;
				ndptr -= 2;
			} else {
				shift = 1L;
			}
                        printf("  poslevel=%d shift = %d \n",poslevel,shift);

			ndptr += (ch * shift + 1) * VERTP_SIZE;
                        printf("  ndptr-dict->root=%d\n",(int)(ndptr-dictInfo->root));
			poslevel++;

			if (VERTP_EXIST(ndptr) != 0)
				goto Success;
			else
				goto Fail;
		} else {
                        printf("  poslevel=%d lthorshift=%d ndptr_before-dictInfo->root=%d\n",poslevel,lthorshift,(int)(ndptr-dictInfo->root));
			ndptr += lthorshift;
                        printf("  ndptr-dict->root=%d\n",(int)(ndptr-dictInfo->root));
			indpos = 0;
			lthorshift = 0L;
			goto Begin;
		}
	else { /* -- if ( ! indpos ) -> pointer tree */
		ndptr += lthorshift;

		while (VERTV_KEY(ndptr) < ch) {
			shift = brother2(ndptr);
			if (shift == 0L)
				goto Fail;
			/* it was the last brother ? */
			ndptr += shift;
		}

		if (VERTV_KEY(ndptr) == ch)
			goto Success;
		else
			goto Fail;
	}

	Success: return OK;

	Fail: return FAIL;

}

/**************************************************************************/
/*       Function brother2() returns the value of shift to next (right)   */
/* brother.If VERTV.lth==0 && VERTV.cont==0 , then function returns zero. */
/* If VERTV.lth==1 && shift==0L (vertex is terminal in a word) , then     */
/* function cntinues searching to next(right) brother.                    */
/**************************************************************************/

TShiftType brother2(uchar * ptr) {
	TShiftType shift = 0;

	if (VERTV_CONT(ptr) == 0) {
		return 0; /* continuation absent  */
	}

	ptr += VERTV_SIZE; /* skip vertex itself */

	while (POSTFICS_TAIL(ptr) != 0 /* skip account and postfics */
	&& POSTFICS_CONT(ptr) != 0) {
		if (POSTFICS_ACCNT(ptr) != 0)
			ptr += ACCOUNT_SIZE;
		else
			ptr += POSTFICS_SIZE;
	}

	if (ADDR_TAIL(ptr) == 0) {
		shift = ADDR_SHIFT0(ptr);
		if (ADDR_LTH(ptr) != 0) {
			shift <<= 7;
			shift += ADDR_SHIFT2 ( ptr );
			if (ADDR_LTH2(ptr) != 0) {
				shift <<= 8;
				shift += ADDR_SHIFT3 ( ptr );
			}
		}
	}

	return shift;
}

/*************************************************************************/
/*    Function analyse() calculates pointer *ptr : *ptr pointing to      */
/*  vertex continuation,test tails and test the status of vertex -       */
/*  terminal in tree or not.                                             */
/*************************************************************************/

int16_t analyse(int16_t * account, int16_t * wordsize, int16_t * found) {
	int16_t tailmaxl = 0;
	uchar * ptr; /* serv pointer: points to vertex cont. */
        
        printf("calling analyse *account=%d, *wordsize=%d *found=%d\n",*account,*wordsize,*found);

	if (indpos)
        {
                printf("  We are at VERTP its offset is %d rest=%d\n",(int)(ndptr-dictInfo->root),rest); 
		lthorshift = ((TShiftType) VERTP_SHIFT0(ndptr) << 16)
				+ ((TShiftType) VERTP_SHIFT1(ndptr) << 8) + VERTP_SHIFT2(ndptr);
                printf("  Preliminary lthorshift=%d\n",lthorshift);
        }
	else
		lthorshift = 1; /*lth of VERTV vertex */

	ptr = ndptr + lthorshift;

	if (VERTV_CONT(ndptr) != 0 && rest >= 0) {
                printf("   VERTV_CONT!=0 for ndptr rest=%d\n",rest);
		int16_t ret = test_tail(ptr, account, &tailmaxl, found);
                printf("   Result of test_tail ret=%d found=%d account = %d\n",ret,*found,*account);
		if (ret && !*found) {
			goto Success;
		}
	}

	if (rest == 0) { /* test the end of word, if current letter  */
		goto Fail;
		/* is the last letter in the word.          */
	} else { /* the current letter isn't the last.       */
		if (VERTV_NOTERM(ndptr) != 0) {
			goto GoNext;
		} else {
			if (*found)
				goto Fail;
			else
				goto Failcnt;
		}
	}

	GoNext: return 0; /* go to the next vertex in tree         */
	Success: return 1; /* the word is in dictionary             */
	Failcnt: if (!(VERTV_CONT(ndptr) != 0 && rest >= 0))
		*wordsize = cnt; /* continue searching impossible         */
	else
		*wordsize = cnt + tailmaxl + 1;
	/* Test variant : cnt points for the current letter.
	 We test tails hence we are working with the next letter:
	 1 is in need.
	 */
	Fail: return 2;
}

/**********************************************************************/
/*       Function test_tail(void *ptr) analize element of vertex      */
/*    continuation, calculates length of vertex in global variable    */
/* lthorshift and calls function comp_tail(),if element is tail       */
/*                            variant.                                */
/*       The order of potfics takes a great importance:               */
/*               ->  LONGADDR,ACCOUNT,POSTFICS !                      */
/**********************************************************************/

int16_t test_tail(uchar * ptr, int16_t * accounter, int16_t * tailmaxl, 
		int16_t * tailfound) {
	int16_t contflag = 0;
	int16_t tailscnter = 0;
	int16_t accntcnter = 0;
	uint32_t wCount = 0;

	do {
		contflag = POSTFICS_CONT(ptr);
		if (POSTFICS_TAIL(ptr)) {
			if (!POSTFICS_ACCNT(ptr)) {
				if (rest != 0) {
					/* -- don't check tails if no rest. */
					uint16_t enterNum;
					enterNum = (POSTFICS_ENTER0(ptr) << 8)
							+ POSTFICS_ENTER1(ptr);

					if (comp_tail(enterNum, rest, cnt, wptr, tailmaxl, 
							tailfound)) {
						goto Success;
					}
				}
				tailscnter++;
				ptr += POSTFICS_SIZE;
			} else {
				if (!*tailfound) {
					*accounter = ACCOUNT_FREQ(ptr);
					if (rest == 0 && ACCOUNT_WRDTERM(ptr) != 0) {
						goto Success;
					}
				}
				accntcnter++;
				ptr += ACCOUNT_SIZE;
			}
		} else {
			if (ADDR_LTH(ptr) == 0) {
				lthorshift += 1;
				ptr += 1;
			} else {
				if (ADDR_LTH2(ptr) == 0) {
					lthorshift += 2;
					ptr += 2;
				} else {
					lthorshift += 3;
					ptr += 3;
				}
			}
		}
		/////////
		wCount++;
		/////////
	} while (contflag);

	lthorshift += tailscnter * POSTFICS_SIZE + accntcnter * ACCOUNT_SIZE;

	if (*tailfound == 0) {
		return FAIL;
	}

	Success: return OK;
}

/*************************************************************************/
/*    Function comp_tail() compares the end of word pointed *wptr with   */
/* the tail variant with the number varnum.                              */
/*************************************************************************/

#define MASK 0x1L

int16_t comp_tail(int16_t varnum, int16_t rest, int16_t cnt, KEYTYPE * wptr, int16_t * tailmaxl,
		 int16_t * tailfound) {
	register int16_t j, k;
	uchar * ptr;
	uchar * tailptr;
	uint32_t mask;

	if (dictInfo->vartable[varnum].maxtaillth < rest) {
		return FAIL;
	}
        printf("Called comp_tail varnum=%d rest=%d\n",varnum,rest);

	ptr = dictInfo->tailset_root
			+ dictInfo->table[dictInfo->vartable[varnum].tablenum];
	mask = dictInfo->vartable[varnum].tailmask;
	for (; mask != 0; mask >>= 1) {
		if ((MASK & mask) != 0) {
			tailptr = ptr, j = cnt + 1, k = rest - 1;
                        printf("  Testing tail: ");
                        int ii;
                        ii=0;
                        while(!TAILSET_TAILEND(((uchar*)(ptr+ii)))){
                          printf("%c",abcLower[TAILSET_CH((((uchar*)(ptr+ii))))]);
                          ii++;
                        }
                        printf("%c ",abcLower[TAILSET_CH((((uchar*)(ptr+ii))))]);

                        printf("\n");
			do {
					if (wptr[j] != TAILSET_CH(ptr)) {
						if (*tailmaxl <= j - cnt - 2) {
							*tailmaxl = j - cnt - 1;
						}
						break;
					}else{
                                          printf("   Matched tail character %d\n",wptr[j]);
                                        }
				/*-------------------------------------------------------------------------*/
				if (TAILSET_TAILEND(ptr) && k == 0) { /* end of word & tail */
						return OK;
				}
				if (TAILSET_TAILEND(ptr)) {
					break;
				}
				/*------------------------------------------------------------------------*/
				j++, k--, ptr++;
			} while (k >= 0);
		}

		while (TAILSET_TAILEND(ptr) == 0)
			ptr++;
		ptr++;
	}

	return FAIL;
}

