/*
 *  Read Minitab portable data set format
 *
 *  Copyright 1999-1999 Douglas M. Bates <bates@stat.wisc.edu>,
 *                      Saikat DebRoy <saikat@stat.wisc.edu>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be
 *  useful, but WITHOUT ANY WARRANTY; without even the implied
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE.  See the GNU General Public License for more
 *  details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <R.h>
#include <Rinternals.h>
#include "foreign.h"

#define MTP_BUF_SIZE 85
#define MTB_INITIAL_ENTRIES 10

typedef struct {
    int    type;		/* 3 = column, m = matrix, k = constant */
    int    cnum;		/* column number in the Minitab worksheet */
    int    len;			/* length of column */
    int    dtype;		/* data type: 0 = numeric */
    union {
	double *ndat;
	char   **cdat;
    } dat;
    char   name[9];
} MTBDATC, *MTB;

#define Column 3
#define Matrix 4
#define Constant 2

static				/* trim white space from end of string */
char *strtrim(char *str)
{
    int i;
    for (i = strlen(str) - 1; i >= 0 && isspace((int)str[i]); i--)
	str[i] = '\0';
    return str;
}

static
SEXP MTB2SEXP(MTB mtb[], int len) /* Create a list from a vector of
				     MTB's and Free the MTB storage */
{
    SEXP ans = PROTECT(allocVector(VECSXP,len)),
	names = PROTECT(allocVector(STRSXP, len));
    int i,j;


    for (i = 0; i < len; i++) {
	MTB thisRec = mtb[i];

	SET_STRING_ELT(names, i, mkChar(thisRec->name));
	switch(mtb[i]->dtype) {
	case 0:			/* numeric data */
	    SET_VECTOR_ELT(ans, i, allocVector(REALSXP, mtb[i]->len));
	    Memcpy(REAL(VECTOR_ELT(ans, i)), mtb[i]->dat.ndat, mtb[i]->len);
	    Free(mtb[i]->dat.ndat);
	    break;
	default:
	    if (mtb[i]->type == 4) {
		int nrow = mtb[i]->len / mtb[i]->dtype;
		int ncol = mtb[i]->dtype;
		SEXP aMatrix = PROTECT(allocMatrix(REALSXP,nrow,ncol));

		for (j = 0; j < nrow*ncol; j++) {
		    REAL(aMatrix)[j] = mtb[i]->dat.ndat[j];
		}
		SET_VECTOR_ELT(ans, i, aMatrix);
		Free(mtb[i]->dat.ndat);
		UNPROTECT(1);
	    } else {
		error(_("non-numeric data types are not yet implemented"));
	    }
	}
	Free(mtb[i]);
    }
    Free(mtb);
    setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return(ans);
}
#include <string.h>
#include <errno.h>

SEXP
read_mtp(SEXP fname)
{
    FILE *f;
    char buf[MTP_BUF_SIZE], blank[1], *pres;
    MTB  *mtb, thisRec;
    int i, j, res, nMTB = MTB_INITIAL_ENTRIES;

    PROTECT(fname = asChar(fname));
#ifdef WIN32 /* force text-mode read */
    if ((f = fopen(R_ExpandFileName(CHAR(fname)), "rt")) == NULL)
#else
    if ((f = fopen(R_ExpandFileName(CHAR(fname)), "r")) == NULL)
#endif
	error(_("unable to open file '%s': '%s'"), 
	      CHAR(fname), strerror(errno));
    if ((fgets(buf, MTP_BUF_SIZE, f) == NULL) ||
	strncmp(buf, "Minitab Portable Worksheet ", 27) != 0)
	error(_("file '%s' is not in Minitab Portable Worksheet format"),
	      CHAR(fname));
    pres = fgets(buf, MTP_BUF_SIZE, f);
    if(pres != buf) error(_("file read error"));
    UNPROTECT(1);

    mtb = Calloc(nMTB, MTB);
    for (i = 0; !feof(f); i++) {
	if (i >= nMTB) {
	    nMTB *= 2;
	    mtb = Realloc(mtb, nMTB, MTB);
	}
	thisRec = mtb[i] = Calloc(1, MTBDATC);
	if (sscanf(buf, "%%%7d%7d%7d%7d%c%8c", &(thisRec->type),
		   &(thisRec->cnum), &(thisRec->len),
		   &(thisRec->dtype), blank, thisRec->name) != 6)
	    error(_("first record for entry %d is corrupt"), i+1);
	thisRec->name[8] = '\0';
	strtrim(thisRec->name);	/* trim trailing white space on name */
	switch (thisRec->dtype) {
	case 0:		/* numeric data */
	    thisRec->dat.ndat = Calloc(thisRec->len, double);
	    for (j = 0; j < thisRec->len; j++) {
		res = fscanf(f, "%lg", thisRec->dat.ndat + j);
		if(res == EOF) error(_("file read error"));
	    }
	    break;
	default:
	    if (thisRec->type == 4) { /* we have a matrix so dtype is number of columns */
		thisRec->dat.ndat = Calloc(thisRec->len, double);
		for (j = 0; j < thisRec->len; j++) {
		    res = fscanf(f, "%lg", thisRec->dat.ndat + j);
		    if(res == EOF) error(_("file read error"));
		}
	    } else {
		error(_("non-numeric data types are not yet implemented"));
	    }
	}
	pres = fgets(buf, MTP_BUF_SIZE, f); /* clear rest of current line */
  	if(pres != buf) error(_("file read error"));
	pres = fgets(buf, MTP_BUF_SIZE, f); /* load next line */
	/* don't test here, as we test eof at end of loop */
    }
    return MTB2SEXP(mtb, i);
}
