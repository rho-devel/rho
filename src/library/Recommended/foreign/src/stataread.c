/*
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/**
  Read  Stata version 8.0, 7.0, 7/SE, 6.0 and 5.0 .dta files, write version 7.0, 6.0.

  (c) 1999, 2000, 2001, 2002 Thomas Lumley.
  2000 Saikat DebRoy

  The format of Stata files is documented under 'file formats'
  in the Stata manual.

  This code currently does not make use of the print format information in
   a .dta file (except for dates). It cannot handle files with 'int'
  'float' or 'double' that differ from IEEE 4-byte integer, 4-byte
  real and 8-byte real respectively: it's not clear whether such files
  can exist.

  Versions of Stata before 4.0 used different file formats.

**/


#include <stdio.h>
#include <stdlib.h> /* for abs */
#include "foreign.h"
#include "swap_bytes.h"

/* versions */
#define VERSION_5 0x69
#define VERSION_6 'l'
#define VERSION_7 0x6e
#define VERSION_7SE 111
#define VERSION_8 113
#define VERSION_114 114
#define VERSION_115 115

/*
http://statacorp.com/statalist/archive/2007-06/msg01021.html
says 113 is versions 8-9, 114 is version 10.
*/

/* Stata format constants */
#define STATA_FLOAT  'f'
#define STATA_DOUBLE 'd'
#define STATA_INT    'l'
#define STATA_SHORTINT 'i'
#define STATA_BYTE  'b'

#define STATA_SE_STRINGOFFSET 0
#define STATA_SE_FLOAT  254
#define STATA_SE_DOUBLE 255
#define STATA_SE_INT    253
#define STATA_SE_SHORTINT 252
#define STATA_SE_BYTE  251

#define STATA_STRINGOFFSET 0x7f

#define STATA_BYTE_NA 127
#define STATA_SHORTINT_NA 32767
#define STATA_INT_NA 2147483647

#define STATA_FLOAT_NA pow(2.0, 127)
#define STATA_DOUBLE_NA pow(2.0, 1023)

static int stata_endian;


/** Low-level input **/

static int InIntegerBinary(FILE * fp, int naok, int swapends)
{
    int i;
    if (fread(&i, sizeof(int), 1, fp) != 1)
	error(_("a binary read error occurred"));
    if (swapends)
	reverse_int(i);
    return ((i==STATA_INT_NA) & !naok ? NA_INTEGER : i);
}
/* read a 1-byte signed integer */
static int InByteBinary(FILE * fp, int naok)
{
    signed char i;
    if (fread(&i, sizeof(char), 1, fp) != 1)
	error(_("a binary read error occurred"));
    return  ((i==STATA_BYTE_NA) & !naok ? NA_INTEGER : (int) i);
}
/* read a single byte  */
static int RawByteBinary(FILE * fp, int naok)
{
    unsigned char i;
    if (fread(&i, sizeof(char), 1, fp) != 1)
	error(_("a binary read error occurred"));
    return  ((i==STATA_BYTE_NA) & !naok ? NA_INTEGER : (int) i);
}

static int InShortIntBinary(FILE * fp, int naok,int swapends)
{
	unsigned first,second;
	int result;

  first = RawByteBinary(fp,1);
  second = RawByteBinary(fp,1);
  if (stata_endian == CN_TYPE_BIG){
    result= (first<<8) | second;
  } else {
    result= (second<<8) | first;
  }
  if (result>STATA_SHORTINT_NA) result-=65536;
  return ((result==STATA_SHORTINT_NA) & !naok ? NA_INTEGER  : result);
}


static double InDoubleBinary(FILE * fp, int naok, int swapends)
{
    double i;
    if (fread(&i, sizeof(double), 1, fp) != 1)
	error(_("a binary read error occurred"));
    if (swapends)
	reverse_double(i);
    return ((i==STATA_DOUBLE_NA) & !naok ? NA_REAL : i);
}

static double InFloatBinary(FILE * fp, int naok, int swapends)
{
    float i;
    if (fread(&i, sizeof(float), 1, fp) != 1)
	error(_("a binary read error occurred"));
    if (swapends)
	reverse_float(i);
    return ((i==STATA_FLOAT_NA) & !naok ? NA_REAL :  (double) i);
}

static void InStringBinary(FILE * fp, int nchar, char* buffer)
{
    if (fread(buffer, nchar, 1, fp) != 1)
	error(_("a binary read error occurred"));
}

/** now optional and done at R level **/
static char* nameMangle(char *stataname, int len){
  return stataname;
}
/**static char* nameMangle(char *stataname, int len){
    int i;
    for(i=0;i<len;i++)
      if (stataname[i]=='_') stataname[i]='.';
    return stataname;
}
**/

/*****
      Turn a .dta file into a data frame
      Variable labels go to attributes of the data frame, value labels go to factor levels

     characteristics could go as attributes of the variables
      not yet implemented
****/



SEXP R_LoadStataData(FILE *fp)
{
    int i, j = 0, nvar, nobs, charlen, version, swapends, 
	varnamelength, nlabels, totlen, res;
    unsigned char abyte;
    /* timestamp is used for timestamp and for variable formats */
    char datalabel[81], timestamp[50], aname[33];
    char stringbuffer[245], *txt;
    SEXP df, names, tmp, varlabels, types, row_names;
    SEXP levels, labels, labeltable, sversion;
    int *off;
    int fmtlist_len = 12;


    /** first read the header **/

    abyte = RawByteBinary(fp, 1);   /* release version */
    version = 0;			/* -Wall */
    varnamelength = 0;		/* -Wall */
    labeltable = R_NilValue;	/* -Wall */
    switch (abyte) {
    case VERSION_5:
	version = 5;
	varnamelength = 8;
	break;
    case VERSION_6:
	version = 6;
	varnamelength = 8;
	break;
    case VERSION_7:
	version = 7;
	varnamelength = 32;
	break;
    case VERSION_7SE:
	version = -7;
	varnamelength = 32;
	break;
    case VERSION_8:
	version = -8;  /* version 8 automatically uses SE format */
	varnamelength = 32;
	break;
    case VERSION_114:
	version = -10;
	varnamelength = 32;
	fmtlist_len = 49;
    case VERSION_115:
	/* Stata say the formats are identical,
	   but _115 allows business dates */
	version = -12;
	varnamelength = 32;
	fmtlist_len = 49;
	break;
    default:
	error(_("not a Stata version 5-12 .dta file"));
    }
    stata_endian = (int) RawByteBinary(fp, 1);     /* byte ordering */
    swapends = stata_endian != CN_TYPE_NATIVE;

    RawByteBinary(fp, 1);            /* filetype -- junk */
    RawByteBinary(fp, 1);            /* padding */
    nvar = (InShortIntBinary(fp, 1, swapends)); /* number of variables */
    nobs = (InIntegerBinary(fp, 1, swapends));  /* number of cases */
    /* data label - zero terminated string */
    switch (abs(version)) {
    case 5:
	InStringBinary(fp, 32, datalabel);
	break;
    case 6:
    case 7:
    case 8:
    case 10:
    case 12:
	InStringBinary(fp, 81, datalabel);
	break;
    }
    /* file creation time - zero terminated string */
    InStringBinary(fp, 18, timestamp);

    /** make the data frame **/

    PROTECT(df = allocVector(VECSXP, nvar));

    /** and now stick the labels on it **/

    PROTECT(tmp = allocVector(STRSXP, 1));
    SET_STRING_ELT(tmp, 0, mkChar(datalabel));
    setAttrib(df, install("datalabel"), tmp);
    UNPROTECT(1);

    PROTECT(tmp = allocVector(STRSXP, 1));
    SET_STRING_ELT(tmp, 0, mkChar(timestamp));
    setAttrib(df, install("time.stamp"), tmp);
    UNPROTECT(1);


    /** read variable descriptors **/

    /** types **/

    PROTECT(types = allocVector(INTSXP, nvar));
    if (version > 0){
	for(i = 0; i < nvar; i++){
	    abyte = RawByteBinary(fp, 1);
	    INTEGER(types)[i] = abyte;
	    switch (abyte) {
	    case STATA_FLOAT:
	    case STATA_DOUBLE:
		SET_VECTOR_ELT(df, i, allocVector(REALSXP, nobs));
		break;
	    case STATA_INT:
	    case STATA_SHORTINT:
	    case STATA_BYTE:
		SET_VECTOR_ELT(df, i, allocVector(INTSXP, nobs));
		break;
	    default:
		if (abyte < STATA_STRINGOFFSET)
		    error(_("unknown data type"));
		SET_VECTOR_ELT(df, i, allocVector(STRSXP, nobs));
		break;
	    }
	}
    } else {
	for(i = 0; i < nvar; i++){
	    abyte = RawByteBinary(fp, 1);
	    INTEGER(types)[i] = abyte;
	    switch (abyte) {
	    case STATA_SE_FLOAT:
	    case STATA_SE_DOUBLE:
		SET_VECTOR_ELT(df, i, allocVector(REALSXP, nobs));
		break;
	    case STATA_SE_INT:
	    case STATA_SE_SHORTINT:
	    case STATA_SE_BYTE:
		SET_VECTOR_ELT(df, i, allocVector(INTSXP, nobs));
		break;
	    default:
		if (abyte > 244)
		    error(_("unknown data type"));
		SET_VECTOR_ELT(df, i, allocVector(STRSXP, nobs));
		break;
	    }
	}
    }

    /** names **/

    PROTECT(names = allocVector(STRSXP, nvar));
    for (i = 0; i < nvar; i++) {
	InStringBinary(fp, varnamelength+1, aname);
	SET_STRING_ELT(names, i, mkChar(nameMangle(aname, varnamelength+1)));
    }
    setAttrib(df, R_NamesSymbol, names);
    UNPROTECT(1);

    /** sortlist -- not relevant **/

    for (i = 0; i < 2*(nvar+1); i++) RawByteBinary(fp, 1);

    /** format list
	passed back to R as attributes.
	Used to identify date variables.
    **/

    PROTECT(tmp = allocVector(STRSXP, nvar));
    for (i = 0; i < nvar; i++) {
	InStringBinary(fp, fmtlist_len, timestamp);
	SET_STRING_ELT(tmp, i, mkChar(timestamp));
    }
    setAttrib(df, install("formats"), tmp);
    UNPROTECT(1);
    setAttrib(df, install("types"), types);


    /** value labels.  These are stored as the names of label formats,
	which are themselves stored later in the file. **/

    PROTECT(tmp = allocVector(STRSXP, nvar));
    for(i = 0; i < nvar; i++) {
	InStringBinary(fp, varnamelength+1, aname);
	SET_STRING_ELT(tmp ,i, mkChar(aname));
    }
    setAttrib(df,install("val.labels"), tmp);
    UNPROTECT(1); /*tmp*/

    /** Variable Labels **/

    PROTECT(varlabels=allocVector(STRSXP,nvar));

    switch(abs(version)){
    case 5:
	for(i = 0; i < nvar; i++) {
	    InStringBinary(fp, 32, datalabel);
	    SET_STRING_ELT(varlabels, i, mkChar(datalabel));
	}
	break;
    case 6:
    case 7:
    case 8:
    case 10:
    case 12:
	for(i = 0; i < nvar; i++) {
	    InStringBinary(fp, 81, datalabel);
	    SET_STRING_ELT(varlabels, i, mkChar(datalabel));
	}
    }
    setAttrib(df, install("var.labels"), varlabels);

    UNPROTECT(1);

    /* Expansion Fields. These include
       variable/dataset 'characteristics' (-char-)
       variable/dataset 'notes' (-notes-)
       variable/dataset/values non-current language labels (-label language-)
    */

    PROTECT(labeltable = allocVector(VECSXP, 0));
    j = 0;
    while(RawByteBinary(fp, 1)) {
	if (abs(version) >= 7) /* manual is wrong here */
	    charlen = (InIntegerBinary(fp, 1, swapends));
	else
	    charlen = (InShortIntBinary(fp, 1, swapends));
	
	if((charlen > 66)) {
	    labeltable = lengthgets(labeltable, j+1);
	    UNPROTECT(1);
	    PROTECT(labeltable);
	    PROTECT(tmp = allocVector(STRSXP, 3));
	    InStringBinary(fp, 33, datalabel);
	    SET_STRING_ELT(tmp, 0, mkChar(datalabel));
	    InStringBinary(fp, 33, datalabel);
	    SET_STRING_ELT(tmp, 1, mkChar(datalabel));
	    txt =  Calloc((size_t) (charlen-66), char);
	    InStringBinary(fp, (charlen-66), txt);
	    SET_STRING_ELT(tmp, 2, mkChar(txt));
	    SET_VECTOR_ELT(labeltable, j, tmp);
	    Free(txt);
	    UNPROTECT(1);
	    j++;
	} else
	    for (i = 0; i < charlen; i++) InByteBinary(fp, 1);
    }
    if(j > 0)
	setAttrib(df, install("expansion.fields"), labeltable);

    UNPROTECT(1); //labeltable
    if (abs(version) >= 7)
	charlen = (InIntegerBinary(fp, 1, swapends));
    else
	charlen = (InShortIntBinary(fp, 1, swapends));
    if (charlen != 0)
	error(_("something strange in the file\n (Type 0 characteristic of nonzero length)"));


    /** The Data **/

    if (version > 0) { /* not Stata/SE */
	for(i = 0; i < nobs; i++){
	    for(j = 0; j < nvar; j++){
		switch (INTEGER(types)[j]) {
		case STATA_FLOAT:
		    REAL(VECTOR_ELT(df,j))[i] = InFloatBinary(fp, 0, swapends);
		    break;
		case STATA_DOUBLE:
		    REAL(VECTOR_ELT(df,j))[i] = InDoubleBinary(fp, 0, swapends);
		    break;
		case STATA_INT:
		    INTEGER(VECTOR_ELT(df,j))[i] = InIntegerBinary(fp, 0, swapends);
		    break;
		case STATA_SHORTINT:
		    INTEGER(VECTOR_ELT(df,j))[i] = InShortIntBinary(fp, 0, swapends);
		    break;
		case STATA_BYTE:
		    INTEGER(VECTOR_ELT(df,j))[i] = (int) InByteBinary(fp, 0);
		    break;
		default:
		    charlen = INTEGER(types)[j] - STATA_STRINGOFFSET;
		    if(charlen > 244) {
			warning("invalid character string length -- truncating to 244 bytes");
			charlen = 244;
		    }
		    InStringBinary(fp, charlen, stringbuffer);
		    stringbuffer[charlen] = 0;
		    SET_STRING_ELT(VECTOR_ELT(df, j), i, mkChar(stringbuffer));
		    break;
		}
	    }
	}
    }  else {
	for(i = 0; i < nobs; i++){
	    for(j = 0;j < nvar; j++){
		switch (INTEGER(types)[j]) {
		case STATA_SE_FLOAT:
		    REAL(VECTOR_ELT(df,j))[i] = InFloatBinary(fp, 0, swapends);
		    break;
		case STATA_SE_DOUBLE:
		    REAL(VECTOR_ELT(df,j))[i] = InDoubleBinary(fp, 0, swapends);
		    break;
		case STATA_SE_INT:
		    INTEGER(VECTOR_ELT(df,j))[i] = InIntegerBinary(fp, 0, swapends);
		    break;
		case STATA_SE_SHORTINT:
		    INTEGER(VECTOR_ELT(df,j))[i] = InShortIntBinary(fp, 0, swapends);
		    break;
		case STATA_SE_BYTE:
		    INTEGER(VECTOR_ELT(df,j))[i] = (int) InByteBinary(fp, 0);
		    break;
		default:
		    charlen = INTEGER(types)[j]-STATA_SE_STRINGOFFSET;
		    if(charlen > 244) {
			warning("invalid character string length -- truncating to 244 bytes");
			charlen = 244;
		    }
		    InStringBinary(fp, charlen, stringbuffer);
		    stringbuffer[charlen] = 0;
		    SET_STRING_ELT(VECTOR_ELT(df,j), i, mkChar(stringbuffer));
		    break;
		}
	    }
	}
    }


    /** value labels **/
    if (abs(version) > 5) {
	PROTECT(labeltable = allocVector(VECSXP, 0));
	PROTECT(tmp = allocVector(STRSXP, 0));
	for(j = 0; ; j++) {
	    /* first int not needed, use fread directly to trigger EOF */
	    res = fread((int *) aname, sizeof(int), 1, fp);
	    if (feof(fp)) break;
	    if (res != 1) warning(_("a binary read error occurred"));

	    //resize the vectors
	    labeltable = lengthgets(labeltable, j+1);
	    UNPROTECT(1);
	    PROTECT(labeltable);
	    tmp = lengthgets(tmp, j+1);
	    UNPROTECT(1);
	    PROTECT(tmp);

	    InStringBinary(fp, varnamelength+1, aname);
	    SET_STRING_ELT(tmp, j, mkChar(aname));
	    RawByteBinary(fp, 1); RawByteBinary(fp, 1); RawByteBinary(fp, 1); /*padding*/
	    nlabels = InIntegerBinary(fp, 1, swapends);
	    totlen = InIntegerBinary(fp, 1, swapends);
	    off =  Calloc((size_t) nlabels, int);
	    PROTECT(levels = allocVector(INTSXP, nlabels));
	    PROTECT(labels = allocVector(STRSXP, nlabels));
	    for(i = 0; i < nlabels; i++)
		off[i] = InIntegerBinary(fp, 1, swapends);
	    for(i = 0; i < nlabels; i++)
		INTEGER(levels)[i] = (double) InIntegerBinary(fp, 0, swapends);
	    txt =  Calloc((size_t) totlen, char);
	    InStringBinary(fp, totlen, txt);
	    for(i = 0; i < nlabels; i++)
		SET_STRING_ELT(labels, i, mkChar(txt+off[i]));
	    namesgets(levels, labels);
	    SET_VECTOR_ELT(labeltable, j, levels);
	    Free(off);
	    Free(txt);
	    UNPROTECT(2);/* levels, labels */
	}
	namesgets(labeltable, tmp);
	UNPROTECT(1); /*tmp*/
    }

    /** tidy up **/

    PROTECT(row_names = allocVector(STRSXP, nobs));
    for (i = 0; i < nobs; i++) {
	sprintf(datalabel, "%d", i+1);
	SET_STRING_ELT(row_names,i,mkChar(datalabel));
    }
    setAttrib(df, R_RowNamesSymbol, row_names);
    UNPROTECT(1);

    PROTECT(sversion = allocVector(INTSXP,1));
    INTEGER(sversion)[0] = (version == -7)? version : abs(version);
    setAttrib(df, install("version"), sversion);
    UNPROTECT(1);

    if (abs(version) > 5) {
	if(j > 0) setAttrib(df, install("label.table"), labeltable);
	UNPROTECT(1); /*labeltable*/;
    }
    UNPROTECT(2); /* types, df */
    return(df);
}

#include <string.h>
#include <errno.h>

SEXP do_readStata(SEXP call)
{
    SEXP fname, result;
    FILE *fp;

    if ((sizeof(double)!=8) | (sizeof(int)!=4) | (sizeof(float)!=4))
      error(_("can not yet read Stata .dta on this platform"));


    if (!isValidString(fname = CADR(call)))
	error(_("first argument must be a file name\n"));

    fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(fname,0))), "rb");
    if (!fp)
	error(_("unable to open file: '%s'"), strerror(errno));

    result = R_LoadStataData(fp);
    fclose(fp);
    return result;
}


/** low level output **/

static void OutIntegerBinary(int i, FILE * fp, int naok)
{
    i=((i==NA_INTEGER) & !naok ? STATA_INT_NA : i);
    if (fwrite(&i, sizeof(int), 1, fp) != 1)
	error(_("a binary write error occurred"));

}

static void OutByteBinary(unsigned char i, FILE * fp)
{
    if (fwrite(&i, sizeof(char), 1, fp) != 1)
	error(_("a binary write error occurred"));
}
static void OutDataByteBinary(int i, FILE * fp)
{
    i=(unsigned char) ((i==NA_INTEGER) ? STATA_BYTE_NA : i);
    if (fwrite(&i, sizeof(char), 1, fp) != 1)
	error(_("a binary write error occurred"));
}

static void OutShortIntBinary(int i,FILE * fp)
{
  unsigned char first,second;

#ifdef WORDS_BIGENDIAN
    first= (i>>8);
    second=i & 0xff;
#else
    first=i & 0xff;
    second=i>>8;
#endif
  if (fwrite(&first, sizeof(char), 1, fp) != 1)
    error(_("a binary write error occurred"));
  if (fwrite(&second, sizeof(char), 1, fp) != 1)
    error(_("a binary write error occurred"));
}


static void  OutDoubleBinary(double d, FILE * fp, int naok)
{
    d=(R_FINITE(d) ? d : STATA_DOUBLE_NA);
    if (fwrite(&d, sizeof(double), 1, fp) != 1)
	error(_("a binary write error occurred"));
}


static void OutStringBinary(const char *buffer, FILE * fp, int nchar)
{
    if (nchar==0) return;
    if (fwrite(buffer, nchar, 1, fp) != 1)
	error(_("a binary write error occurred"));
}

static char* nameMangleOut(char *stataname, int len)
{
    for(int i = 0; i < len; i++) {
      if (stataname[i] == '.') stataname[i] = '_';
    }
    return stataname;
}

/* Writes out a value label (name, and then labels and levels). 
 * theselevels can be R_NilValue in which case the level values will be
 * written out as 1,2,3, ...
 */
static Rboolean 
writeStataValueLabel(const char *labelName, const SEXP theselabels,
		     const SEXP theselevels, const int namelength, FILE *fp)
{
    int i,len,txtlen; 

    if(!isString(theselabels))
	return FALSE;

    if (!isNull(theselevels) && 
	((TYPEOF(theselevels)!=INTSXP && TYPEOF(theselevels)!=REALSXP)  || 
	 LENGTH(theselabels)!=LENGTH(theselevels)))
	return FALSE;

    len = 4*2*(length(theselabels)+1);
    txtlen = 0;
    for (i = 0; i < length(theselabels); i++)
	txtlen += strlen(CHAR(STRING_ELT(theselabels, i))) + 1;
    len += txtlen;
    OutIntegerBinary(len, fp, 0); /* length of table */
    char labelName2[strlen(labelName) + 1];
    strcpy(labelName2, labelName);
    OutStringBinary(nameMangleOut(labelName2, namelength), fp, namelength);
    OutByteBinary(0, fp); /* label format name */
    OutByteBinary(0, fp); OutByteBinary(0, fp); OutByteBinary(0, fp); /*padding*/
    OutIntegerBinary(length(theselabels), fp, 0);
    OutIntegerBinary(txtlen, fp, 0);
    /* offsets */
    len = 0;
    for (i = 0; i < length(theselabels); i++){
	OutIntegerBinary(len, fp, 0);
	len += strlen(CHAR(STRING_ELT(theselabels,i))) + 1;
    }
    /* values: just 1,2,3,...*/
    if(isNull(theselevels)){
	for (i = 0; i < length(theselabels); i++)
	    OutIntegerBinary(i+1, fp, 0);
    }
    else{
	if(TYPEOF(theselevels)==INTSXP){
		for (i = 0; i < length(theselevels); i++)
		    OutIntegerBinary(INTEGER(theselevels)[i], fp, 0);
	}
	else{
		for (i = 0; i < length(theselevels); i++)
		    OutIntegerBinary((int) REAL(theselevels)[i], fp, 0);
	}
    }
    /* the actual labels */
    for(i = 0; i < length(theselabels); i++){
	len = strlen(CHAR(STRING_ELT(theselabels, i)));
	OutStringBinary(CHAR(STRING_ELT(theselabels,i)), fp, len);
	OutByteBinary(0, fp);
	txtlen -= len+1;
	if (txtlen < 0) error(_("this should happen: overrun"));
    }
    if (txtlen > 0) error(_("this should happen: underrun"));

    return TRUE;

}

void R_SaveStataData(FILE *fp, SEXP df, int version, SEXP leveltable)
{
    int i,j,k=0,l,nvar,nobs,charlen;
    char datalabel[81]="Written by R.              ", timestamp[18], aname[33];
    char format9g[50]="%9.0g", strformat[50]="";
    const char *thisnamechar;
    SEXP names,types,theselabels,orig_names,vlabels,dlabel,exp_fields,exp_field,curr_val_labels,label_table,names_lt,theselabelslevels;

    int namelength = 8;
    int fmtlist_len = 12;

    if (version >= 7) namelength=32;
    if (version >= 10) fmtlist_len = 49;

    /* names are 32 characters in version 7 */

    /** first write the header **/
    if (version == 6)
	OutByteBinary((char) VERSION_6, fp);            /* release */
    else if (version == 7)
	OutByteBinary((char) VERSION_7, fp);
    else if (version == 8)  /* and also 9, mapped in R code */
	OutByteBinary((char) VERSION_8, fp);
    else if (version == 10) /* see comment above */
	OutByteBinary((char) VERSION_114, fp);
    OutByteBinary((char) CN_TYPE_NATIVE, fp);
    OutByteBinary(1, fp);            /* filetype */
    OutByteBinary(0, fp);            /* padding */

    nvar = length(df);
    OutShortIntBinary(nvar, fp);
    nobs = length(VECTOR_ELT(df, 0));
    OutIntegerBinary(nobs, fp, 1);  /* number of cases */

    PROTECT(dlabel = getAttrib(df, install("datalabel")));
    if(!isNull(dlabel) && isString(dlabel) && LENGTH(dlabel) == 1)
	strncpy(datalabel, CHAR(STRING_ELT(dlabel, 0)), 81);
    UNPROTECT(1);
    datalabel[80] = (char) 0;
    OutStringBinary(datalabel, fp, 81);


    /* FIXME: use a real time */
    for(i = 0; i < 18; i++) timestamp[i] = 0;
    OutStringBinary(timestamp,fp,18);   /* file creation time - zero terminated string */



    /** write variable descriptors **/

    /** types **/
    /* FIXME: writes everything as double or integer to save effort*/
    /*  we should honor the "Csingle" attribute and also write logicals as
	byte rather than long */

    PROTECT(types = allocVector(INTSXP,nvar));
    if (version <= 7) {
	for(i = 0;i < nvar; i++){
	    switch(TYPEOF(VECTOR_ELT(df, i))){
	    case LGLSXP:
		OutByteBinary(STATA_BYTE, fp);
		break;
	    case INTSXP:
		OutByteBinary(STATA_INT, fp);
		break;
	    case REALSXP:
		OutByteBinary(STATA_DOUBLE, fp);
		break;
	    case STRSXP:
		/* NB: there is a 244 byte limit on strings */
		charlen = 0;
		for(j = 0; j < nobs; j++){
		    k = strlen(CHAR(STRING_ELT(VECTOR_ELT(df, i), j)));
		    if (k > charlen) charlen = k;
		}
		if(charlen > 244)
		    warning("character strings of >244 bytes in column %d will be truncated", i+1);
		charlen =  (charlen < 244) ? charlen : 244;
		OutByteBinary((unsigned char)(charlen+STATA_STRINGOFFSET), fp);
		INTEGER(types)[i] = charlen;
		break;
	    default:
		error(_("unknown data type"));
		break;
	    }
	}
    } else { /* version 8, 10 */
	for(i = 0; i < nvar; i++){
	    switch(TYPEOF(VECTOR_ELT(df, i))){
	    case LGLSXP:
		OutByteBinary(STATA_SE_BYTE,fp);
		break;
	    case INTSXP:
		OutByteBinary(STATA_SE_INT,fp);
		break;
	    case REALSXP:
		OutByteBinary(STATA_SE_DOUBLE,fp);
		break;
	    case STRSXP:
		/* NB: there is a 244 byte limit on strings */
		charlen = 0;
		for(j = 0;j < nobs; j++){
		    k = strlen(CHAR(STRING_ELT(VECTOR_ELT(df, i),j)));
		    if (k > charlen) charlen = k;
		}
		if(charlen > 244)
		    warning("character strings of >244 bytes in column %d will be truncated", i+1);
		charlen =  (charlen < 244) ? charlen : 244;
		OutByteBinary((unsigned char)(charlen+STATA_SE_STRINGOFFSET), fp);
		INTEGER(types)[i] = charlen;
		break;
	    default:
		error(_("unknown data type"));
		break;
	    }
	}
    }
    /** names truncated to 8 (or 32 for v>=7) characters**/

    PROTECT(names = getAttrib(df, R_NamesSymbol));
    for (i = 0; i < nvar;i ++){
	strncpy(aname, CHAR(STRING_ELT(names, i)), namelength);
	OutStringBinary(nameMangleOut(aname, namelength), fp, namelength);
	OutByteBinary(0, fp);
    }



    /** sortlist -- not relevant **/
    for (i = 0; i < 2*(nvar+1); i++) OutByteBinary(0, fp);

    /** format list: arbitrarily write numbers as %9g format
	but strings need accurate types */
    for (i = 0; i < nvar; i++) {
	if (TYPEOF(VECTOR_ELT(df,i)) == STRSXP){
	    /* string types are at most 244 characters
	       so we can't get a buffer overflow in sprintf **/
	    sprintf(strformat,"%%%ds",INTEGER(types)[i]);
	    OutStringBinary(strformat, fp, fmtlist_len);
	} else {
	    OutStringBinary(format9g, fp, fmtlist_len);
	}
    }

    /** value labels.  These are stored as the names of label formats,
	which are themselves stored later in the file.
	The label format has the same name as the variable. **/

    PROTECT(curr_val_labels = getAttrib(df, install("val.labels")));
    for(i = 0; i < nvar; i++) {
	if (VECTOR_ELT(leveltable, i) == R_NilValue){ /* no label */
	    for(j = 0; j < namelength+1; j++) OutByteBinary(0, fp);
	} else {                                   /* label */
            //If we remember what the value label was called, use that. Otherwise use the var name
	    if(!isNull(curr_val_labels) && isString(curr_val_labels) 
	       && LENGTH(curr_val_labels) > i)
	    	strncpy(aname, CHAR(STRING_ELT(curr_val_labels, i)), namelength);
	    else
		strncpy(aname, CHAR(STRING_ELT(names, i)), namelength);
	    OutStringBinary(nameMangleOut(aname, namelength), fp, namelength);
	    OutByteBinary(0, fp);
	}
    }
    UNPROTECT(1);


    /** Variable Labels -- Uses "var.labels" attribute
	if is a string vector of the right length, otherwise the
	the variable name (FIXME: this is now just the same abbreviated name) **/
    PROTECT(vlabels = getAttrib(df, install("var.labels")));
    if(!isNull(vlabels) && isString(vlabels) && LENGTH(vlabels)==nvar){
        for(i = 0; i < nvar; i++) {
	    strncpy(datalabel,CHAR(STRING_ELT(vlabels,i)),81);
	    datalabel[80] = (char) 0;
	    OutStringBinary(datalabel, fp, 81);
        }
    }
    else{
        PROTECT(orig_names = getAttrib(df,install("orig.names")));
        for(i = 0; i < nvar; i++) {
	    strncpy(datalabel,CHAR(STRING_ELT(orig_names,i)),81);
	    datalabel[80] = (char) 0;
	    OutStringBinary(datalabel, fp, 81);
        }
        UNPROTECT(1);
    }
    UNPROTECT(1);


    /** Expansion fields. Only existing ones are type-1 fields (first byte is 1).
	This includes dataset/variables characteristics, multilingual data (dataset/variable labels, 
	value label attachments, language list), and notes. 
	Don't have documentation for pre-version7 format (are the first two fields still 33 bytes?) **/
    PROTECT(exp_fields = getAttrib(df, install("expansion.fields")));
    if(!isNull(exp_fields) && TYPEOF(exp_fields) == VECSXP && abs(version)>=7){
	for(i = 0; i< LENGTH(exp_fields); i++){
		PROTECT(exp_field = VECTOR_ELT(exp_fields, i));	
		if(!isNull(exp_field) && isString(exp_field) && LENGTH(exp_field) ==3) {
		    OutByteBinary(1, fp);
		    OutIntegerBinary(2*(namelength+1) + (length(STRING_ELT(exp_field,2))+1), fp, 1);

		    OutStringBinary(CHAR(STRING_ELT(exp_field,0)), fp, namelength);
		    OutByteBinary(0, fp);
		    OutStringBinary(CHAR(STRING_ELT(exp_field,1)), fp, namelength);
		    OutByteBinary(0, fp);
		    OutStringBinary(CHAR(STRING_ELT(exp_field,2)), fp, length(STRING_ELT(exp_field,2)));
		    OutByteBinary(0, fp);
		}
		UNPROTECT(1);
	}
    }
    UNPROTECT(1);
    //The last block is always zeros
    OutByteBinary(0, fp);
    OutByteBinary(0, fp);
    OutByteBinary(0, fp);
    if (version >= 7) { /*longer in version 7. This is wrong in the manual*/
	OutByteBinary(0, fp);
	OutByteBinary(0, fp);
    }


    /** The Data **/
    for(i = 0; i < nobs; i++){
	for(j = 0;j < nvar; j++){
	    switch (TYPEOF(VECTOR_ELT(df, j))) {
	    case LGLSXP:
		OutDataByteBinary(LOGICAL(VECTOR_ELT(df,j))[i], fp);
		break;
	    case INTSXP:
		OutIntegerBinary(INTEGER(VECTOR_ELT(df,j))[i], fp, 0);
		break;
	    case REALSXP:
		OutDoubleBinary(REAL(VECTOR_ELT(df,j))[i], fp, 0);
		break;
	    case STRSXP:
		/* Up to 244 bytes should be written, zero-padded */
		k = length(STRING_ELT(VECTOR_ELT(df, j), i));
		if (k == 0)
		    error("empty string is not valid in Stata's documented format");
		if(k > 244) k = 244;
		OutStringBinary(CHAR(STRING_ELT(VECTOR_ELT(df, j), i)), fp, k);
		for(l = INTEGER(types)[j]-k; l > 0; l--) OutByteBinary(0, fp);
		break;
	    default:
		error(_("this should not happen."));
		break;
	    }
	}
    }

    /** value labels: pp92-94 of 'Programming' manual in v7.0 **/

    PROTECT(curr_val_labels = getAttrib(df, install("val.labels")));
    for(i = 0;i < nvar; i++){
	theselabels = VECTOR_ELT(leveltable, i);
	if (!isNull(theselabels)){
            //If we remember what the value label was called, use that. Otherwise use the var name
	    if(!isNull(curr_val_labels) && isString(curr_val_labels) && LENGTH(curr_val_labels)>i)
	    	strncpy(aname, CHAR(STRING_ELT(curr_val_labels, i)), namelength);
	    else
		strncpy(aname, CHAR(STRING_ELT(names, i)), namelength);

	    writeStataValueLabel(aname, theselabels, R_NilValue, namelength, fp);
	}
    }
    PROTECT(label_table = getAttrib(df, install("label.table")));
    if(TYPEOF(label_table) == VECSXP){
	PROTECT(names_lt = getAttrib(label_table, R_NamesSymbol));
	if(!isNull(names_lt) && LENGTH(label_table)==LENGTH(names_lt)){
		for(i=0; i<LENGTH(label_table); i++){
			thisnamechar = CHAR(STRING_ELT(names_lt,i));
			//check it this label was noted in val.labels because it would've been already written
			if(!isNull(curr_val_labels) && isString(curr_val_labels)){
				for(j=0; j<LENGTH(curr_val_labels); j++){
					if(strncmp(thisnamechar, CHAR(STRING_ELT(curr_val_labels, j)), namelength)==0)
						break;
				}
				if(j<LENGTH(curr_val_labels)) continue;
			}
			//check to 
			theselabelslevels = VECTOR_ELT(label_table, i);
			if(!isNull(theselabelslevels)){
				PROTECT(theselabels = getAttrib(theselabelslevels, R_NamesSymbol));
				writeStataValueLabel(thisnamechar, theselabels, theselabelslevels, namelength, fp);
				UNPROTECT(1); /* theselabel */
			}
		}
	}
	UNPROTECT(1); /* names_lt */
    }

    UNPROTECT(4); /* names,types,curr_val_labels, label_table */
}

SEXP do_writeStata(SEXP call)
{
    SEXP fname, df, leveltable;
    FILE *fp;
    int version;

    if ((sizeof(double) != 8) | (sizeof(int) != 4) | (sizeof(float) != 4))
      error(_("cannot yet read write .dta on this platform"));


    if (!isValidString(fname = CADR(call)))
	error(_("first argument must be a file name\n"));

    df = CADDR(call);
    if (!inherits(df,"data.frame"))
	error(_("data to be saved must be in a data frame"));

    fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(fname,0))), "wb");
    if (!fp) error(_("unable to open file for writing: '%s'"), strerror(errno));

    version = INTEGER(coerceVector(CADDDR(call), INTSXP))[0];
    /* 9 is mapped to 8 in R code */
    if ((version < 6) || (version > 10))
	error(_("can only write version 6-10 formats"));
    leveltable = CAD4R(call);

    R_SaveStataData(fp,df,version,leveltable);
    fclose(fp);
    return R_NilValue;
}
