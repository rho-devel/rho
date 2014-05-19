/*
 *
 *  Read SAS transport data set format
 *
 *  Copyright 1999-1999 Douglas M. Bates <bates@stat.wisc.edu>,
 *                      Saikat DebRoy <saikat@stat.wisc.edu>
 *
 *
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

#include <stdio.h>
#include <string.h>
#include <R.h>
#include <Rinternals.h>
#include "foreign.h"
#include "SASxport.h"

#define HEADER_BEG "HEADER RECORD*******"
#define HEADER_TYPE_LIBRARY "LIBRARY "
#define HEADER_TYPE_MEMBER "MEMBER  "
#define HEADER_TYPE_DSCRPTR "DSCRPTR "
#define HEADER_TYPE_NAMESTR "NAMESTR "
#define HEADER_TYPE_OBS "OBS     "
#define HEADER_END "HEADER RECORD!!!!!!!000000000000000000000000000000  "

#define LIB_HEADER HEADER_BEG HEADER_TYPE_LIBRARY HEADER_END
#define MEM_HEADER HEADER_BEG HEADER_TYPE_MEMBER \
		   "HEADER RECORD!!!!!!!000000000000000001600000000"
#define DSC_HEADER HEADER_BEG HEADER_TYPE_DSCRPTR HEADER_END
#define NAM_HEADER HEADER_BEG HEADER_TYPE_NAMESTR \
		   "HEADER RECORD!!!!!!!000000"
#define OBS_HEADER HEADER_BEG HEADER_TYPE_OBS HEADER_END
#define BLANK24 "                        "

#define GET_RECORD(rec, fp, len) \
	  fread((rec), sizeof(char), (size_t) (len), (fp))

#define IS_SASNA_CHAR(c) ((c) == 0x5f || (c) == 0x2e || \
			  (0x41 <= (c) && (c) <= 0x5a))

#ifndef NULL
#define NULL ((void *) 0)
#endif


#define Two32 4294967296.0

static double get_IBM_double(char* c, size_t len)
{
				/* Conversion from IBM 360 format to double */
/*
 * IBM format:
 * 6       5                 0
 * 3       1                 0
 *
 * SEEEEEEEMMMM ......... MMMM
 *
 * Sign bit, 7 bit exponent, 56 bit fraction. Exponent is
 * excess 64. The fraction is multiplied by a power of 16 of
 * the actual exponent. Normalized floating point numbers are
 * represented with the radix point immediately to the left of
 * the high order hex fraction digit.
 */
    unsigned int i, upper, lower;
				/* exponent is expressed here as
				   excess 70 (=64+6) to accomodate
				   integer conversion of c[1] to c[4] */
    char negative = c[0] & 0x80, exponent = (c[0] & 0x7f) - 70, buf[4];
    double value;
    char ibuf[8];

    if (len < 2 || len > 8)
      error(_("invalid field length in numeric variable"));

    /* this effectively zero-pads c: */
    memset(ibuf, 0, (size_t) 8);
    memcpy(ibuf, c, len);
    c = ibuf;
    /* check for missing value */
    /* This isn't really right: NAs are ' ',  '.', A-Z plus zero fill */
    if (c[1] == '\0' && c[0] != '\0') return R_NaReal;
    /* convert c[1] to c[3] to an int */
    buf[0] = '\0';
    for (i = 1; i < 4; i++) buf[i] = c[i];
    char_to_uint(buf, upper);
    /* convert c[4] to c[7] to an int */
    for (i = 0; i < 4; i++) buf[i] = c[i + 4];
    char_to_uint(buf, lower);
    /* initialize the constant if needed */
    value = ((double) upper + ((double) lower)/Two32) *
	pow(16., (double) exponent);
    if (negative) value = -value;
    return value;
}

static int
get_nam_header(FILE *fp, struct SAS_XPORT_namestr *namestr, int length)
{
    char record[141];
    int n;

    record[length] = '\0';
    n = GET_RECORD(record, fp, length);
    if(n != length)
	return 0;

    char_to_short(record, namestr->ntype);
    char_to_short(record+2, namestr->nhfun);
    char_to_short(record+4, namestr->nlng);
    char_to_short(record+6, namestr->nvar0);
    memcpy(namestr->nname, record + 8, 8);
    memcpy(namestr->nlabel, record + 16, 40);
    memcpy(namestr->nform, record + 56, 8);
    char_to_short(record+64, namestr->nfl);
    char_to_short(record+66, namestr->nfd);
    char_to_short(record+68, namestr->nfj);
    memcpy(namestr->nfill, record + 70, 2);
    memcpy(namestr->niform, record + 72, 8);
    char_to_short(record+80, namestr->nifl);
    char_to_short(record+82, namestr->nifd);
    char_to_int(record+84, namestr->npos);
    return 1;
}

static int
get_lib_header(FILE *fp, struct SAS_XPORT_header *head)
{
    char record[81];
    int n;

    n = GET_RECORD(record, fp, 80);

    /* GRW: Fail if not enough bytes OR wrong header record */
    if(n != 80 || strncmp(LIB_HEADER, record, 80) != 0)
	error(_("file not in SAS transfer format"));

    n = GET_RECORD(record, fp, 80);
    if(n != 80)
	return 0;
    record[80] = '\0';
    memcpy(head->sas_symbol[0], record, 8);
    memcpy(head->sas_symbol[1], record+8, 8);
    memcpy(head->saslib, record+16, 8);
    memcpy(head->sasver, record+24, 8);
    memcpy(head->sas_os, record+32, 8);
    if((strrchr(record+40, ' ') - record) != 63)
	return 0;
    memcpy(head->sas_create, record+64, 16);

    n = GET_RECORD(record, fp, 80);
    if(n != 80)
	return 0;
    record[80] = '\0';
    memcpy(head->sas_mod, record, 16);

    /*GRW: The remaining field is an optional dataset label, 
      which may contain data. */
    /*if((strrchr(record+16, ' ') - record) != 79)*/
    /*return 0;*/
    return 1;
}

static int
get_mem_header(FILE *fp, struct SAS_XPORT_member *member)
{
    char record[81];
    int n;

    n = GET_RECORD(record, fp, 80);
    if(n != 80 || strncmp(DSC_HEADER, record, 80) != 0)
	error(_("file not in SAS transfer format"));

    n = GET_RECORD(record, fp, 80);
    if(n != 80)
	return 0;
    record[80] = '\0';
    memcpy(member->sas_symbol, record, 8);
    memcpy(member->sas_dsname, record+8, 8);
    memcpy(member->sasdata, record+16, 8);
    memcpy(member->sasver, record+24, 8);
    memcpy(member->sas_osname, record+32, 8);
    if((strrchr(record+40, ' ') - record) != 63)
	return 0;
    memcpy(member->sas_create, record+64, 16);

    n = GET_RECORD(record, fp, 80);
    if(n != 80)
	return 0;
    memcpy(member->sas_mod, record, 16);
    if((strrchr(record+16, ' ') - record) != 79)
	return 0;
    return 1;
}

static int
init_xport_info(FILE *fp)
{
    char record[81];
    int n;
    int namestr_length;

    struct SAS_XPORT_header *lib_head;

    lib_head = Calloc(1, struct SAS_XPORT_header);

    if(!get_lib_header(fp, lib_head)) {
	Free(lib_head);
	error(_("SAS transfer file has incorrect library header"));
    }

    Free(lib_head);

    n = GET_RECORD(record, fp, 80);
    if(n != 80 || strncmp(MEM_HEADER, record, 75) != 0 ||
       strncmp("  ", record+78, 2) != 0)
	error(_("file not in SAS transfer format"));
    record[78] = '\0';
    sscanf(record+75, "%d", &namestr_length);

    return namestr_length;
}

static int
init_mem_info(FILE *fp, char *name)
{
    int length, n;
    char record[81];
    char *tmp;
    struct SAS_XPORT_member  *mem_head;

    mem_head = Calloc(1, struct SAS_XPORT_member);
    if(!get_mem_header(fp, mem_head)) {
	Free(mem_head);
	error(_("SAS transfer file has incorrect member header"));
    }

    n = GET_RECORD(record, fp, 80);
    record[80] = '\0';
    if(n != 80 || strncmp(NAM_HEADER, record, 54) != 0 ||
       (strrchr(record+58, ' ') - record) != 79) {
	Free(mem_head);
	error(_("file not in SAS transfer format"));
    }
    record[58] = '\0';
    sscanf(record+54, "%d", &length);

    tmp = strchr(mem_head->sas_dsname, ' ');
    n = tmp - mem_head->sas_dsname;
    if(n > 0) {
	if (n > 8)
	    n = 8;
	strncpy(name, mem_head->sas_dsname, n);
	name[n] = '\0';
    } else name[0] = '\0';

    Free(mem_head);

    return length;
}

static int
next_xport_info(FILE *fp, int namestr_length, int nvars, int *headpad,
		int *tailpad, int *length, int *ntype, int *nlng,
		int *nvar0, SEXP nname, SEXP nlabel, SEXP nform, int *npos)
{
    char *tmp;
    char record[81];
    int i, n, totwidth, nlength, restOfCard;
    struct SAS_XPORT_namestr *nam_head;

    nam_head = Calloc(nvars, struct SAS_XPORT_namestr);

    for(i = 0; i < nvars; i++) {
	if(!get_nam_header(fp, nam_head+i, namestr_length)) {
	    Free(nam_head);
	    error(_("SAS transfer file has incorrect library header"));
	}
    }

    *headpad = 480 + nvars * namestr_length;
    i = *headpad % 80;
    if(i > 0) {
	i = 80 - i;
	if (fseek(fp, i, SEEK_CUR) != 0) {
	    Free(nam_head);
	    error(_("file not in SAS transfer format"));
	}
	(*headpad) += i;
    }

    n = GET_RECORD(record, fp, 80);
    if(n != 80 || strncmp(OBS_HEADER, record, 80) != 0) {
	Free(nam_head);
	error(_("file not in SAS transfer format"));
    }

    for(i = 0; i < nvars; i++) {
	int nname_len = 0, nlabel_len = 0, nform_len = 0;
	char tmpname[41];

	ntype[i] = (int) ((nam_head[i].ntype == 1) ? REALSXP : STRSXP);
	nlng[i]  = nam_head[i].nlng;
	nvar0[i] = nam_head[i].nvar0;
	npos[i]  = nam_head[i].npos;

	/* Variable name */
	nname_len = 8;
	while (nname_len && nam_head[i].nname[nname_len-1] == ' ')
	    nname_len--;
	strncpy(tmpname, nam_head[i].nname, nname_len);
	tmpname[nname_len] = '\0';
	SET_STRING_ELT(nname, i, mkChar(tmpname));

	/* Variable label */
	nlabel_len = 40;
	while (nlabel_len && nam_head[i].nlabel[nlabel_len-1] == ' ')
	    nlabel_len--;
	strncpy(tmpname, nam_head[i].nlabel, nlabel_len);
	tmpname[nlabel_len] = '\0';
	SET_STRING_ELT(nlabel, i, mkChar(tmpname));

	/* Variable format name */
	nform_len = 8;
	while (nform_len && nam_head[i].nform[nform_len-1] == ' ')
	    nform_len--;
	strncpy(tmpname, nam_head[i].nform, nform_len);
	tmpname[nform_len] = '\0';
	SET_STRING_ELT(nform, i, mkChar(tmpname));
    }

    Free(nam_head);

    totwidth = 0;
    for(i = 0; i < nvars; i++)
	totwidth += nlng[i];

    nlength = 0;
    tmp = Calloc(totwidth <= 80 ? 81 : (totwidth+1), char);
    restOfCard = 0;
    *tailpad = 0;
    while(!feof(fp)) {
	int allSpace = 1;
	fpos_t currentPos;

/*	restOfCard = 80 - (ftell(fp) % 80); */
	if (fgetpos(fp, &currentPos)) {
	    error(_("problem accessing SAS XPORT file"));
	}

	n = GET_RECORD(tmp, fp, restOfCard);
	if (n != restOfCard) {
	    allSpace = 0;
	} else {
	    for (i = 0; i < restOfCard; i++) {
		if (tmp[i] != ' ') {
		    allSpace = 0;
		    break;
		}
	    }
	}
	if (allSpace) {
	    n = GET_RECORD(record, fp, 80);
	    if (n < 1) {
		*tailpad = restOfCard;
		break;
	    }
	    if(n == 80 && strncmp(MEM_HEADER, record, 75) == 0 &&
	       strncmp("  ", record+78, 2) == 0) {
		*tailpad = restOfCard;
		record[78] = '\0';
		sscanf(record+75, "%d", &namestr_length);
		break;
	    }
	}
	else /* beware that the previous member can end on card
	      * boundary with no padding */
	    if (restOfCard == 80 && n == 80 &&
		strncmp(MEM_HEADER, tmp, 75) == 0 &&
		strncmp("  ", tmp+78, 2) == 0) {
		strncpy(record, tmp, 80);
		*tailpad = 0;
		record[78] = '\0';
		sscanf(record+75, "%d", &namestr_length);
		break;
	    }

	if (fsetpos(fp, &currentPos)) {
	    error(_("problem accessing SAS XPORT file"));
	}

	n = GET_RECORD(tmp, fp, totwidth);
	if (n != totwidth) {
	    if (!feof(fp)) {
		error(_("problem accessing SAS XPORT file"));
	    }
	    *tailpad = n;
	    break;
	}
	restOfCard = (restOfCard >= totwidth)?
	    (restOfCard - totwidth):
	    (80 - (totwidth - restOfCard)%80);
	nlength++;
    }
    *length = nlength;
    Free(tmp);

    return (feof(fp)?-1:namestr_length);
}

/*
 * get the list element named str.
 */

static SEXP
getListElement(SEXP list, char *str) {
    SEXP names;
    SEXP elmt = (SEXP) NULL;
    const char *tempChar;
    int i;

    names = getAttrib(list, R_NamesSymbol);

    for (i = 0; i < LENGTH(list); i++) {
	tempChar = CHAR(STRING_ELT(names, i));
	if( strcmp(tempChar,str) == 0) {
	    elmt = VECTOR_ELT(list, i);
	    break;
	}
    }
    return elmt;
}

#define VAR_INFO_LENGTH 11

const char *cVarInfoNames[] = {
    "headpad",
    "type",
    "width",
    "index",
    "position",
    "name",
    "label",
    "format",
    "sexptype",
    "tailpad",
    "length"
};

#define XPORT_VAR_HEADPAD(varinfo)   VECTOR_ELT(varinfo, 0)
#define XPORT_VAR_TYPE(varinfo)      VECTOR_ELT(varinfo, 1)
#define XPORT_VAR_WIDTH(varinfo)     VECTOR_ELT(varinfo, 2)
#define XPORT_VAR_INDEX(varinfo)     VECTOR_ELT(varinfo, 3)
#define XPORT_VAR_POSITION(varinfo)  VECTOR_ELT(varinfo, 4)
#define XPORT_VAR_NAME(varinfo)      VECTOR_ELT(varinfo, 5)
#define XPORT_VAR_LABEL(varinfo)     VECTOR_ELT(varinfo, 6)
#define XPORT_VAR_FORM(varinfo)      VECTOR_ELT(varinfo, 7)
#define XPORT_VAR_SEXPTYPE(varinfo)  VECTOR_ELT(varinfo, 8)
#define XPORT_VAR_TAILPAD(varinfo)   VECTOR_ELT(varinfo, 9)
#define XPORT_VAR_LENGTH(varinfo)    VECTOR_ELT(varinfo, 10)

#define SET_XPORT_VAR_HEADPAD(varinfo, val)   SET_VECTOR_ELT(varinfo, 0, val)
#define SET_XPORT_VAR_TYPE(varinfo, val)      SET_VECTOR_ELT(varinfo, 1, val)
#define SET_XPORT_VAR_WIDTH(varinfo, val)     SET_VECTOR_ELT(varinfo, 2, val)
#define SET_XPORT_VAR_INDEX(varinfo, val)     SET_VECTOR_ELT(varinfo, 3, val)
#define SET_XPORT_VAR_POSITION(varinfo, val)  SET_VECTOR_ELT(varinfo, 4, val)
#define SET_XPORT_VAR_NAME(varinfo, val)      SET_VECTOR_ELT(varinfo, 5, val)
#define SET_XPORT_VAR_LABEL(varinfo, val)     SET_VECTOR_ELT(varinfo, 6, val)
#define SET_XPORT_VAR_FORM(varinfo, val)      SET_VECTOR_ELT(varinfo, 7, val)
#define SET_XPORT_VAR_SEXPTYPE(varinfo, val)  SET_VECTOR_ELT(varinfo, 8, val)
#define SET_XPORT_VAR_TAILPAD(varinfo, val)   SET_VECTOR_ELT(varinfo, 9, val)
#define SET_XPORT_VAR_LENGTH(varinfo, val)    SET_VECTOR_ELT(varinfo, 10, val)

#include <string.h>
#include <errno.h>

SEXP
xport_info(SEXP xportFile)
{
    FILE *fp;
    int i, namestrLength, memLength, ansLength;
    char dsname[9];
    SEXP ans, ansNames, varInfoNames, varInfo;
    SEXP char_numeric, char_character;

    PROTECT(varInfoNames = allocVector(STRSXP, VAR_INFO_LENGTH));
    for(i = 0; i < VAR_INFO_LENGTH; i++)
	SET_STRING_ELT(varInfoNames, i, mkChar(cVarInfoNames[i]));

    PROTECT(char_numeric   = mkChar("numeric"));
    PROTECT(char_character = mkChar("character"));

    if(!isValidString(xportFile))
	error(_("first argument must be a file name"));
    fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(xportFile, 0))), "rb");
    if (!fp)
	error(_("unable to open file: '%s'"), strerror(errno));
    namestrLength = init_xport_info(fp);

    ansLength = 0;
    PROTECT(ans = allocVector(VECSXP, 0));
    PROTECT(ansNames = allocVector(STRSXP, 0));

    /*GRW: File may contain *empty* datasets, so don't rely on
      namestrLength==0 or memLength==0 to determine when to stop looping. */
    while(!feof(fp))
      {
	memLength = init_mem_info(fp, dsname);

	PROTECT(varInfo = allocVector(VECSXP, VAR_INFO_LENGTH));
	setAttrib(varInfo, R_NamesSymbol, varInfoNames);

	SET_XPORT_VAR_TYPE(varInfo, allocVector(STRSXP, memLength));
	SET_XPORT_VAR_WIDTH(varInfo, allocVector(INTSXP, memLength));
	SET_XPORT_VAR_INDEX(varInfo, allocVector(INTSXP, memLength));
	SET_XPORT_VAR_POSITION(varInfo, allocVector(INTSXP, memLength));
	SET_XPORT_VAR_NAME(varInfo, allocVector(STRSXP, memLength));
	SET_XPORT_VAR_LABEL(varInfo, allocVector(STRSXP, memLength));
	SET_XPORT_VAR_FORM(varInfo, allocVector(STRSXP, memLength));
	SET_XPORT_VAR_SEXPTYPE(varInfo, allocVector(INTSXP, memLength));
	SET_XPORT_VAR_HEADPAD(varInfo, allocVector(INTSXP, 1));
	SET_XPORT_VAR_TAILPAD(varInfo, allocVector(INTSXP, 1));
	SET_XPORT_VAR_LENGTH(varInfo, allocVector(INTSXP, 1));

	namestrLength =
	    next_xport_info(fp, namestrLength, memLength,
			    INTEGER(XPORT_VAR_HEADPAD(varInfo)),
			    INTEGER(XPORT_VAR_TAILPAD(varInfo)),
			    INTEGER(XPORT_VAR_LENGTH(varInfo)),
			    INTEGER(XPORT_VAR_SEXPTYPE(varInfo)),
			    INTEGER(XPORT_VAR_WIDTH(varInfo)),
			    INTEGER(XPORT_VAR_INDEX(varInfo)),
			    XPORT_VAR_NAME(varInfo),
			    XPORT_VAR_LABEL(varInfo),
			    XPORT_VAR_FORM(varInfo),
			    INTEGER(XPORT_VAR_POSITION(varInfo)));

	for(i = 0; i < memLength; i++) {
	    int *ntype = INTEGER(XPORT_VAR_SEXPTYPE(varInfo));
	    SET_STRING_ELT(XPORT_VAR_TYPE(varInfo), i,
			   (ntype[i] == REALSXP) ? char_numeric :
			   char_character);
	}
	PROTECT(ans = lengthgets(ans, ansLength+1));
	PROTECT(ansNames = lengthgets(ansNames, ansLength+1));
/*	PROTECT(newAns = allocVector(VECSXP, ansLength+1)); */
/*	PROTECT(newAnsNames = allocVector(STRSXP, ansLength+1)); */

/*	for(i = 0; i < ansLength; i++) { */
/*	    SET_VECTOR_ELT(newAns, i, VECTOR_ELT(ans, i)); */
/*	    SET_STRING_ELT(newAnsNames, i, STRING_ELT(ansNames, i)); */
/*	} */
/*	ans = newAns; */
/*	ansNames = newAnsNames; */

	SET_STRING_ELT(ansNames, ansLength, mkChar(dsname));
	SET_VECTOR_ELT(ans, ansLength, varInfo);
	ansLength++;

	UNPROTECT(5);
	PROTECT(ans);
	PROTECT(ansNames);
    }

    setAttrib(ans, R_NamesSymbol, ansNames);
    UNPROTECT(5);
    fclose(fp);
    return ans;
}

SEXP
xport_read(SEXP xportFile, SEXP xportInfo)
{
    int i, j, k, n;
    int nvar;
    int ansLength, dataLength, totalWidth;
    int dataHeadPad, dataTailPad;
    int *dataWidth;
    int *dataPosition;
    SEXPTYPE *dataType;
    char *record, *tmpchar, *c;
    FILE *fp;
    SEXP ans, names, data, dataInfo, dataName;

    ansLength = LENGTH(xportInfo);
    PROTECT(ans = allocVector(VECSXP, ansLength));
    names = getAttrib(xportInfo, R_NamesSymbol);
    setAttrib(ans, R_NamesSymbol, names);

    if(!isValidString(xportFile))
	error(_("first argument must be a file name"));
    fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(xportFile, 0))), "rb");
    if (!fp)
	error(_("unable to open file: '%s'"), strerror(errno));
    if (fseek(fp, 240, SEEK_SET) != 0)
	error(_("problem reading SAS XPORT file '%s'"),
	      CHAR(STRING_ELT(xportFile, 0)));

    for(i = 0; i < ansLength; i++) {
	dataInfo = VECTOR_ELT(xportInfo, i);
	dataName = getListElement(dataInfo, "name");
	nvar = LENGTH(dataName);
	dataLength = asInteger(getListElement(dataInfo, "length"));
	SET_VECTOR_ELT(ans, i, data = allocVector(VECSXP, nvar));
	setAttrib(data, R_NamesSymbol, dataName);
	dataType = (SEXPTYPE *) INTEGER(getListElement(dataInfo, "sexptype"));
	for(j = 0; j < nvar; j++)
	    SET_VECTOR_ELT(data, j, allocVector(dataType[j], dataLength));

	dataWidth = INTEGER(getListElement(dataInfo, "width"));
	dataPosition = INTEGER(getListElement(dataInfo, "position"));

	totalWidth = 0;
	for(j = 0; j < nvar; j++)
	    totalWidth += dataWidth[j];
	record = Calloc(totalWidth + 1, char);

	dataHeadPad = asInteger(getListElement(dataInfo, "headpad"));
	dataTailPad = asInteger(getListElement(dataInfo, "tailpad"));
	fseek(fp, dataHeadPad, SEEK_CUR);

	for(j = 0; j < dataLength; j++) {
	    n = GET_RECORD(record, fp, totalWidth);
	    if(n != totalWidth) {
		error(_("problem reading SAS transport file"));
	    }

	    for(k = nvar-1; k >= 0; k--) {
		tmpchar = record + dataPosition[k];
		if(dataType[k] == REALSXP) {
		    REAL(VECTOR_ELT(data, k))[j] =
			get_IBM_double(tmpchar, dataWidth[k]);
		} else {
		    tmpchar[dataWidth[k]] = '\0';
		    /* strip trailing blanks */
		    c = tmpchar + dataWidth[k];
		    while (c-- > tmpchar && *c == ' ')
			*c ='\0';

		    SET_STRING_ELT(VECTOR_ELT(data, k), j,
				   (c < tmpchar) ? R_BlankString :
				   mkChar(tmpchar));
		}
	    }
	}

	fseek(fp, dataTailPad, SEEK_CUR);

	Free(record);
    }
    UNPROTECT(1);
    fclose(fp);
    return ans;
}
