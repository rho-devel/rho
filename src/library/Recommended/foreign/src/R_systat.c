/*
 *  Copyright (C) 1990-1992, 2004 Roger Bivand
 *  Patches (C) 2004 B. D. Ripley
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
#include <math.h>
#include "foreign.h"

#define MAXVARS	      8192	/* maximum number of variables */
#define MAXLINES	50	/* number of history lines */
#define SYSLABSIZ	12	/* systat label size */
#define	LABELSIZ	12	/* length of variable names and string values */
#define	FORTBUF	       128	/* apparent packet length in .sys files */
#define	MYBUFSIZ     10*72	/* comment length */
#define DMIS	   -1.0e36	/* missing value */

struct SysAction {
	int	_history;
	int	_save;
	char	*history[MAXLINES];
	char	fmt[4];
	FILE	*output;
	FILE	*submit;
};

#define ERRMES		256	/* maximum length of error message */

struct Header {
	short	flag;		/* non-zero for use */
	short	nv,		/* no of variables */
		nd,		/* no of reals */
		nk,		/* no of strings */
		mtype,		/* type of file, rectangular=1 */
		ntype;		/* real type, float=1, double=2 */
	char	*comment;	/* pointer to comment string */
	char	*lab[MAXVARS];	/* array of pointers to variable names */
	FILE	*fd;		/* pointer to file being read/written */
	char	fname[ERRMES];	/* opened file name */
};

struct SysFilev3 {
    struct  Header h;		/* file header */
    short   ithstr[MAXVARS],	/* array of indices to the i'th string
				    variable */
	    ithdb[MAXVARS],	/* array of indices to the i'th real
				    variable */
	    str_offset[MAXVARS];/* for each ithstr[i], if i is
				    undivided and does not terminate in
				    octal 201:201 0, if terminates in
				    201:201 -1, else is equal to the number
				    of bytes beyond 201:201 */

    int     local_offset[MAXVARS]; /* local offset for each variable from
				    the beginning of each record */

    int	    nobs,		/* number of observations */
	    offset,		/* offset from observation to observation */
	    pos;		/* file position at first data record */
};


static void init_use(struct SysFilev3 *);
static void getuse(const char *, struct SysFilev3 *);
static void getlab(struct SysFilev3 *);
static void closeuse(struct SysFilev3 *);
static size_t getshort(short *, FILE *);
static char *getvarnam(int, struct SysFilev3 *);
static void getdbvar(int, double *, struct SysFilev3 *);
static void getsvar(FILE *, char *s, short);
static int getnv(struct SysFilev3 *use);
static int getnd(struct SysFilev3 *use);
static int getnk(struct SysFilev3 *use);
static int isdb(int i, struct SysFilev3 *use);
static int getmtype(struct SysFilev3 *use);
static int isuse(struct SysFilev3 *use);
static int getnobs(struct SysFilev3 *use);
static int getdb(FILE *fd, short type, double *x);
static int getoctal(int *o, FILE *fp);
static size_t getshort(short *sh, FILE *fp);


#include <R.h>
#include <Rinternals.h>
#include <Rconfig.h>

static void swapb(void *result, int size)
{
#ifdef WORDS_BIGENDIAN
    int i;
    char *p = result, tmp;

    if (size == 1) return;
    for (i = 0; i < size/2; i++) {
	tmp = p[i];
	p[i] = p[size - i - 1];
	p[size - i - 1] = tmp;
    }
#endif
}

SEXP readSystat(SEXP file)
{

    SEXP res, resnames, comment;
    int i, j, pc=0;
    struct SysFilev3 *use;
    char str[LABELSIZ+1], msg[256];
    double *x;


    use = (struct SysFilev3 *) R_alloc(1, sizeof(struct SysFilev3));
    init_use(use);
    getuse(CHAR(STRING_ELT(file, 0)), use);
    if (!(getmtype(use) == 1)) {
	sprintf(msg, _("not a rectangular data file (%s mtype is %d)"),
		CHAR(STRING_ELT(file, 0)), getmtype(use));
	error(msg);
    }

    if ((getnd(use) + getnk(use)) != getnv(use))
	error(_("mismatch in numbers of variables"));
    PROTECT(res = allocVector(VECSXP, getnv(use))); pc++;
    for (i = 0; i < getnv(use); i++) {
	if (isdb(i, use) == 0)
	    SET_VECTOR_ELT(res, i, allocVector(REALSXP, getnobs(use)));
	else
	    SET_VECTOR_ELT(res, i, allocVector(STRSXP, getnobs(use)));
    }

    PROTECT(resnames = allocVector(STRSXP, getnv(use))); pc++;
    for (i = 0; i < getnv(use); i++)
	SET_STRING_ELT(resnames, i, mkChar(getvarnam(i, use)));
    setAttrib(res, R_NamesSymbol, resnames);

    if (use->h.comment != NULL) {
	PROTECT(comment = allocVector(STRSXP, 1)); pc++;
	SET_STRING_ELT(comment, 0, mkChar(use->h.comment));
	setAttrib(res, install("comment"), comment);
    }

    x = (double *) R_alloc(getnobs(use), sizeof(double));
    for (i = 0; i < getnv(use); i++) {
	if (isdb(i, use) == 0) {
	    getdbvar(i, x, use);
	    for (j = 0; j < getnobs(use); j++) {
		if (x[j] == (double) DMIS)
		    REAL(VECTOR_ELT(res, i))[j] = NA_REAL;
		else REAL(VECTOR_ELT(res, i))[j] = x[j];
	    }
	} else {
	    for (j = 0; j < getnobs(use); j++) {

		if(fseek(use->h.fd,
			 use->pos + 1L + (use->offset *  j)
			 + use->local_offset[i],
			 SEEK_SET) != 0)
		    error(_("file access error"));

		getsvar(use->h.fd, str,
			use->str_offset[use->ithstr[i]]);

		if (strncmp(str, "            ", 12) != (int) 0)
		    SET_STRING_ELT(VECTOR_ELT(res, i), j, mkChar(str));
		else
		    SET_STRING_ELT(VECTOR_ELT(res, i), j, NA_STRING);
	    }
	}
    }
    closeuse(use);
    UNPROTECT(pc);
    return(res);
}

/* Initialises the values of the SysFilev3 structure */

static void init_use(struct SysFilev3 *use)
{
    int i;

    use->h.nv = 0;
    use->h.nd = 0;
    use->h.nk = 0;
    use->h.mtype = 0;
    use->h.ntype = 0;
    use->h.comment = NULL;
    use->nobs = (int) 0;
    use->offset = (int) 0;
    use->pos = (int) 0;
    for (i = 0; i < MAXVARS; i++) {
	use->ithstr[i] = 0;
	use->ithdb[i] = 0;
	use->str_offset[i] = 0;
	use->local_offset[i] = 0;
    }
    use->h.flag = 0;
}


/*
The function that extracts the information from the systat
file to permit its manipulation, returns an alert message.
The function calculates vital information concerning the
oddities of the MS-Fortran sequential unformatted file
definition, in particular the number of 128 byte blocks
per record, and the positioning of string variables across
block boundaries. It checks for data integrity by fseeking
to the end of the file, and calculating that the number of
observations is integer.
*/

static void getuse(const char *fname, struct SysFilev3 *u)
{
    int i, j, k, db_offset_rec;
    int end;
    char tmp[ERRMES];


    /* open systat file */
    if ((u->h.fd = fopen(fname,  "rb")) == NULL)
	error(_("cannot open file '%s'"), fname);

    strcpy(u->h.fname, fname);

    /* call getlab to collect file header */
    getlab(u);

    j = 0; k = 0;
    for (i = 0; i < u->h.nv; i++) {/* number the respective real and string
				      variables by the '$' in the string
				      variables' names */
	u->ithstr[i] = -1;
	u->ithdb[i] = -1;
	if(strrchr(u->h.lab[i], '$') == NULL)
	    u->ithdb[i] = j++;
	else
	    u->ithstr[i] = k++;
    }

    if (u->h.nd != j || u->h.nk != k)
	error(_("getuse: Failure in variable unpacking"));

    if(getoctal(&k, u->h.fd) != 1) error(_("getuse: File access error"));
    /* get the byte at the front of the first data record/packet */
    if (k < 0201)
	u->offset = (int) k + 2;	/* if less than octal 201 then
					   one packet per record and
					   record length is offset */
    else if (k == 0201) {
	for (i = 0; k == 0201; i++) {	/* if octal 201 then find
					   the number of packets, since
					   k stops being octal 201 at
					   the last packet */
	    if(fseek(u->h.fd, (1 + FORTBUF), SEEK_CUR) != 0)
		error(_("getuse: File access error"));
	    /* seek to beginning of next packet */
	    if(getoctal(&k, u->h.fd) != 1)
		error(_("getuse: File access error"));
	    /* read k */
	}
	u->offset = (int) k + 2 + (i*(FORTBUF+2));

	/* once k is no longer octal 201, the offset will be k, plus
	   its packet bytes, plus i times FORTBUF, the standard packet
	   length plus i times two packet bytes
	*/

	if (u->h.nk > 0) {	/* if there are string variables */

	    db_offset_rec =
		(u->h.nd % (FORTBUF / (u->h.ntype == 1 ? sizeof(float) : sizeof(double))))
		* (u->h.ntype == 1 ? sizeof(float) :
		   sizeof(double));
	    /* find the number of real values in the first
	       packet in which strings begin, and multiply
	       by their size in bytes */

	    for (i=0; i < u->h.nk ; i++) {	/* for each string
						   variable */
		db_offset_rec += LABELSIZ;	/* increment
						   the packet pointer by LABELSIZ */
		u->str_offset[i] = 0;

		if (db_offset_rec > FORTBUF) {
		    /* if the packet pointer exceeds standard packet
		       length then set the string offset to the
		       remainder, and reset the packet pointer */
		    u->str_offset[i] = db_offset_rec % FORTBUF;
		    db_offset_rec = u->str_offset[i];
		}

		else if (db_offset_rec == FORTBUF) {
		    /* if the packet pointer exceeds
		       standard packet length then set the
		       string offset to -1, and reset the
		       packet pointer */
		    u->str_offset[i] = -1;
		    db_offset_rec = 0;
		}
	    }	/* for each string variable */
	}	/* if there were string variables */
    }	/* k == 0201 */
    else {
	sprintf(tmp, _("getuse: byte counter %o octal"), k);
	error(tmp);
    }
    if(fseek(u->h.fd, 0L, SEEK_END) != 0)
	error(_("getuse: File access error"));
    /* seek to end of file */
    end = ftell(u->h.fd);			/* and find value (int) */

    i = 0;
    if(fseek(u->h.fd, -1L, SEEK_CUR) != 0)
	error(_("getuse: File access error"));
    do {
	end--;
	i++;
	if(getoctal(&k, u->h.fd) != 1) {
	    sprintf(tmp, "Getuse: failure reading byte %d", end);
	    error(tmp);
	}
	if(fseek(u->h.fd, -2L, SEEK_CUR) != 0)
	    error(_("getuse: File access error"));
    } while (i < 512 && k == 000);
    if (i >= 512) error(_("getuse: terminal null block"));
    /* Backtrack from end of file over null bytes which
       the operating system may have inserted VMS in particular,
       but not more than a VAX block - normally just does loop once */

    if (k != 0202) {
	sprintf(tmp, "Getuse: last byte = %o octal", k);
	error(tmp);
    }
    /* seek back one byte and check k == 0202 */

    if (((end - (u->pos)) % u->offset) != (int) 0)
	error(_("getuse: non-integer number of observations"));
    /* Check data integrity */

    u->nobs = (end - (u->pos))/u->offset;
    /* calculate number of observations */
    for (i=0, k=0; i < u->h.nv; i++) {
	/* for all variables calculate their
	   offset from the beginning of the
	   record and store in local_offset */
	if (u->ithdb[i] > -1) {	/* if a real */
	    u->local_offset[i] = (int) u->ithdb[i] *
		(u->h.ntype == 1 ? sizeof(float) :
		 sizeof(double))	/* the easy
					   part: number of variables times
					   sizeof real in the file */

		+ (u->ithdb[i] / (FORTBUF / (u->h.ntype == 1 ?
					     sizeof(float) : sizeof(double)))) * 2;
	    /* the odd part: add two bytes for
	       each intra-record packet boundary to
	       the left of this variable */
	}
	else {			/* or a string */

	    u->local_offset[i] = (int) (u->h.nd*(u->h.ntype == 1 ?
						  sizeof(float) : sizeof(double)))
		/* the reals */
		+ (u->h.nd / (FORTBUF / (u->h.ntype == 1 ?
					 sizeof(float) : sizeof(double)))) * 2
		/* and their packet boundaries */
		+ u->ithstr[i]*LABELSIZ
		/* the strings */
		+ k * 2;
	    /* and their packet boundaries */

	    if(u->ithstr[i] >= 0
	       && u->str_offset[u->ithstr[i]] != 0) k++;
	    /* count the number of packet
	       boundaries passed within the strings */

	}	/* reals or strings */
    }
    /* end of local offset calculation */

    u->h.flag = 1;
}	/* getuse */


/*
gets the header of a release 2 or 3 .sys file
from the file pointed at by fd, returns an error
description on failure,
*/
static void getlab(struct SysFilev3 *u)
{

    char mes[ERRMES], tmp1[ERRMES];
    char label[LABELSIZ+1], tmp[LABELSIZ+1];
    char var[30];
    int i, j, o, len, isDollar;

    strcpy(mes, _("getlab: File format unknown"));
    u->h.nd = 0;
    u->h.nk = 0;
    if((fseek(u->h.fd, 0L, SEEK_SET)) != 0)
	error(_("getlab: File access error"));
    /* move to file beginning */

    if(getoctal(&o, u->h.fd) != 1 || o != 0113) {
	sprintf(tmp1, _("getlab: byte 0 = %o octal"), o);
	error(tmp1); }	/* read and throw away zeroth byte=0113 */

    if(getoctal(&o, u->h.fd) != 1 || o != 006) {
	sprintf(tmp1, _("getlab: byte 1 = %o octal"), o);
	error(tmp1); }
    /* read and throw away front of package
       byte=006, i.e. 3 shorts */
/*	fread((short *) &u->h.nv, sizeof(short), 1, u->h.fd); */
    if(getshort(&u->h.nv, u->h.fd) != 1)
	error(_("getlab: File access error"));
    if(getshort(&u->h.mtype, u->h.fd) != 1)
	error(_("getlab: File access error"));
    if(getshort(&u->h.ntype, u->h.fd) != 1)
	error(_("getlab: File access error"));
    if(getoctal(&o, u->h.fd) != 1 || o != 006) {
	sprintf(tmp1, _("getlab: byte 9 = %o octal"), o);
	error(tmp1);}
    /* read and throw away end of package
       byte=006, i.e. 3 shorts */

    if (u->h.ntype != 1 || u->h.ntype != 2) {	/* i.e. version later than 2 */
	/* test changed to accommodate MYSTAT 9/9/91 */
	len = 0;
	do {
	    isDollar = 0;
	    if(getoctal(&o, u->h.fd) != 1 || o != 0110) {
		sprintf(tmp1, _("getlab: comment begin byte = %o"), o);
		error(tmp1); }
	    /* read and throw away
	       front of package byte=0110, i.e. 72 chars */
	    for (j = 0; j < 72; j++, len++) {
		if(getoctal(&o, u->h.fd) != 1) {
		    sprintf(tmp1, _("getlab: comment = %c"), o);
		    error(tmp1); }
		if (j == 0) isDollar = (o == '$');
	    }
	    if(getoctal(&o, u->h.fd) != 1 || o != 0110) {
		sprintf(tmp1, _("getlab: comment end byte = %o"), o);
		error(tmp1); }
	    /* read and throw away
	       end of package byte=0110, i.e. 72 chars */
	} while (len >= 72 && !isDollar);
	/* until start of comment line is '$' */
	/* removed Mar 2006 to avoid a problen with over-long comments
	if (len > 72) {
	    combuf[len - 73] = '\0';
	    u->h.comment = (char *) R_alloc(len - 72, sizeof(char));
	    strncpy(u->h.comment, combuf, (len - 72));
	}
	else u->h.comment = NULL; */

	/* If comment on record(s) before the one beginning
	   with a $, allocate space and squirrel away */

	if(getoctal(&o, u->h.fd) != 1 || o != 006) {
	    sprintf(tmp1, _("getlab: byte nv0 = %o octal"), o);
	    error(tmp1); }
	/* read and throw away front of package
	   byte=006, i.e. 3 shorts */
	if(getshort(&u->h.nv, u->h.fd) != 1)
	    error(_("getlab: File access error"));
	if(getshort(&u->h.mtype, u->h.fd) != 1)
	    error(_("getlab: File access error"));
	if(getshort(&u->h.ntype, u->h.fd) != 1)
	    error(_("getlab: File access error"));
	if(getoctal(&o, u->h.fd) != 1 || o != 006) {
	    sprintf(tmp1, _("getlab: byte nv$ = %o octal"), o);
	    error(tmp1); }
	/* read and throw away end of package
	   byte=006, i.e. 3 shorts */

    }	/* i.e. version later than 2 */
/* RSB 2004-10-22 */
    if (u->h.nv > MAXVARS)
	error(_("file has more variables than this function can read"));

    for (j=0; j<u->h.nv; j++) {	/* since the number of variables is now
				   known, read in their labels, allocating
				   memory on the go */

	if(getoctal(&o, u->h.fd) != 1 || o != 014) {
	    sprintf(tmp1, _("getlab: byte lab[%d]0 = %o, nv=%d"),
		    j, o, u->h.nv);
	    error(tmp1); }
	/* read and throw away front of package
	   byte=014, i.e. LABELSIZ chars */
	if(fread(label, 1, LABELSIZ, u->h.fd) != LABELSIZ)
	    error(_("getlab: File access error"));
	/* read LABELSIZ chars into label */
	label[LABELSIZ] = '\0';	/* terminate the string */

	if(label[8] == '$') u->h.nk++;
	else if (strrchr(label, '$') != NULL) {
	    u->h.nk++;
	    sprintf(mes, _("$ not in variable label column 9: %s"), label);
	    warning(mes);
	} else u->h.nd++;	/* if the ninth char in label is '$',
				   it is a string variable, else a real
				   variable */

	for(i=0, o=0; label[i] != '\0'; i++)
	    if(label[i] != ' ') tmp[o++] = label[i];
	/* from left to right copy label into tmp
	   until a blank is encountered */
	tmp[o] = '\0';	/* terminate the string */

	len=strlen(tmp);
	sprintf(var, "u->h.lab[%d]", j);
	u->h.lab[j] = (char *) R_alloc(len+1, sizeof(char));
	strcpy(u->h.lab[j], tmp);

	/* allocate memory for the label,
	   move it and point lab[j] at it */
	if(getoctal(&o, u->h.fd) != 1 || o != 014) {
	    sprintf(tmp1, _("getlab: byte lab[%d]$ = %o octal"), j, o);
	    error(tmp1); }
	/* read and throw away end of package
	   byte=014, i.e. LABELSIZ chars */
    }	/* j */

    u->pos = ftell(u->h.fd);	/* find current position, should
				   be at packet boundary of first
				   data record */
}	/* getlab */


/* Function that closes the systat file in use */

static void closeuse(struct SysFilev3 *use)
{
    if(use->h.flag != 0) fclose(use->h.fd);
    use->h.flag = 0;
} /* closeuse */


#if 0
/*
Function to return the number of a found variable name, or
-1 if not found
*/
static int getvarno(char *name, struct SysFilev3 *use)
{
    int i;
    if (use->h.flag != 1) return(-1);
    for (i=0; i < use->h.nv; i++)
	if(strcmp(name, use->h.lab[i]) == 0) return(i);
    return(-1);
}
#endif

/*
Function to return the number of variables, or
-1 if not found
*/
static int getnv(struct SysFilev3 *use)
{
    if (isuse(use) == 0) return(-1);
    return((int) use->h.nv);
}

/*
Function to return the number of numerical variables, or
-1 if not found
*/
static int getnd(struct SysFilev3 *use)
{
    if (isuse(use) == 0) return(-1);
    return((int) use->h.nd);
}

/*
Function to return the number of string variables, or
-1 if not found
*/
static int getnk(struct SysFilev3 *use)
{
    if (isuse(use) == 0) return(-1);
    return((int) use->h.nk);
}

/* returns the variable name for a given number, or NULL on error */
static char *getvarnam(int i, struct SysFilev3 *use)
{
    if (isuse(use) == 0 || i >= use->h.nv) return(NULL);
    return(use->h.lab[i]);
}


/* says whether a given variable is a double (>=0) or a string (-1) */
static int isdb(int i, struct SysFilev3 *use)
{
    if (use->ithdb[i] >= 0) return(0);
    else return(-1);
}


/* returns the current mtype - type of data file */
static int getmtype(struct SysFilev3 *use)
{
    return((int) use->h.mtype);
}

/* says whether a systat file is in use */
static int isuse(struct SysFilev3 *use)
{
    return((int) use->h.flag);
}

/*
returns the number of observations in the currently open
systat file
*/
static int getnobs(struct SysFilev3 *use)
{
    if(isuse(use) == 0) return(-1);
    return((int) use->nobs);
}


/*
gets a single observation on a real valued variable,
from the file pointed at by fd, of type 1=float, 2=double,
and puts it in the double pointed at by x. The values are
assumed to be little-endian.
*/
static int getdb(FILE *fd, short type, double *x)
{
    float fx;
    double dx;

    if (type == 1) {
	if(fread(&fx, sizeof(float), 1, fd) != 1) return(1);
	swapb(&fx, sizeof(float));
	*x = (double) fx;	/* and cast it */
    } else {
	if(fread(&dx, sizeof(double), 1, fd) != 1) return(1);
	swapb(&dx, sizeof(double));
	*x = dx;
    }
    return(0);
}	/* getdb */

/*
gets a single observation on a string valued variable, from the file
pointed at by fd, puts it in the string pointed at by svalue, in accord
with packet_bound - if the desired variable is undivided and
does not terminate in octal 201:201 (the packet bound for observations
exceeding 128 bytes) 0, if terminates in 201:201 -1, else is
equal to the number of bytes beyond 201:201.
*/
static void getsvar(FILE *fd, char *svalue, short packet_bound)

    /* if the desired variable is undivided and
       does not terminate in octal 201:201 (the
       packet bound for observations exceeding 128
       bytes) 0, if terminates in 201:201 -1, else is
       equal to the number of bytes beyond 201:201 */
{
    char tmp_str[9];
    if (packet_bound <= 0) {	/* string value not split */

	if((fread(svalue, 1, LABELSIZ, fd)) != LABELSIZ)
	    error(_("file access error"));/* read LABELSIZ chars */
	svalue[LABELSIZ] = '\0';
    }
    else {
	if((fread(tmp_str, 1, (LABELSIZ - packet_bound), fd)) !=
	   (LABELSIZ - packet_bound)) error(_("file access error"));
	/* read the LABELSIZ - packet_bound chars in this record */

	tmp_str[LABELSIZ - packet_bound] = '\0';
	strcpy(svalue, tmp_str);	/* store in svalue */

	if((fseek(fd, 2L, SEEK_CUR)) != 0) error(_("file access error"));			/* hop over the packet boundary */

	if((fread(tmp_str, 1, packet_bound, fd)) !=
	   packet_bound) error(_("file access error"));
	/* read the remaining packet_bound chars */

	tmp_str[packet_bound] = '\0';
	strcat(svalue, tmp_str);	/* concatenate strings */
    }
}	/* getsvar */



/* Get a whole double variable and put it in the array pointed at by db */
static void getdbvar(int varno, double *db, struct SysFilev3 *use)
{
    int j, k;
    double x;

    if (use->ithdb[varno] < 0) error(_("string variable"));

    if((j = fseek(use->h.fd, use->pos+use->local_offset[varno]+1L, SEEK_SET))
       != 0) error(_("file access error"));

    /* seek to first byte of this variable in first observation,
       pos is at beginning of record, thus we need pos + local
       offset for this variable + 1 to hop over front of packet byte */

    k = 0;
    do {
	if((j = getdb(use->h.fd, use->h.ntype, &x)) != 0) break;
	*(db+k) = x;
	k++;	/* get value */

    } while ((j = fseek(use->h.fd,
			(use->h.ntype == 1 ?
			 use->offset - (int) sizeof(float) :
			 use->offset - (int) sizeof(double)),
			SEEK_CUR)) == 0 && k < use->nobs);
    /* seek forward offset to next observation minus
       length of real just read until all observations read */

    if (j != 0) error(_("file access error"));
}	/* getdbvar */

/*
gets a byte from fp, puts it in the int pointed
at by o, returns 1 on success, otherwise != 1
*/
static int getoctal(int *o, FILE *fp)
{
    char c;
    int n;
    *o = 000;
    if ((n = fread((char *)&c, sizeof(char), 1, fp)) != 1)
	return(n);
    else {
	*o = c & 0377;
	return(n);
    }
}	/* getoctal */

/*
gets a short and points sh at it
*/
static size_t getshort(short *sh, FILE *fp)
{
    size_t res;

    res = fread((char *)sh, sizeof(short), 1, fp);
    swapb(sh, sizeof(short));
    return res;
}
