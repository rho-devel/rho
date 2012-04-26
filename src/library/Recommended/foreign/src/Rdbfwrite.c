/* Based on code in the shapelib.maptools.org library.
 *
 * First version for R's maptools package appears to be
 * Copyright 2000-2001 (c) Nicholas Lewin-Koh
 *
 * Changes for the foreign package
 * Copyright (C) 2004 the R Code Development Team
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

#include <stdlib.h>
#include <string.h>
#include "shapefil.h"
#include <R.h>
#include <Rinternals.h>
#include "foreign.h"


static DBFHandle Rdbfwrite(DBFHandle, SEXP, SEXP, SEXP, SEXP);

static char* nameMangleOut(char *dbfFldname, int len)
{
    int i;
    for(i = 0; i < len; i++)
      if (dbfFldname[i] == '.') dbfFldname[i] = '_';
    return dbfFldname;
}


SEXP DoWritedbf(SEXP file, SEXP df, SEXP pr, SEXP sc, SEXP DataTypes)
{
    DBFHandle hDBF;

    if (!isValidString(file))
	error (_("first argument must be a file name"));

    hDBF = DBFCreate(R_ExpandFileName(CHAR(STRING_ELT(file, 0))));
    if (hDBF == NULL) error(_("unable to open file"));

    Rdbfwrite(hDBF, df, pr, sc, DataTypes);
    DBFClose(hDBF);
    return R_NilValue;
}


static DBFHandle
Rdbfwrite(DBFHandle hDBF, SEXP df, SEXP pr, SEXP sc, SEXP DataTypes)
{

    int	i, iRecord, nflds, nrecs, itmp;
    int	nWidth;
    char szTitle[12];
    double rtmp;
    SEXP names = getAttrib(df, R_NamesSymbol), this;

    nflds = length(df);
    nrecs = length(VECTOR_ELT(df, 0));
    for(i = 0; i < nflds; i++) {
	strncpy(szTitle, CHAR(STRING_ELT(names,i)), 11);
	szTitle[11] = '\0';
	nWidth = INTEGER(pr)[i];
	switch(CHAR(STRING_ELT(DataTypes, i))[0]) {
	case 'L':
	    DBFAddField(hDBF, nameMangleOut(szTitle,11), FTLogical, nWidth, 0);
	    break;
	case 'N':
	case 'F':
	    if(TYPEOF(VECTOR_ELT(df, i)) == INTSXP)
		DBFAddField(hDBF, nameMangleOut(szTitle,11), FTInteger,
			    nWidth, 0);
	    else
		DBFAddField(hDBF, nameMangleOut(szTitle,11), FTDouble, nWidth,
			    INTEGER(sc)[i]);
	    break;
	case 'C':
	    DBFAddField(hDBF, nameMangleOut(szTitle,11), FTString, nWidth, 0);
	    break;
	case 'D':
	    DBFAddField(hDBF, nameMangleOut(szTitle,11), FTDate, 8, 0);
	    break;
	default:
	    error(_("unknown data type"));
	    break;
	}
    }

    for(iRecord = 0; iRecord < nrecs; iRecord++) {
	for(i = 0; i < nflds; i++) {
	    switch(TYPEOF(VECTOR_ELT(df, i))) {
	    case LGLSXP:
		itmp = LOGICAL(VECTOR_ELT(df, i))[iRecord];
		if(itmp == NA_INTEGER)
		    DBFWriteNULLAttribute(hDBF, iRecord, i);
		else
		    DBFWriteLogicalAttribute(hDBF, iRecord, i,
					     (itmp != 0) ? 'T' : 'F');
		break;
	    case INTSXP:
		itmp = INTEGER(VECTOR_ELT(df, i))[iRecord];
		if(itmp == NA_INTEGER)
		    DBFWriteNULLAttribute(hDBF, iRecord, i);
		else
		    DBFWriteIntegerAttribute(hDBF, iRecord, i, itmp);
		break;
	    case REALSXP:
		rtmp = REAL(VECTOR_ELT(df, i))[iRecord];
		if(ISNAN(rtmp))
		    DBFWriteNULLAttribute(hDBF, iRecord, i);
		else
		    DBFWriteDoubleAttribute(hDBF, iRecord, i, rtmp);
		break;
	    case STRSXP:
		this = STRING_ELT(VECTOR_ELT(df,i), iRecord);
		if(this == NA_STRING)
		    DBFWriteNULLAttribute(hDBF, iRecord, i);
		else
		    DBFWriteStringAttribute(hDBF, iRecord, i, CHAR(this));
		break;
	    default:
		error(_("unknown data type"));
		break;
	    }
	}
    }

    return(hDBF);
}
