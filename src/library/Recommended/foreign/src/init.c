/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005   The R Development Core Team.
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

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void spss_init(void);

SEXP Rdbfread(SEXP dbfnm);
SEXP DoWritedbf(SEXP file, SEXP df, SEXP pr, SEXP sc, SEXP DataTypes);
SEXP read_mtp(SEXP fname);
SEXP readSystat(SEXP file);
SEXP do_read_SPSS(SEXP file);
SEXP xport_info(SEXP xportFile);
SEXP xport_read(SEXP xportFile, SEXP xportInfo);

SEXP do_readStata(SEXP call);
SEXP do_writeStata(SEXP call);

#define CDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CMethodDef CEntries[]  = {
    CDEF(spss_init, 0),
    {NULL, NULL, 0}
};

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef CallEntries[] = {
    CALLDEF(Rdbfread, 1),
    CALLDEF(DoWritedbf, 5),
    CALLDEF(read_mtp, 1),
    CALLDEF(readSystat, 1),
    CALLDEF(do_read_SPSS, 1),
    CALLDEF(xport_info, 1),
    CALLDEF(xport_read, 2),
    {NULL, NULL, 0}
};

#define EXTDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_ExternalMethodDef ExtEntries[] = {
    EXTDEF(do_readStata, 1),
    EXTDEF(do_writeStata, 4),
    {NULL, NULL, 0}
};


#include <Rversion.h>
void R_init_foreign(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, ExtEntries);
    R_useDynamicSymbols(dll, FALSE);
#if defined(R_VERSION) && R_VERSION >= R_Version(3, 0, 0)
    R_forceSymbols(dll, TRUE);
#endif
}
