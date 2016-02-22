/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007  The R Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifndef R_STATS_PACKAGE_H
#define R_STATS_PACKAGE_H
#include <Rconfig.h>

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif

#ifdef __cplusplus
extern "C" {
#endif

enum AlgType {NREG = 1, OPT = 2};
				/* 0-based indices into v */
enum  VPos {F = 9, F0 = 12, FDIF = 10, G = 27, HC = 70};
				/* 0-based indices into iv */
enum IVPos {AI = 90, AM = 94, ALGSAV = 50, COVMAT = 25,
	    COVPRT = 13, COVREQ = 14, DRADPR = 100,
	    DTYPE = 15, IERR = 74, INITH = 24, INITS = 24,
	    IPIVOT = 75, IVNEED =  2, LASTIV = 42, LASTV = 44,
	    LMAT =  41, MXFCAL = 16, MXITER = 17, NEXTV  = 46,
	    NFCALL =  5, NFCOV = 51, NFGCAL = 6, NGCOV = 52,
	    NITER = 30, NVDFLT = 49, NVSAVE = 8, OUTLEV = 18,
	    PARPRT = 19, PARSAV = 48, PERM = 57, PRUNIT = 20,
	    QRTYP = 79, RDREQ = 56, RMAT = 77, SOLPRT = 21,
	    STATPR = 22, TOOBIG = 1, VNEED = 3, VSAVE = 59,
	    X0PRT = 23};

void attribute_hidden
S_Rf_divset(int alg, int iv[], int liv, int lv, double v[]);

void attribute_hidden
S_nlsb_iterate(double b[], double d[], double dr[], int iv[],
	       int liv, int lv, int n, int nd, int p,
	       double r[], double rd[], double v[], double x[]);

void attribute_hidden
S_nlminb_iterate(double b[], double d[], double fx, double g[],
		 double h[], int iv[], int liv, int lv, int n,
		 double v[], double x[]);

static R_INLINE int S_v_length(int alg, int n)
{
    return (alg - 1) ? (105 + (n * (2 * n + 20))) :
	(130 + (n * (n + 27))/2);
}

static R_INLINE int S_iv_length(int alg, int n)
{
    return (alg - 1) ? (82 + 4 * n) : (78 + 3 * n);
}

#ifdef __cplusplus
}
#endif

#endif /* R_STATS_PACKAGE_H */


