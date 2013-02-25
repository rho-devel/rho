/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/* ./line.c : */

void tukeyline(double *x, double *y, double *z, double *w, 
	       int *n, double *coef);

/* ./smooth.c : */

typedef enum { 
    sm_NO_ENDRULE, sm_COPY_ENDRULE, sm_TUKEY_ENDRULE 
} R_SM_ENDRULE;


/* Callable from R : */
void Rsm_3RSR (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3RSS (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3RS3R(double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3R   (double *x, double *y, int *n, int *end_rule, int *iter);
void Rsm_3    (double *x, double *y, int *n, int *end_rule, int *changed);

void Rsm_S    (double *x, double *y, int *n, int *do_ends, int *changed);
