#include <R.h>
#include <Rinternals.h>

#include "cluster.h"

#include <R_ext/Rdynload.h>

#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}


static R_NativePrimitiveArgType clara_t[33] = {
    /*n:*/ INTSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP, INTSXP,
    /*valmd:*/ REALSXP, INTSXP, INTSXP, /* rng_R: */ LGLSXP, /* pam_like:*/ LGLSXP,
    /*nrepr: */ INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
    /*radus:*/ REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP,
    /*obj: */ REALSXP, REALSXP, REALSXP, REALSXP,  INTSXP, INTSXP,
    /*tmp: */ REALSXP,INTSXP
};

static R_NativePrimitiveArgType fanny_t[29] = {
    INTSXP, INTSXP, INTSXP, REALSXP, REALSXP,
    /*jdyss: */ INTSXP, REALSXP, INTSXP,  INTSXP, INTSXP, INTSXP,
    /*negbr: */ INTSXP, /*syl: */ REALSXP, REALSXP, REALSXP, REALSXP,
    /*nfuzz: */ INTSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    /*obj: */ REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, INTSXP
};

static R_NativePrimitiveArgType pam_t[23] = {
    INTSXP, INTSXP, INTSXP, REALSXP, REALSXP,
    /*jdyss: */ INTSXP, REALSXP, INTSXP,  INTSXP, INTSXP,
    /*nrepr: */ LGLSXP, INTSXP, /*radus: */ REALSXP, REALSXP, REALSXP, REALSXP,
    /*ttsyl: */ REALSXP, REALSXP, INTSXP, INTSXP,  REALSXP, REALSXP, INTSXP
};

static R_NativePrimitiveArgType spannel_t[12] = {
    INTSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    /*varss: */ REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType sildist_t[] = {
    REALSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP,
    /* si: */ REALSXP, INTSXP, LGLSXP
};

/* is only .C()-called from ../tests/sweep-ex.R : */
static R_NativePrimitiveArgType sweep_t[5] = {
    REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static const R_CMethodDef CEntries[]  = {
    {"cl_clara", (DL_FUNC) &cl_clara, 33, clara_t},
    {"dysta3", (DL_FUNC) &dysta3, 8},/* ./fanny.c */
    {"cl_fanny", (DL_FUNC) &cl_fanny, 27, fanny_t},
    {"cl_pam", (DL_FUNC) &cl_pam, 23, pam_t},
    // ./spannel.c :
    {"spannel", (DL_FUNC) &spannel, 12, spannel_t},
    {"cl_sweep", (DL_FUNC) &cl_sweep, 5, sweep_t},
    CDEF(sildist),
    {NULL, NULL, 0}
};

/* static R_CallMethodDef CallEntries[] = {
 *     {NULL, NULL, 0}
 * };
 */

static R_FortranMethodDef FortEntries[] = {
    {"bncoef", (DL_FUNC) &F77_SUB(bncoef), 3},/* ./twins.f */
    {"cl_daisy", (DL_FUNC) &F77_SUB(cldaisy), 11},
    {"cl_mona", (DL_FUNC) &F77_SUB(clmona), 9},
    {"twins", (DL_FUNC) &F77_SUB(twins), 17},
    {"dysta", (DL_FUNC) &F77_SUB(dysta), 8},
    {NULL, NULL, 0}
};

void R_init_cluster(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL/*CallEntries*/, FortEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
