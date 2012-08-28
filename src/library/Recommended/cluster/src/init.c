#include <R.h>
#include <Rinternals.h>

#include "cluster.h"

#include <R_ext/Rdynload.h>

#define CDEF(name)  {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}


static R_NativePrimitiveArgType R_bncoef_t[3] = {
    INTSXP, REALSXP, REALSXP
};

static R_NativePrimitiveArgType cl_clara_t[33] = {
    /*n:*/ INTSXP, INTSXP, INTSXP, REALSXP, INTSXP, INTSXP, REALSXP, INTSXP,
    /*valmd:*/ REALSXP, INTSXP, INTSXP, /* rng_R: */ LGLSXP, /* pam_like:*/ LGLSXP,
    /*nrepr: */ INTSXP, INTSXP, INTSXP, INTSXP, INTSXP,
    /*radus:*/ REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP,
    /*obj: */ REALSXP, REALSXP, REALSXP, REALSXP,  INTSXP, INTSXP,
    /*tmp: */ REALSXP,INTSXP
};

static R_NativePrimitiveArgType cl_fanny_t[27] = {
    INTSXP, INTSXP, INTSXP, REALSXP, REALSXP,
    /*jdyss: */ INTSXP, REALSXP, INTSXP,  INTSXP, INTSXP, INTSXP,
    /*negbr: */ INTSXP, /*syl: */ REALSXP, REALSXP, REALSXP, REALSXP,
    /*nfuzz: */ INTSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    /*obj: */ REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, INTSXP
};

static R_NativePrimitiveArgType cl_pam_t[24] = {
    INTSXP, INTSXP, INTSXP, REALSXP, REALSXP,
    /*jdyss: */ INTSXP, REALSXP, INTSXP,  INTSXP, INTSXP,
    /*nrepr: */ LGLSXP, INTSXP, /*radus: */ REALSXP, REALSXP, REALSXP, REALSXP,
    /*ttsyl: */ REALSXP, REALSXP, INTSXP, INTSXP,  REALSXP, REALSXP, INTSXP,
    /*optim: */ INTSXP
};

static R_NativePrimitiveArgType spannel_t[12] = { // ./spannel.c :
    INTSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP,
    /*varss: */ REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType sildist_t[] = {
    REALSXP, INTSXP, INTSXP, INTSXP, REALSXP, INTSXP,
    /* si: */ REALSXP, INTSXP, LGLSXP
};

static R_NativePrimitiveArgType twins_t[17] = {
    INTSXP, INTSXP, REALSXP, REALSXP, REALSXP,
    /* jdiss: */ INTSXP, REALSXP,
    INTSXP, INTSXP, INTSXP, INTSXP,
    /* kwan: */ INTSXP, INTSXP, REALSXP, REALSXP,
    REALSXP, INTSXP
};

/* is only .C()-called from ../tests/sweep-ex.R : */
static R_NativePrimitiveArgType cl_sweep_t[5] = {
    REALSXP, INTSXP, INTSXP, INTSXP, REALSXP
};

static const R_CMethodDef CEntries[]  = {
    CDEF(R_bncoef),
    CDEF(cl_clara),
    {"dysta3", (DL_FUNC) &dysta3, 8},/* ./fanny.c */
    CDEF(cl_fanny),
    CDEF(cl_pam),
    CDEF(spannel),
    CDEF(cl_sweep),
    CDEF(sildist),
    CDEF(twins),
    {NULL, NULL, 0}
};

/* static R_CallMethodDef CallEntries[] = {
 *     {NULL, NULL, 0}
 * };
 */

static R_FortranMethodDef FortEntries[] = {
    {"cl_daisy", (DL_FUNC) &F77_SUB(cldaisy), 11},
    {"cl_mona", (DL_FUNC) &F77_SUB(clmona), 9},
    {"dysta", (DL_FUNC) &F77_SUB(dysta), 8},
    {NULL, NULL, 0}
};

void R_init_cluster(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL/*CallEntries*/, FortEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
