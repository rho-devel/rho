/* Symbol registration initialization: original provided by Brian Ripley.
   Anything called from R should be registered here (and declared in mgcv.h).
   (See also NAMESPACE:1)
 */ 
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "mgcv.h"


R_CMethodDef CEntries[] = {
    {"RMonoCon", (DL_FUNC) &RMonoCon, 7},
    {"RuniqueCombs", (DL_FUNC) &RuniqueCombs, 4},
    {"RPCLS", (DL_FUNC) &RPCLS, 14},
    {"construct_tprs", (DL_FUNC) &construct_tprs, 13},
    {"crspl", (DL_FUNC) &crspl,8},
    {"predict_tprs", (DL_FUNC) &predict_tprs, 12},
    {"MinimumSeparation", (DL_FUNC) &MinimumSeparation, 7},
    {"magic", (DL_FUNC) &magic, 19},
    {"mgcv_mmult", (DL_FUNC) &mgcv_mmult,8},
    {"mgcv_pmmult", (DL_FUNC) &mgcv_mmult,9},
    {"gdi1",(DL_FUNC) &gdi1,46},
    {"R_cond",(DL_FUNC) &R_cond,5} ,
    {"pls_fit",(DL_FUNC)&pls_fit,10},
    {"pls_fit1",(DL_FUNC)&pls_fit1,12},
    {"tweedious",(DL_FUNC)&tweedious,8},
    {"psum",(DL_FUNC)&psum,4},
    {"get_detS2",(DL_FUNC)&get_detS2,12},
    {"get_stableS",(DL_FUNC)&get_stableS,14},
    {"mgcv_tri_diag",(DL_FUNC)&mgcv_tri_diag,3},
    {"mgcv_td_qy",(DL_FUNC)&mgcv_td_qy,7},
    {"mgcv_symeig",(DL_FUNC)&mgcv_symeig,6},
    {"read_mat",(DL_FUNC)&read_mat,4},
    {"rwMatrix",(DL_FUNC)&rwMatrix,6},
    {"in_out",(DL_FUNC)&in_out,8},
    {"Rlanczos",(DL_FUNC)&Rlanczos,7},
    {"rksos",(DL_FUNC)&rksos,3},
    {"gen_tps_poly_powers",(DL_FUNC)&gen_tps_poly_powers,4},
    {"k_nn",(DL_FUNC)&k_nn,8},
    {"Rkdtree",(DL_FUNC)&Rkdtree,5},
    {"Rkdnearest",(DL_FUNC)&Rkdnearest,9},
    {"Rkradius",(DL_FUNC)&Rkradius,9},
    {"sspl_construct",(DL_FUNC)&sspl_construct,9},
    {"sspl_mapply",(DL_FUNC)&sspl_mapply,9},
    {"tri2nei",(DL_FUNC)&tri2nei,5},
    {"nei_penalty",(DL_FUNC)&nei_penalty, 10},
    {"boundary",(DL_FUNC)&boundary, 14},
    {"pde_coeffs",(DL_FUNC)&pde_coeffs, 9},
    {"gridder",(DL_FUNC)&gridder, 13},
    {"row_block_reorder",(DL_FUNC)&row_block_reorder,5},
    {"mgcv_pqr",(DL_FUNC)&mgcv_pqr,6},
    {"getRpqr",(DL_FUNC)&getRpqr,6},
    {"mgcv_pqrqy",(DL_FUNC)&mgcv_pqrqy,8},
    {NULL, NULL, 0}
};

void R_init_mgcv(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
