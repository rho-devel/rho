/*
 *  spatial/src/init.c by W. N. Venables and B. D. Ripley.  Copyright (C) 2002
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 or 3 of the License
 *  (at your option).
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 *
 */
#include <R.h>
#include "spatial.h"
#include "R_ext/Rdynload.h"

static const R_CMethodDef CEntries[] = {
    {"VR_alset", (DL_FUNC) &VR_alset, 2},
    {"VR_correlogram", (DL_FUNC) &VR_correlogram, 8},
    {"VR_fmat", (DL_FUNC) &VR_fmat, 5},
    {"VR_frset", (DL_FUNC) &VR_frset, 4},
    {"VR_gls", (DL_FUNC) &VR_gls, 15},
    {"VR_krpred", (DL_FUNC) &VR_krpred, 8},
    {"VR_ls", (DL_FUNC) &VR_ls, 11},
    {"VR_prvar", (DL_FUNC) &VR_prvar, 12},
    {"VR_valn", (DL_FUNC) &VR_valn, 6},
    {"VR_variogram", (DL_FUNC) &VR_variogram, 8},
    {"VR_pdata", (DL_FUNC) &VR_pdata, 3},
    {"VR_plike", (DL_FUNC) &VR_plike, 8},
    {"VR_ppget", (DL_FUNC) &VR_ppget, 1},
    {"VR_ppset", (DL_FUNC) &VR_ppset, 1},
    {"VR_simmat", (DL_FUNC) &VR_simmat, 4},
    {"VR_simpat", (DL_FUNC) &VR_simpat, 6},
    {"VR_sp_pp2", (DL_FUNC) &VR_sp_pp2, 8},
    {NULL, NULL, 0}
};

void R_init_spatial(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
}
