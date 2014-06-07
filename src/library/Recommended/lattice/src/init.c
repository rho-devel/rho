
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include "threeDplot.h"

static R_CallMethodDef CallEntries[] = {
    {"wireframePanelCalculations", (DL_FUNC) &wireframePanelCalculations, 12},
    {NULL, NULL, 0}
};

void R_init_lattice(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

