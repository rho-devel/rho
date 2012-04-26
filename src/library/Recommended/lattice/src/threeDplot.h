#ifndef LATTICE_THREED_H
#define LATTICE_THREED_H

#include <R.h>
#include <Rdefines.h>



SEXP wireframePanelCalculations(SEXP xArg, SEXP yArg, SEXP zArg, SEXP rotArg, 
				SEXP distanceArg,
				SEXP nxArg, SEXP nyArg, SEXP ngArg,
				SEXP lsArg,
				SEXP env, 
				SEXP shadeArg,
				SEXP isParSurfArg);



#endif


