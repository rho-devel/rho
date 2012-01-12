/*
   header file for the nlme package

   Copyright 1999 Saikat DebRoy

   This file is part of the nlme package for R and related languages
   and is made available under the terms of the GNU General Public
   License, version 2, or at your option, any later version,
   incorporated herein by reference.

   This program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, a copy is available at
   http://www.r-project.org/Licenses/

*/

#ifndef NLME_PDMAT_H
#define NLME_PDMAT_H
#include "base.h"

extern void compSymm_pd(double *, longint *, double *);
extern void matrixLog_pd(double *, longint *, double *);
extern void logChol_pd(double *, longint *, double *);

#endif /* NLME_BASE_H */
