/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-12 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2001   The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */

#ifndef R_COMPLEX_H
#define R_COMPLEX_H

#ifndef  __cplusplus

typedef struct {
    double r;
    double i;
} Rcomplex;

#else

struct Rcomplex {
    double r;
    double i;

    Rcomplex()
    {}

    Rcomplex(double rl, double im = 0.0)
	: r(rl), i(im)
    {}

    Rcomplex& operator=(double rhs)
    {
	r = rhs;
	i = 0;
	return *this;
    }
};

inline bool operator==(const Rcomplex& l, const Rcomplex& r)
{
    return (l.r == r.r) && (l.i == r.i);
}

inline bool operator!=(const Rcomplex& l, const Rcomplex& r)
{
    return !(l==r);
}

#endif // __cplusplus

#endif /* R_COMPLEX_H */
