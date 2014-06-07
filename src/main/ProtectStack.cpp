/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file ProtectStack.cpp
 *
 * Implementation of class CXXR::ProtectStack and associated C
 * interface.
 */

#include "CXXR/ProtectStack.h"

#include <stdexcept>

using namespace std;
using namespace CXXR;

// Force generation of non-inline embodiments of functions in the C
// interface:
namespace CXXR {
    namespace ForceNonInline {
	SEXP (*protectp)(SEXP) = Rf_protect;
        void (*unprotectp)(int) = Rf_unprotect;
	void (*unprotect_ptrp)(RObject*) = Rf_unprotect_ptr;
	void (*ProtectWithIndexp)(SEXP, PROTECT_INDEX *) = R_ProtectWithIndex;
	void (*Reprotectp)(SEXP, PROTECT_INDEX) = R_Reprotect;
    }
}

NodeStack* ProtectStack::s_stack = 0;

void ProtectStack::cleanup()
{
    delete s_stack;
}

void ProtectStack::initialize()
{
    s_stack = new NodeStack(64);
}

void ProtectStack::restoreSize(size_t new_size)
{
    if (new_size > s_stack->size())
	throw out_of_range("ProtectStack::ppsRestoreSize: requested size"
			   " greater than current size.");
    s_stack->resize(new_size);
}

// ***** C interface *****

void Rf_ppsRestoreSize(size_t new_size)
{
    ProtectStack::restoreSize(new_size);
}

size_t Rf_ppsSize()
{
    return ProtectStack::size();
}
