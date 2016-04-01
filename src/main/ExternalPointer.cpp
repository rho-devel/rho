/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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
 *  https://www.R-project.org/Licenses/
 */

/** @file ExternalPointer.cpp
 *
 * @brief Class rho::ExternalPointer and associated C interface.
 */

#include "rho/ExternalPointer.hpp"

#include "localization.h"
#include "rho/GCStackRoot.hpp"

using namespace std;
using namespace rho;

// Force the creation of non-inline embodiments of functions in the C
// interface:
namespace rho {
    namespace ForceNonInline {
	void* (*R_ExternalPtrAddrp)(SEXP) = R_ExternalPtrAddr;
	RObject* (*R_ExternalPtrTagp)(SEXP) = R_ExternalPtrTag;
	RObject* (*R_ExternalPtrProtectedp)(SEXP) = R_ExternalPtrProtected;
	void (*R_ClearExternalPtrp)(SEXP) = R_ClearExternalPtr;
    }
}

void ExternalPointer::detachReferents()
{
    m_protege.detach();
    m_tag.detach();
    RObject::detachReferents();
}

const char* ExternalPointer::typeName() const
{
    return ExternalPointer::staticTypeName();
}

void ExternalPointer::visitReferents(const_visitor* v) const
{
    const GCNode* protege = m_protege;
    const GCNode* tag = m_tag;
    RObject::visitReferents(v);
    if (protege)
	(*v)(protege);
    if (tag)
	(*v)(tag);
}

// ***** C interface *****

SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot)
{
    return new ExternalPointer(p, tag, prot);
}

void R_SetExternalPtrAddr(SEXP s, void *p)
{
    rho::ExternalPointer& ep
	= *rho::SEXP_downcast<rho::ExternalPointer*>(s);
    ep.setPtr(p);
}

void R_SetExternalPtrTag(SEXP s, SEXP tag)
{
    rho::ExternalPointer& ep
	= *rho::SEXP_downcast<rho::ExternalPointer*>(s);
    ep.setTag(tag);
}

void R_SetExternalPtrProtected(SEXP s, SEXP p)
{
    rho::ExternalPointer& ep
	= *rho::SEXP_downcast<rho::ExternalPointer*>(s);
    ep.setProtege(p);
}
