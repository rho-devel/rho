/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-10 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/** @file S3Launcher.cpp
 *
 * Implementation of class S3Launcher.
 */

#include "CXXR/S3Launcher.hpp"

#include "CXXR/Environment.h"
#include "CXXR/FunctionBase.h"

using namespace std;
using namespace CXXR;

// Implementation of S3Launcher::create() is in objects.cpp

void S3Launcher::detachReferents()
{
    m_classes.detach();
    m_function.detach();
    m_symbol.detach();
}

std::pair<FunctionBase*, bool>
S3Launcher::findMethod(const Symbol* symbol, Environment* call_env,
		       Environment* table_env)
{
    pair<Environment*, FunctionBase*> pr = findFunction(symbol, call_env);
    if (pr.first)
	return make_pair(pr.second, true);
    Environment* table = 0;
    // Look for S3 methods table:
    {
	Frame::Binding* tblbdg
	    = table_env->frame()->binding(S3MethodsTableSymbol);
	if (tblbdg) {
	    RObject* tblbdgval = tblbdg->forcedValue().first;
	    if (tblbdgval && tblbdgval->sexptype() == ENVSXP)
		table = static_cast<Environment*>(tblbdgval);
	}
    }
    // Look up method in table:
    if (table) {
	Frame::Binding* symbdg = table->frame()->binding(symbol);
	if (symbdg) {
	    RObject* symbdgval = symbdg->forcedValue().first;
	    // Assume that the result is a FunctionBase:
	    return make_pair(static_cast<FunctionBase*>(symbdgval), false);
	}
    }
    return pair<FunctionBase*, bool>(0, false);
}

void S3Launcher::visitReferents(const_visitor* v) const
{
    if (m_classes)
	(*v)(m_classes);
    if (m_function)
	(*v)(m_function);
    if (m_symbol)
	(*v)(m_symbol);
}
