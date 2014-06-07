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

/** @file S3Launcher.cpp
 *
 * Implementation of class S3Launcher.
 */

#include "CXXR/S3Launcher.hpp"

#include "CXXR/Environment.h"
#include "CXXR/FunctionBase.h"

using namespace std;
using namespace CXXR;

void S3Launcher::addMethodBindings(Frame* frame) const
{
    // .Class:
    if (m_index == 0)
	frame->bind(DotClassSymbol, m_classes);
    else {
	GCStackRoot<StringVector>
	    dotclass(CXXR_NEW(StringVector(m_classes->size() - m_index)));
	for (unsigned int j = 0; j < dotclass->size(); ++j)
	    (*dotclass)[j] = (*m_classes)[j + m_index];
	dotclass->setAttribute(PreviousSymbol, m_classes);
	frame->bind(DotClassSymbol, dotclass);
    }

    // .Method:
    {
	String* method_name
	    = const_cast<String*>(m_symbol->name());
	frame->bind(DotMethodSymbol, CXXR_NEW(StringVector(1, method_name)));
    }

    // .Group:
    if (m_using_group)
	frame->bind(DotGroupSymbol, asStringVector(m_group));

    // Others:
    frame->bind(DotGenericSymbol, asStringVector(m_generic));
    frame->bind(DotGenericCallEnvSymbol, m_call_env);
    frame->bind(DotGenericDefEnvSymbol, m_table_env);
}	

// Implementation of S3Launcher::create() is in objects.cpp

void S3Launcher::detachReferents()
{
    m_call_env.detach();
    m_table_env.detach();
    m_classes.detach();
    m_function.detach();
}

std::pair<FunctionBase*, bool>
S3Launcher::findMethod(const Symbol* symbol, Environment* call_env,
		       Environment* table_env)
{
    pair<Environment*, FunctionBase*> pr = findFunction(symbol, call_env);
    if (pr.first)
	return make_pair(pr.second, true);
    if (!table_env)
	return pair<FunctionBase*, bool>(0, false);
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
    if (m_call_env)
	(*v)(m_call_env);
    if (m_table_env)
	(*v)(m_table_env);
    if (m_classes)
	(*v)(m_classes);
    if (m_function)
	(*v)(m_function);
}
