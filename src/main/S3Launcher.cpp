/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

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
	    dotclass(StringVector::create(m_classes->size() - m_index));
	for (unsigned int j = 0; j < dotclass->size(); ++j)
	    (*dotclass)[j] = (*m_classes)[j + m_index];
	dotclass->setAttribute(PreviousSymbol, m_classes);
	frame->bind(DotClassSymbol, dotclass);
    }

    // .Method:
    {
	String* method_name
	    = const_cast<String*>(m_symbol->name());
	frame->bind(DotMethodSymbol, StringVector::createScalar(method_name));
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
    FunctionBase* fun = findFunction(symbol, call_env);
    if (fun)
	return make_pair(fun, true);
    if (!table_env)
	return pair<FunctionBase*, bool>(nullptr, false);
    Environment* table = nullptr;
    // Look for S3 methods table:
    {
	Frame::Binding* tblbdg
	    = table_env->frame()->binding(S3MethodsTableSymbol);
	if (tblbdg) {
	    RObject* tblbdgval = tblbdg->forcedValue();
	    if (tblbdgval && tblbdgval->sexptype() == ENVSXP)
		table = static_cast<Environment*>(tblbdgval);
	}
    }
    // Look up method in table:
    if (table) {
	Frame::Binding* symbdg = table->frame()->binding(symbol);
	if (symbdg) {
	    RObject* symbdgval = symbdg->forcedValue();
	    // Assume that the result is a FunctionBase:
	    return make_pair(static_cast<FunctionBase*>(symbdgval), false);
	}
    }
    return pair<FunctionBase*, bool>(nullptr, false);
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
