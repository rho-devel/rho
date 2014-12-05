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

/** @file ByteCode.cpp
 *
 * @brief Class CXXR::ByteCode.
 */

#include "CXXR/ByteCode.hpp"

#include "CXXR/IntVector.h"
#include "CXXR/errors.h"
#include "CXXR/StackChecker.hpp"

using namespace CXXR;

NodeStack* ByteCode::s_nodestack = nullptr;
std::vector<Frame::Binding*>* ByteCode::s_loopvar_stack = nullptr;

void ByteCode::detachReferents()
{
    m_code.detach();
    m_constants.detach();
    RObject::detachReferents();
}

RObject* ByteCode::evaluate(Environment* env)
{
#ifdef BYTECODE
    IncrementStackDepthScope scope;
    return interpret(this, env);
#else
    Rf_error(_("bytecode evaluation not enabled"));
    return 0;
#endif
}

void ByteCode::initialize()
{
    if (!s_nodestack) {
	s_nodestack = new NodeStack(512);
	s_loopvar_stack = new std::vector<Frame::Binding*>;
#ifdef THREADED_CODE
	interpret(0, 0);
#endif
    }
}

// ByteCode::interpret() is in eval.cpp

void ByteCode::protectAll()
{
    if (s_nodestack)
	s_nodestack->protectAll();
}

// ByteCode::thread() is in eval.cpp

const char* ByteCode::typeName() const
{
    return staticTypeName();
}

void ByteCode::visitReferents(const_visitor* v) const
{
    const GCNode* code = m_code;
    const GCNode* constants = m_constants;
    RObject::visitReferents(v);
    if (code)
	(*v)(code);
    if (constants)
	(*v)(constants);
}

void ByteCode::visitRoots(GCNode::const_visitor* v)
{
    if (s_nodestack)
	s_nodestack->visitRoots(v);
}
