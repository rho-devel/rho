/*CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
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

#define R_NO_REMAP

#include "CXXR/jit/FrameDescriptor.hpp"
#include "CXXR/Closure.h"
#include "CXXR/ConsCell.h"
#include "CXXR/Expression.h"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/PairList.h"
#include "CXXR/RObject.h"
#include "CXXR/Symbol.h"

namespace CXXR {
namespace JIT {

namespace {

/**
 * This class builds a usually-correct list of the set of local variable names
 * that are assigned to in an R expression.
 *
 * The logic loosely follows that of codeltools::findFuncLocals().
 */
struct LocalVariableVisitor : public GCNode::const_visitor {

    LocalVariableVisitor(std::vector<const Symbol*> *local_vars)
	: m_locals(local_vars)
    { }

    void operator()(const GCNode* node) override
    {
	visit(node);
    }

    // This loosely follows the logic of codetools::findFuncLocals().
    // It doesn't always get the set of local variables correct (it's
    // not always possible to do so in R), but it should be fairly close.
    // Any errors here may cause the generated code to be slower, but
    // should not effect the correctness of the code.
    void visit(const GCNode* node)
    {
	// We only really care about expressions here.
	const Expression* expression = dynamic_cast<const Expression*>(node);
	if (!expression) {
	    return;
	}

	// function is usually a symbol (it could also be a function or function
	// valued expression but those are relatively rare).
	const RObject* function = expression->car();
	if (shouldSkipFunction(function)) {
	    return;
	}

	// These functions all define a local variable with their first argument.
	if (function == Symbol::obtain("<-") || function == Symbol::obtain("=")
	    || function == Symbol::obtain("for")) {
	    if (!expression->tail()) {
		// This is something weird like `<-()` or `for`() etc.
		return;
	    }
	    const Symbol* local_var = dynamic_cast
		<const Symbol*>(expression->tail()->car());

	    // Add the symbol to the set of local variable names.
	    // TODO(kmillar): this is O(N^2) in the number of symbols in a
	    //   function.  Probably not an issue in practice since functions
	    //   generally don't have a lot of symbols.
	    if (local_var
		&& (std::find(m_locals->begin(), m_locals->end(), local_var)
		    == m_locals->end())) {
		m_locals->push_back(local_var);
	    }
	}

	// Visit the function and arguments.
	for (const ConsCell& item : *expression) {
	    visit(item.car());
	}
    }

    static bool shouldSkipFunction(const RObject* function)
    {
	static std::set<const RObject*> skippedFunctions = {
	    // These functions typically don't evaluate all of their
	    // arguments in the calling environment, so we don't include
	    // them.
	    Symbol::obtain("function"),
	    Symbol::obtain("~"),
	    Symbol::obtain("expression"),
	    Symbol::obtain("substitute"),
	    Symbol::obtain("quote"),
	    Symbol::obtain("bquote"),
	    Symbol::obtain("Quote"),
	    Symbol::obtain("with"),
	    Symbol::obtain("within"),
	    Symbol::obtain("eval.parent"),
	    Symbol::obtain("local"),
	    Symbol::obtain("delayedAssign")
	};
	return skippedFunctions.find(function) != skippedFunctions.end();
    }

    std::vector<const Symbol*>* m_locals;
};

}  // namespace

FrameDescriptor::FrameDescriptor(const Closure* closure)
{
    // Assign slots for the formal parameters.
    const PairList* formals = closure->matcher()->formalArgs();
    for (const ConsCell& formal : *formals) {
	const RObject* tag = formal.tag();
	m_local_vars.push_back(SEXP_downcast<const Symbol*>(tag));
    }
    m_num_formals = m_local_vars.size();

    // Add the expected local variables.
    LocalVariableVisitor(&m_local_vars).visit(closure->body());
}

FrameDescriptor::FrameDescriptor(std::initializer_list<const Symbol*> formals,
				 std::initializer_list<const Symbol*> locals)
{
    m_local_vars.insert(m_local_vars.end(),
			formals.begin(), formals.end());
    m_num_formals = formals.size();
    m_local_vars.insert(m_local_vars.end(),
			locals.begin(), locals.end());
}

int FrameDescriptor::getLocation(const Symbol* symbol) const
{
    for (int i = 0; i < m_local_vars.size(); ++i) {
	if (m_local_vars[i] == symbol) {
	    return i;
	}
    }
    return -1;
}

} // namespace JIT
} // namespace CXXR
