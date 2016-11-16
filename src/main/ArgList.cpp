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
 *  https://www.R-project.org/Licenses/
 */

/** @file ArgList.cpp
 *
 * Implementation of class ArgList.
 */

#include "rho/ArgList.hpp"

#include <list>
#include "rho/DottedArgs.hpp"
#include "rho/Environment.hpp"
#include "rho/Evaluator.hpp"
#include "rho/Expression.hpp"
#include "rho/Promise.hpp"
#include "rho/errors.hpp"

using namespace std;
using namespace rho;

// Implementation of ArgList::coerceTag() is in coerce.cpp

PairList* ArgList::append(RObject* value, const RObject* tag,
			  PairList* last_element) {
    PairList* object = PairList::cons(value, nullptr, tag);
    if (last_element)
	last_element->setTail(object);
    else
	setList(object);
    return object;
}

void ArgList::evaluateToArray(Environment* env,
			      int num_args, RObject** evaluated_args,
			      MissingArgHandling allow_missing)
{
    assert(allow_missing != MissingArgHandling::Drop);

    unsigned int arg_number = 0;
    for (const ConsCell& arg_cell : getExpandedArgs(env)) {
	RObject* arg = arg_cell.car();
	RObject* value;
	if (m_status == EVALUATED) {
	    value = arg;
	} else {
	    value = evaluateSingleArgument(arg, env, allow_missing,
					   arg_number + 1);
	}
	evaluated_args[arg_number] = value;
	++arg_number;
    }
    assert(arg_number == num_args);
}

void ArgList::evaluate(Environment* env,
		       MissingArgHandling allow_missing)
{
    // assert(allow_missing != MissingArgHandling::Drop);

    if (m_status == EVALUATED)
	return;

    auto expanded_args = getExpandedArgs(env);
    setList(nullptr);
    PairList* lastout = nullptr;

    unsigned int arg_number = 1;
    for (const ConsCell& arg : expanded_args) {
	RObject* value = evaluateSingleArgument(arg.car(), env,
						allow_missing, arg_number);
	lastout = append(value, arg.tag(), lastout);
	++arg_number;
    }
    m_status = EVALUATED;
}

RObject* ArgList::evaluateSingleArgument(const RObject* arg,
					 Environment* env,
					 MissingArgHandling allow_missing,
					 int arg_number) {
    // assert(allow_missing != MissingArgHandling::Drop);

    if (m_first_arg_env) {
	RObject* value = m_first_arg;
	m_first_arg = nullptr;
	m_first_arg_env = nullptr;
	return value;
     } else if (arg && arg->sexptype() == SYMSXP) {
	const Symbol* sym = static_cast<const Symbol*>(arg);
	if (sym == Symbol::missingArgument()) {
	    if (allow_missing == MissingArgHandling::Keep)
		return Symbol::missingArgument();
	    else Rf_error(_("argument %d is empty"), arg_number);
	} else if (allow_missing == MissingArgHandling::Keep
		   && isMissingArgument(sym, env->frame())) {
	    return Symbol::missingArgument();
	    // allow_missing = false && isMissingArgument() is handled in
	    // evaluate() below.
	}
    }
    return Evaluator::evaluate(const_cast<RObject*>(arg), env);
}

void ArgList::merge(const ConsCell* extraargs)
{
    if (m_status != PROMISED)
	Rf_error("Internal error: ArgList::merge() requires PROMISED ArgList");
    // Convert extraargs into a doubly linked list:
    typedef std::list<pair<const RObject*, RObject*> > Xargs;
    Xargs xargs;
    for (const ConsCell* cc = extraargs; cc; cc = cc->tail())
	xargs.push_back(make_pair(cc->tag(), cc->car()));
    // Apply overriding arg values supplied in extraargs:
    PairList* last = nullptr;
    for (PairList* pl = mutable_list(); pl; pl = pl->tail()) {
	last = pl;
	const RObject* tag = pl->tag();
	if (tag) {
	    Xargs::iterator it = xargs.begin();
	    while (it != xargs.end() && (*it).first != tag)
		++it;
	    if (it != xargs.end()) {
		pl->setCar((*it).second);
		xargs.erase(it);
	    }
	}
    }
    // Append remaining extraargs:
    for (Xargs::const_iterator it = xargs.begin(); it != xargs.end(); ++it) {
	last = append(it->second, it->first, last);
    }
}

pair<bool, RObject*> ArgList::firstArg(Environment* env)
{
    auto expanded_args = getExpandedArgs(env);
    if (expanded_args.empty()) {
	return pair<bool, RObject*>(false, nullptr);
    }
    RObject* first_arg = expanded_args.begin()->car();
    if (m_status == EVALUATED) {
	return make_pair(true, first_arg);
    }
    if (!first_arg)
	return pair<bool, RObject*>(true, nullptr);

    m_first_arg = Evaluator::evaluate(first_arg, env);
    m_first_arg_env = env;
    return make_pair(true, m_first_arg.get());
}

// TODO: these ought to handle '...'
RObject* ArgList::get(int position) const {
    ConsCell* cell = m_list.get();
    for (int i = 0; i < position && cell != nullptr; i++) {
	cell = cell->tail();
    }
    return cell ? cell->car() : nullptr;
}

const RObject* ArgList::getTag(int position) const {
    ConsCell* cell = m_list.get();
    for (int i = 0; i < position && cell != nullptr; i++) {
	cell = cell->tail();
    }
    return cell ? cell->tag() : nullptr;
}

bool ArgList::has3Dots() const {
  if (!list())
    return false;

  for (const ConsCell& cell : *list()) {
    if (cell.car() == R_DotsSymbol)
      return true;
  }
  return false;
}

bool ArgList::hasTags() const {
  if (!list())
    return false;

  for (const ConsCell& cell : *list()) {
    if (cell.tag())
      return true;
  }
  return false;
}

void ArgList::stripTags()
{
    for (PairList* p = mutable_list(); p; p = p->tail())
	p->setTag(nullptr);
}

const Symbol* ArgList::tag2Symbol(const RObject* tag)
{
    return ((!tag || tag->sexptype() == SYMSXP)
	    ? static_cast<const Symbol*>(tag)
	    : coerceTag(tag));
}

void ArgList::wrapInPromises(Environment* env,
			     const Expression* call)
{
    if (m_status == PROMISED)
	return;
    if (m_status == EVALUATED) {
	assert(call != nullptr);
	ArgList raw_args(call->tail(), RAW);
	raw_args.wrapInForcedPromises(env, this);
	m_status = PROMISED;
	m_list = raw_args.m_list;
	return;
    }
    assert(env != nullptr);

    auto expanded_args = getExpandedArgs(env);
    setList(nullptr);
    PairList* lastout = nullptr;

    for (const ConsCell& arg : expanded_args) {
	RObject* rawvalue = arg.car();
	const Symbol* tag = tag2Symbol(arg.tag());
	RObject* value = Symbol::missingArgument();
	if (m_first_arg_env) {
	    value = Promise::createEvaluatedPromise(rawvalue, m_first_arg);
	    m_first_arg = nullptr;
	    m_first_arg_env = nullptr;
	} else if (rawvalue != Symbol::missingArgument())
	    value = new Promise(rawvalue, env);
	lastout = append(value, tag, lastout);
    }

    m_status = PROMISED;
}

void ArgList::wrapInForcedPromises(Environment* env,
				   const ArgList* evaluated_values)
{
    assert(m_status == RAW);
    assert(evaluated_values->status() == EVALUATED);

    if (m_first_arg_env) {
	assert(m_first_arg.get() == evaluated_values->get(0));
    }

    auto expanded_args = getExpandedArgs(env);
    const auto& values = *evaluated_values->list();

    setList(nullptr);
    PairList* lastout = nullptr;

    auto arg = expanded_args.begin();
    auto value = values.begin();
    for (; arg != expanded_args.end() && value != values.end();
	 ++arg, ++value) {
	const RObject* expr = arg->car();
	const Symbol* tag = tag2Symbol(arg->tag());

	Promise* promise = Promise::createEvaluatedPromise(expr, value->car());
	lastout = append(promise, tag, lastout);
    }

    // Check to make sure that the lengths matched up OK.
    if (arg != expanded_args.end() || value != values.end()) {
	Rf_error(_("dispatch error"));
    }

    m_status = PROMISED;
}

void ArgList::const_iterator::handleDots()
{
    Frame::Binding* binding = m_env->findBinding(DotsSymbol);
    if (!binding) {
	Rf_error(_("'...' used in an incorrect context"));
    }
    RObject* dots = binding->forcedValue();
    if (!dots || dots == Symbol::missingArgument()) {
	// There aren't any dots to expand, so go on directly to the next
	// argument.
	++(*this);
	return;
    }
    if (dots->sexptype() != DOTSXP) {
	Rf_error(_("'...' used in an incorrect context"));
    }
    m_dots = static_cast<DottedArgs*>(dots);
}
