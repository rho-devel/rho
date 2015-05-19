/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file Promise.h
 * @brief Class CXXR::Promise and associated C interface.
 */

#ifndef RPROMISE_H
#define RPROMISE_H

#include "CXXR/RObject.h"
// Just to pick up define of BYTECODE:
#include "CXXR/Evaluator.h"

#ifdef __cplusplus

#include "CXXR/Environment.h"
#include "CXXR/Symbol.h"

namespace CXXR {
    /** @brief Mechanism for deferred evaluation.
     *
     * This class is used to handle function arguments within R's lazy
     * evaluation scheme.  A Promise object encapsulates a pointer to
     * an arbitrary RObject (typically a Symbol or an Expression), and
     * a pointer to an Environment.  When the Promise is first
     * evaluated, the RObject is evaluated within the Environment, and
     * the result of evaluation returned as the value of the Promise.
     *
     * After the first evaluation, the result of evaluation is cached
     * within the Promise object, and the Environment pointer is set
     * null (thus possibly allowing the Environment to be
     * garbage-collected).  Subsequent evaluations of the Promise
     * object simply return the cached value.
     *
     * @note When a Promise is evaluated \e via a call to evaluate()
     * (the virtual function defined in class RObject), the \a env
     * parameter to evaluate() is ignored: evaluation uses only the
     * Environment encapsulated within the Promise object.  When an
     * RObject is known to be a Promise, it is suggested that
     * evaluation be carried out using the function Promise::force(),
     * which lacks the redundant parameter and is consequently clearer
     * to readers of the code.
     */
    class Promise : public RObject {
    public:
	/**
	 * @param valgen pointer to RObject to be evaluated to provide
	 *          the value of the Promise.  Can be null.
	 *
	 * @param env pointer to the Environment in which \a valgen is
	 *          to be evaluated.  If this pointer is null, the
	 *          value of the Promise is immediately set to be \a
	 *          valgen itself.
	 */
	Promise(RObject* valgen, Environment* env)
	    : RObject(PROMSXP), 
	      m_under_evaluation(false), m_interrupted(false)
	{
	    m_value = env ? Symbol::unboundValue() : valgen;
	    m_valgen = valgen;
	    m_environment = env;
	}

	/** @brief Access the environment of the Promise.
	 *
	 * @return Pointer to the environment of the Promise.  This
	 * will be a null pointer after the promise has been
	 * evaluated.
	 */
	Environment* environment() const
	{
	    return m_environment;
	}

	/** @brief Force the Promise.
	 *
	 * i.e. evaluate the value generator of the Promise within the
	 * Environment of the Promise.  Following this, the
	 * environment pointer is set null, thus possibly allowing the
	 * Environment to be garbage-collected.
	 *
	 * If this function is used on a Promise that has already been
	 * forced, it simply returns the previously computed value.
	 *
	 * @return The value of the Promise, i.e. the result of
	 * evaluating the value generator.
	 */
	RObject* force()
	{
	    return evaluate(nullptr);
	}

	/** @brief Not for general use.
	 *
	 * This function is used by ::isMissingArgument().  It
	 * implements some logic from CR's R_isMissing() which I don't
	 * fully understand.
	 *
	 * @return true iff ... well, read the code!
	 */
	bool isMissingSymbol() const;

	/** @brief Set value of the Promise.
	 *
	 * Once the value is set to something other than
	 * Symbol::unboundValue(), the environment pointer is set
	 * null.
	 *
	 * @param val Value to be associated with the Promise.
	 *
	 * @todo Should be private (or removed entirely), but currently
	 * still used in saveload.cpp.
	 */
	void setValue(RObject* val);

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "promise";
	}

	/** @brief Access the value of a Promise.
	 *
	 * @return pointer to the value of the Promise, or to
	 * Symbol::unboundValue() if it has not yet been evaluated.
	 */
	RObject* value()
	{
	    return m_value;
	}

	/** @brief RObject to be evaluated by the Promise.
	 *
	 * @return const pointer to the RObject to be evaluated by
	 * the Promise.
	 */
	const RObject* valueGenerator() const
	{
	    return m_valgen;
	}

	RObject* evaluate(Environment* env) override;
	const char* typeName() const override;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const override;
    protected:
	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	GCEdge<> m_value;
	GCEdge<RObject> m_valgen;
	GCEdge<Environment> m_environment;
	mutable bool m_under_evaluation;
	mutable bool m_interrupted;

	// Declared private to ensure that Promise objects are
	// created only using 'new':
	~Promise() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Promise(const Promise&);
	Promise& operator=(const Promise&);
    };

    /** @brief Use forced value if RObject is a Promise.
     *
     * @param object Pointer, possibly null, to an RObject.
     *
     * @return If \a object points to a Promise, the Promise is forced
     * and the value of the Promise returned.  Otherwise \a object
     * itself is returned.
     */
    inline RObject* forceIfPromise(RObject* object)
    {
	if (object && object->sexptype() == PROMSXP)
	    object = static_cast<Promise*>(object)->force();
	return object;
    }
}  // namespace CXXR

extern "C" {
#endif

    /** @brief Create a CXXR::Promise object.
     *
     * @param expr Expression to be evaluated to provide the value
     *          of the CXXR::Promise.
     *
     * @param env CXXR::Environment in which \a expr is to be evaluated.
     */
    SEXP Rf_mkPROMISE(SEXP expr, SEXP env);

    /** @brief Access the expression of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the expression to be evaluated by the
     *         CXXR::Promise. 
     */
#ifndef __cplusplus
    SEXP PRCODE(SEXP x);
#else
    inline SEXP PRCODE(SEXP x)
    {
	using namespace CXXR;
	const Promise& prom = *SEXP_downcast<Promise*>(x);
	return const_cast<RObject*>(prom.valueGenerator());
    }
#endif

    /** @brief Access the environment of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the environment in which the CXXR::Promise
     *         is to be  evaluated.  Set to a null pointer when the
     *         CXXR::Promise has been evaluated.
     */
#ifndef __cplusplus
    SEXP PRENV(SEXP x);
#else
    inline SEXP PRENV(SEXP x)
    {
	using namespace CXXR;
	const Promise& prom = *SEXP_downcast<Promise*>(x);
	return prom.environment();
    }
#endif

    /** @brief Access the value of a CXXR::Promise.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @return Pointer to the value of the CXXR::Promise, or to
     *         R_UnboundValue if it has not yet been evaluated..
     */
#ifndef __cplusplus
    SEXP PRVALUE(SEXP x);
#else
    inline SEXP PRVALUE(SEXP x)
    {
	using namespace CXXR;
	Promise& prom = *SEXP_downcast<Promise*>(x);
	return prom.value();
    }
#endif

    /** @brief Set the value of a CXXR::Promise.
     *
     * Once the value is set to something other than R_UnboundValue,
     * the environment pointer is set null.
     *
     * @param x Pointer to a CXXR::Promise (checked).
     *
     * @param v Pointer to the value to be assigned to the promise.
     *
     * @todo Replace this with a method call to evaluate the promise.
     */
    void SET_PRVALUE(SEXP x, SEXP v);

    // PREXPR() behaves similarly to valueGenerator(), but has special
    // (but apparently undocumented) behaviour if m_valgen (PRCODE) is
    // bytecode.  My guess is that if the bytecode evaluates to a
    // symbol, PREXPR returns that symbol, otherwise R_NilValue.
#ifdef BYTECODE
    SEXP R_PromiseExpr(SEXP);
#define PREXPR(e) R_PromiseExpr(e)
#else
#define PREXPR(e) PRCODE(e)
#endif

#ifdef __cplusplus
}
#endif

#endif /* RPROMISE_H */
