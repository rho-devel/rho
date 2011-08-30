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

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

#ifdef __cplusplus

#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/export.hpp>
#include "CXXR/BSerializer.hpp"
#include "CXXR/Expression.h"
#include "CXXR/Environment.h"
#include "CXXR/Symbol.h"

// Stack entry and stack top for pending promises, to be moved inside
// the Promise class in due course:
extern "C" {
    typedef struct RPRSTACK {
	SEXP promise;
	struct RPRSTACK *next;
    } RPRSTACK;

    extern RPRSTACK* R_PendingPromises;
}

namespace CXXR {
    /** @brief Placeholder for function argument.
     *
     * This class is used to handle function arguments within R's lazy
     * evaluation scheme.  (I'll document it better when I understand
     * it better!)
     */
    class Promise : public RObject {
    public:
	/**
	 * @param valgen pointer to RObject to be evaluated to provide
	 *          the value of the Promise.  Can be null.
	 *
	 * @param env pointer to the Environment in which \a valgen is
	 *          to be evaluated.
	 */
	Promise(RObject* valgen, Environment* env)
	    : RObject(PROMSXP), m_value(Symbol::unboundValue()),
	      m_valgen(valgen), m_environment(env), m_seen(false),
	      m_interrupted(false)
	{}

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

	/** @brief Has evaluation been interrupted by a jump?
	 *
	 * @return true iff evaluation of this Promise has been
	 * interrupted by a jump (JMPException).
	 */
	bool evaluationInterrupted() const
	{
	    return m_interrupted;
	}

	/** @brief Force the Promise.
	 *
	 * i.e. evaluate the Promise within its environment.
	 * Following this, the environment pointer is set null, thus
	 * possibly allowing the Environment to be garbage-collected.
	 *
	 * @return The result of evaluating the promise.
	 */
	RObject* force()
	{
	    return evaluate(0);
	}

	/** @brief Indicate whether evaluation has been interrupted.
	 *
	 * @param on true to indicate that evaluation of this promise
	 *           has been interrupted by a JMPException.
	 *
	 * @note To be removed from public interface in due course.
	 */
	void markEvaluationInterrupted(bool on)
	{
	    m_interrupted = on;
	}

	/** @brief Indicate whether this promise is under evaluation.
	 *
	 * @param on true to indicate that this promise is currently
	 *           under evaluation; otherwise false.
	 *
	 * @note To be removed from public interface in due course.
	 */
	void markUnderEvaluation(bool on)
	{
	    m_seen = on;
	}

	/** @brief Set value of the Promise.
	 *
	 * Once the value is set to something other than
	 * Symbol::unboundValue(), the environment pointer is set
	 * null.
	 *
	 * @param val Value to be associated with the Promise.
	 *
	 * @todo Should be private (or removed entirely), but current
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

	/** @brief Is this promise currently under evaluation?
	 *
	 * @return true iff this promise is currently under evaluation.
	 */
	bool underEvaluation() const
	{
	    return m_seen;
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

	// Virtual functions of RObject:
	RObject* evaluate(Environment* env);
	const char* typeName() const;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const;
    protected:
	// Virtual function of GCNode:
	void detachReferents();
	// For boost::serialization
	Promise() {} 
    private:
	friend class boost::serialization::access;
	template <class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    BSerializer::Frame frame("Promise");
	    ar & boost::serialization::base_object<RObject>(*this);

	    BSerializer::attrib("m_value");
	    ar & m_value;
	    BSerializer::attrib("m_valgen");
	    ar & m_valgen;
	    BSerializer::attrib("m_environment");
	    ar & m_environment;
	    BSerializer::attrib("m_seen");
	    ar & m_seen;
	    BSerializer::attrib("m_interrupted");
	    ar & m_interrupted;
	}

	GCEdge<> m_value;
	GCEdge<RObject> m_valgen;
	GCEdge<Environment> m_environment;
	bool m_seen;
	bool m_interrupted;

	// Declared private to ensure that Environment objects are
	// created only using 'new':
	~Promise() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Promise(const Promise&);
	Promise& operator=(const Promise&);
    };
}  // namespace CXXR

BOOST_CLASS_EXPORT(CXXR::Promise)

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

#ifdef __cplusplus
}
#endif

#endif /* RPROMISE_H */
