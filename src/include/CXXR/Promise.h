/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008 Andrew R. Runnalls, subject to such other
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

#include "CXXR/Expression.h"
#include "CXXR/Environment.h"
#include "CXXR/SpecialSymbol.h"

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
	 * @param env Environment in which \a valgen is to be evaluated.
	 */
	Promise(const RObject* valgen, const Environment& env)
	    : RObject(PROMSXP), m_value(SpecialSymbol::unboundValue()),
	      m_valgen(valgen), m_environment(&env)
	{}

	/** @brief Access the environment of the Promise.
	 *
	 * @return Pointer to the environment of the Promise.  This
	 * will be a null pointer after the promise has been
	 * evaluated.
	 */
	const Environment* environment() const
	{
	    return m_environment;
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

	/** @brief Set value of the Promise.
	 *
	 * Once the value is set to something other than
	 * SpecialSymbol::unboundValue(), the environment pointer is
	 * set null.
	 *
	 * @param val Value to be associated with the Promise.
	 *
	 * @todo Replace this with a method to evaluate the promise.
	 * Possibly have method value() itself force the promise.
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
	 * SpecialSymbol::unboundValue() if it has not yet been
	 * evaluated.
	 */
	const RObject* value() const
	{
	    return m_value;
	}

	// Virtual function of RObject:
	const char* typeName() const;

	// Virtual function of GCNode:
	void visitChildren(const_visitor* v) const;
    private:
	RObject* m_value;
	const RObject* m_valgen;
	const Environment* m_environment;

	// Declared private to ensure that Environment objects are
	// created only using 'new':
	~Promise() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Promise(const Promise&);
	Promise& operator=(const Promise&);
    };
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
    SEXP Rf_mkPROMISE(SEXP expr, SEXP rho);

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
	const CXXR::Promise& prom = *CXXR::SEXP_downcast<CXXR::Promise*>(x);
	return const_cast<CXXR::RObject*>(prom.valueGenerator());
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
	const CXXR::Promise& prom = *CXXR::SEXP_downcast<CXXR::Promise*>(x);
	return const_cast<CXXR::Environment*>(prom.environment());
    }
#endif

    /**
     * @param x Pointer to a promise.
     * @return ?
     * @deprecated Will need to be fixed.
     */
#ifndef __cplusplus
    int PRSEEN(SEXP x);
#else
    inline int PRSEEN(SEXP x) {return x->m_gpbits;}
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
	const CXXR::Promise& prom = *CXXR::SEXP_downcast<CXXR::Promise*>(x);
	return const_cast<CXXR::RObject*>(prom.value());
    }
#endif

    /**
     * @param x Pointer to a promise.
     * @deprecated Will need to be fixed.
     */
#ifndef __cplusplus
    void SET_PRSEEN(SEXP x, int v);
#else
    inline void SET_PRSEEN(SEXP x, int v) {x->m_gpbits = v;}
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
#ifndef __cplusplus
    void SET_PRVALUE(SEXP x, SEXP v);
#else
    inline void SET_PRVALUE(SEXP x, SEXP v)
    {
	CXXR::Promise* prom = CXXR::SEXP_downcast<CXXR::Promise*>(x);
	prom->setValue(v);
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* RPROMISE_H */
