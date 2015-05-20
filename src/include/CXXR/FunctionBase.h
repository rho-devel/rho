/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file FunctionBase.h
 *
 * @brief Class CXXR::FunctionBase and associated C interface functions.
 */

#ifndef FUNCTIONBASE_H
#define FUNCTIONBASE_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    class ArgList;
    class Expression;
    class Environment;

    /** @brief Base class for function types.
     */
    class FunctionBase : public RObject {
    public:
	/** @brief Apply the function.
	 *
	 * @param arglist Non-null pointer to the ArgList containing
	 *          the list of arguments with which the function is
	 *          to be invoked.
	 *
	 * @param env Pointer to the Environment in which the function
	 *          is to be evaluated.
	 *
	 * @param call Pointer to the Expression calling the function.
	 *
	 * @return The result of applying the function.
         *
         * @note Using \a arglist after calls to this function may
         *    result in arguments being evaluated multiple time.
	 */
	virtual RObject* apply(ArgList* arglist, Environment* env,
			       const Expression* call) const = 0;

	/** @brief Enable/disable function tracing.
	 *
	 * @param on True iff function tracing is to be enabled.
	 */
	static void enableTracing(bool on)
	{
	    s_tracing_enabled = on;
	}

	/** @brief Is an RObject a FunctionBase?
	 *
	 * @param obj Pointer to RObject to be tested.  This may be a
	 *          null pointer, in which case the function returns
	 *          false.
	 *
	 * @return true iff \a obj is a FunctionBase.
	 */
	static bool isA(const RObject* obj)
	{
	    // We could of course use dynamic_cast here, but the
	    // following is probably faster:
	    if (!obj) return false;
	    SEXPTYPE st = obj->sexptype();
	    return st == CLOSXP || st == BUILTINSXP || st == SPECIALSXP;
	}

	/** @brief Produce a tracing report if appropriate.
	 *
	 * A tracing report is generated if this function is set to be
	 * traced and tracing in general is enabled.
	 *
	 * @param call The call to this function to be reported.
	 */
	void maybeTrace(const Expression* call) const
	{
	    if (traced() && tracingEnabled())
		reportCall(call);
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "(function type)";
	}

	/** @brief Set tracing status.
	 *
	 * @param on True iff it is required to monitor calls to this
	 *           function object.
	 */
	void setTracing(bool on)
	{
	    m_traced = on;
	}

	/** @brief Is this function being traced?
	 *
	 * @return true if this function object is currently being
	 * traced.
	 */
	bool traced() const
	{
	    return m_traced;
	}

	/** @brief If function tracing currently enabled?
	 *
	 * @return true iff function tracing is currently enabled.
	 */
	static bool tracingEnabled()
	{
	    return s_tracing_enabled;
	}
    protected:
	/**
	 * @param stype Required type of the FunctionBase.
	 */
	explicit FunctionBase(SEXPTYPE stype)
	    : RObject(stype), m_traced(false)
	{}

	/** @brief Copy constructor.
	 *
	 * @param pattern FunctionBase to be copied.
	 */
	FunctionBase(const FunctionBase& pattern)
	    : RObject(pattern), m_traced(false)
	{}

	virtual ~FunctionBase() {}
    private:
	static bool s_tracing_enabled;
	bool m_traced;

	static void reportCall(const Expression* call);
    };
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /** @brief Get function tracing status.
     *
     * @param x Pointer to a CXXR::FunctionBase (checked), or a null
     *          pointer.
     *
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     * null pointer.
     */
#ifndef __cplusplus
    int RTRACE(SEXP x);
#else
    inline int RTRACE(SEXP x)
    {
	using namespace CXXR;
	if (!x) return 0;
	const FunctionBase& f = *SEXP_downcast<const FunctionBase*>(x);
	return f.traced();
    }
#endif

    /** @brief Set function tracing status.
     *
     * @param x Pointer to a CXXR::FunctionBase (checked), or a null
     * pointer.
     *
     * @param v The desired tracing status: non-zero if tracing is
     * required.
     */
#ifndef __cplusplus
    void SET_RTRACE(SEXP x, int v);
#else
    inline void SET_RTRACE(SEXP x, int v)
    {
	using namespace CXXR;
	FunctionBase* f = SEXP_downcast<FunctionBase*>(x);
	f->setTracing(v != 0);
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* FUNCTIONBASE_H */
