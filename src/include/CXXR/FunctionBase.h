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
 *  Copyright (C) 1999-2007   The R Development Core Team.
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
    /** @brief Base class for function types.
     */
    class FunctionBase : public RObject {
    public:
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
	    m_tracing = on;
	}

	/** @brief Is this function being traced?
	 *
	 * @return true if this function object is currently being
	 * traced.
	 */
	bool tracing() const
	{
	    return m_tracing;
	}
    protected:
	/**
	 * @param stype Required type of the FunctionBase.
	 */
	explicit FunctionBase(SEXPTYPE stype)
	    : RObject(stype), m_tracing(false)
	{}

	virtual ~FunctionBase() {}
    private:
	bool m_tracing;
    };
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /** @brief Get object tracing status.
     *
     * @param x Pointer to a CXXR::FunctionBase (checked), or a null
     *          pointer.
     *
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     * null pointer.
     */
#ifndef __cplusplus
    int TRACE(SEXP x);
#else
    inline int TRACE(SEXP x)
    {
	using namespace CXXR;
	if (!x) return 0;
	const FunctionBase& f = *SEXP_downcast<const FunctionBase*>(x);
	return f.tracing();
    }
#endif

#ifndef __cplusplus
    void SET_TRACE(SEXP x, int v);
#else
    inline void SET_TRACE(SEXP x, int v)
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
