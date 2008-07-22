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

/** @file SpecialSymbol.h
 * @brief Class CXXR::SpecialSymbol and associated C interface.
 */

#ifndef SPECIALSYMBOL_H
#define SPECIALSYMBOL_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/GCRoot.h"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/String.h"

namespace CXXR {
    /** @brief Pseudo-objects, and base class for Symbol.
     *
     * As well as forming a base class for Symbol, this class is also
     * used to implement certain pseudo-objects (::R_MissingArg,
     * ::R_RestartToken and ::R_UnboundValue) that CR expects to have
     * ::SEXPTYPE SYMSXP.
     *
     * Each of these pseudo-objects is accessed by a static member
     * function which returns a pointer to it.  Because these
     * pointers are (necessarily) not const, it is possible for
     * clients to modify the attributes of the pseudo-objects; this is
     * untidy but probably innocuous.
     *
     * @note It would be desirable to get rid of the pseudo-objects
     * (and hence this class) algother.
     */
    class SpecialSymbol : public RObject {
    public:
	/** @brief Is this a double-dot symbol?
	 *
	 * @return true iff this symbol relates to an element of a
	 *         <tt>...</tt> argument list.
	 */
	virtual bool isDDSymbol() const;

	/** @brief Missing argument.
	 *
	 * @return a pointer to the 'missing argument' pseudo-object,
	 * which is identified as such by its address, not by its
	 * content.
	 */
	static SpecialSymbol* missingArgument()
	{
	    return s_missing_arg;
	}

	/** @brief Access name.
	 *
	 * @return const reference to the name of this Symbol.
	 */
	const String& name() const
	{
	    return m_name;
	}

	/** @brief Restart token.
	 *
	 * @return a pointer to the 'restart token' pseudo-object,
	 * which is identified as such by its address, not by its
	 * content.
	 */
	static SpecialSymbol* restartToken()
	{
	    return s_restart_token;
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "symbol (special)";
	}

	/** @brief Unbound value.
	 *
	 * This is used as the 'value' of a Symbol that has not been
	 * assigned any actual value.
	 *
	 * @return a pointer to the 'unbound value' pseudo-object,
	 * which is identified as such by its address, not by its
	 * content.
	 */
	static SpecialSymbol* unboundValue()
	{
	    return s_unbound_value;
	}

	/** @brief Access value.
	 *
	 * @return pointer to the value of this Symbol.  Returns
	 *         unboundValue() if no value is currently associated
	 *         with the Symbol (as will always be the case for
	 *         SpecialSymbols as opposed to full Symbols).
	 */
	virtual RObject* value();

	/** @brief Access value (const variant).
	 *
	 * @return const pointer to the value of this Symbol.  Returns
	 *         unboundValue() if no value is currently associated
	 *         with the Symbol (as will always be the case for
	 *         SpecialSymbols as opposed to full Symbols).
	 */
	const RObject* value() const;

	// Virtual function of RObject:
	const char* typeName() const;

	// Virtual function of GCNode:
	void visitChildren(const_visitor* v) const;
    protected:
	/**
	 * @param name The name to be associated with the created symbol.
	 */
	explicit SpecialSymbol(const String& name = *String::blank())
	    : RObject(SYMSXP), m_name(name)
	{}

	/**
	 * Declared protected to ensure that SpecialSymbol objects are
	 * allocated only using 'new'.
	 */
	~SpecialSymbol() {}
    private:
	static GCRoot<SpecialSymbol> s_missing_arg;
	static GCRoot<SpecialSymbol> s_restart_token;
	static GCRoot<SpecialSymbol> s_unbound_value;

	const String& m_name;

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	SpecialSymbol(const SpecialSymbol&);
	SpecialSymbol& operator=(const SpecialSymbol&);
    };
}  // namespace CXXR

extern "C" {
#endif

    extern SEXP R_MissingArg;
    extern SEXP R_RestartToken;
    extern SEXP R_UnboundValue;

    /** @brief Test if SYMSXP.
     *
     * @param s Pointer to a CXXR::RObject.
     *
     * @return TRUE iff s points to a CXXR::RObject with ::SEXPTYPE
     *         SYMSXP. 
     */
#ifndef __cplusplus
    Rboolean Rf_isSymbol(SEXP s);
#else
    inline Rboolean Rf_isSymbol(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == SYMSXP);
    }
#endif

    /** @brief Does symbol relate to a <tt>...</tt> expression?
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return \c TRUE iff this symbol denotes an element of a
     *         <tt>...</tt> expression.
     */
#ifndef __cplusplus
    Rboolean DDVAL(SEXP x);
#else
    inline Rboolean DDVAL(SEXP x)
    {
	using namespace CXXR;
	const SpecialSymbol& sym = *SEXP_downcast<SpecialSymbol*>(x);
	return Rboolean(sym.isDDSymbol());
    }
#endif

    /** @brief Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::String representings \a x's name.
     */
#ifndef __cplusplus
    SEXP PRINTNAME(SEXP x);
#else
    inline SEXP PRINTNAME(SEXP x)
    {
	const CXXR::SpecialSymbol& sym
	    = *CXXR::SEXP_downcast<CXXR::SpecialSymbol*>(x);
	return const_cast<CXXR::String*>(&sym.name());
    }
#endif

    /** @brief Symbol value.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::RObject representings \a x's value.
     *         Returns R_UnboundValue if no value is currently
     *         associated with the Symbol.
     */
#ifndef __cplusplus
    SEXP SYMVALUE(SEXP x);
#else
    inline SEXP SYMVALUE(SEXP x)
    {
	using namespace CXXR;
	SpecialSymbol& sym = *SEXP_downcast<SpecialSymbol*>(x);
	return sym.value();
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* SPECIALSYMBOL_H */
