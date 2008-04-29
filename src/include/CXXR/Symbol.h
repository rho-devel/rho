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

/** @file Symbol.h
 * @brief Class CXXR::Symbol and associated C interface.
 */

#ifndef RSYMBOL_H
#define RSYMBOL_H

#include "CXXR/SpecialSymbol.h"

#ifdef __cplusplus

#include "CXXR/BuiltInFunction.h"

namespace CXXR {
    /** @brief Class used to represent R symbols.
     *
     * A symbol associates a String object with an arbitrary RObject,
     * and (or?) with a BuiltInFunction object.  (I'll document it
     * better when I understand it better! - arr)
     */
    class Symbol : public SpecialSymbol {
    public:
	/**
	 * @param name Pointer to String object representing the name
	 *          of the symbol.  Names of the form
	 *          <tt>..<em>n</em></tt>, where n is a (non-negative)
	 *          decimal integer signify that the Symbol to be
	 *          constructed relates to an element of a
	 *          <tt>...</tt> argument list.
	 *
	 * @param val Value to be associated with the constructed
	 *          Symbol object.  The default value is a placeholder
	 *          signifying that no value has yet been associated
	 *          with the Symbol.
	 *
	 * @param internal_func Pointer to an internal function to be
	 *          denoted by the constructed Symbol.
	 */
	explicit Symbol(const String& name, RObject* val = unboundValue(),
			const BuiltInFunction* internal_func = 0);

	/** @brief Access internal function.
	 *
	 * @return const pointer to the internal function (if any)
	 *         denoted by this Symbol.
	 */
	const BuiltInFunction* internalFunction() const
	{
	    return m_internalfunc;
	}

	/** @brief Is this a double-dot symbol?
	 *
	 * @return true iff this symbol relates to an element of a
	 *         <tt>...</tt> argument list.
	 */
	bool isDDSymbol() const
	{
	    return m_flags[s_DDBIT];
	}

	/** @brief Set internal function.
	 *
	 * @param func Pointer to the internal function now to be
	 *          denoted by this symbol.  A null pointer is
	 *          permissible.
	 *
	 * @note It would be better if this was set exclusively during
	 * construction.
	 */
	void setInternalFunction(const BuiltInFunction* fun)
	{
	    m_internalfunc = fun;
	    devolveAge(m_internalfunc);
	}

	/** @brief Set value.
	 *
	 * @param val Pointer to the RObject now to be considered as
	 *            the value of this symbol.  A null pointer or
	 *            unboundValue() are permissible values of \a val.
	 */
	void setValue(RObject* val)
	{
	    m_value = val;
	    devolveAge(m_value);
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "symbol";
	}

	/** @brief Access value.
	 *
	 * @return pointer to the value of this Symbol.  Returns
	 *         unboundValue() if no value is currently associated
	 *         with the Symbol.
	 */
	RObject* value()
	{
	    return m_value;
	}

	/** @brief Access value (const variant).
	 *
	 * @return const pointer to the value of this Symbol.  Returns
	 *         unboundValue() if no value is currently associated
	 *         with the Symbol.
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
	static const unsigned int s_DDBIT = 0;

	RObject* m_value;
	const BuiltInFunction* m_internalfunc;

	// Declared private to ensure that Symbol objects are
	// allocated only using 'new':
	~Symbol() {}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Symbol(const Symbol&);
	Symbol& operator=(const Symbol&);
    };
}  // namespace CXXR

extern "C" {
#endif

    /* Symbol Table Shortcuts */
    extern SEXP R_Bracket2Symbol;   /* "[[" */
    extern SEXP R_BracketSymbol;    /* "[" */
    extern SEXP R_BraceSymbol;      /* "{" */
    extern SEXP R_ClassSymbol;	/* "class" */
    extern SEXP R_DimNamesSymbol;   /* "dimnames" */
    extern SEXP R_DimSymbol;	/* "dim" */
    extern SEXP R_DollarSymbol;	/* "$" */
    extern SEXP R_DotsSymbol;	/* "..." */
    extern SEXP R_DropSymbol;	/* "drop" */
    extern SEXP R_LevelsSymbol;	/* "levels" */
    extern SEXP R_ModeSymbol;	/* "mode" */
    extern SEXP R_NamesSymbol;	/* "names" */
    extern SEXP R_RowNamesSymbol;   /* "row.names" */
    extern SEXP R_SeedsSymbol;	/* ".Random.seed" */
    extern SEXP R_TspSymbol;	/* "tsp" */

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
	const CXXR::Symbol& sym = *CXXR::SEXP_downcast<CXXR::Symbol*>(x);
	return Rboolean(sym.isDDSymbol());
    }
#endif

    /** @brief Internal function value.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return If \a x denotes an internal function, a pointer to
     *         the appropriate CXXR::BuiltInFunction, otherwise a null
     *         pointer..
     */
#ifndef __cplusplus
    SEXP INTERNAL(SEXP x);
#else
    inline SEXP INTERNAL(SEXP x)
    {
	CXXR::Symbol& sym = *CXXR::SEXP_downcast<CXXR::Symbol*>(x);
	return const_cast<CXXR::BuiltInFunction*>(sym.internalFunction());
    }
#endif

    /** @brief Create a CXXR::Symbol object.
     *
     * @param name Pointer to a CXXR::String object (checked) to be
     *          taken as the name of the constructed symbol.
     *
     * @param val Pointer to the CXXR::RObject to be considered as
     *          the value of the constructed symbol.  A null pointer or
     *          R_UnboundValue are permissible values of \a val.
     *
     * @return Pointer to the created CXXR::Symbol object.
     */
    SEXP Rf_mkSYMSXP(SEXP name, SEXP value);

    /** @brief Set internal function denoted by a symbol.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param func Pointer to the CXXR::BuiltInFunction (checked) to
     *          be denoted by this symbol.  A null pointer is
     *          permissible.
     *
     * @note It would be better if this was set exclusively during
     * construction.
     */
    void SET_INTERNAL(SEXP x, SEXP v);

    /** @brief Set symbol's value.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @param val Pointer to the RObject now to be considered as
     *            the value of this symbol.  A null pointer or
     *            R_UnboundValue are permissible values of \a val.
     */
#ifndef __cplusplus
    void SET_SYMVALUE(SEXP x, SEXP v);
#else
    inline void SET_SYMVALUE(SEXP x, SEXP v)
    {
	CXXR::Symbol& sym = *CXXR::SEXP_downcast<CXXR::Symbol*>(x);
	sym.setValue(v);
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
	CXXR::Symbol& sym = *CXXR::SEXP_downcast<CXXR::Symbol*>(x);
	return sym.value();
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* RSYMBOL_H */
