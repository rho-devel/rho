/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
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
 *  Andrew Runnalls (C) 2007
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

/** @file BuiltInFunction.h
 * @brief Class CXXR::BuiltInFunction and associated C interface.
 *
 * CXXR::BuiltInFunction implements functionality common to BUILTINSXP
 * and SPECIALSXP.
 */

#ifndef BUILTINFUNCTION_H
#define BUILTINFUNCTION_H

#include "CXXR/FunctionBase.h"

#ifdef __cplusplus
extern "C" {
#endif

    /** @brief The type of the do_xxxx functions.
     *
     * These are the built-in R functions.
     */
    typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

#ifdef __cplusplus
}  // extern "C"

namespace CXXR {
    /** @brief R function implemented within the interpreter.
     *
     * This class provides functionality common to BUILTINSXP and
     * SPECIALSXP, and is basically an offset into a table of
     * functions.
     *
     * @note This is an interim implementation: expect major changes
     * in the future.
     */
    class BuiltInFunction : public FunctionBase {
    public:
	/** @brief Kind of function, used mainly in deparsing.
	 */
	enum Kind {
	    PP_INVALID  =  0,
	    PP_ASSIGN   =  1,
	    PP_ASSIGN2  =  2,
	    PP_BINARY   =  3,
	    PP_BINARY2  =  4,
	    PP_BREAK    =  5,
	    PP_CURLY    =  6,
	    PP_FOR      =  7,
	    PP_FUNCALL  =  8,
	    PP_FUNCTION =  9,
	    PP_IF 	= 10,
	    PP_NEXT 	= 11,
	    PP_PAREN    = 12,
	    PP_RETURN   = 13,
	    PP_SUBASS   = 14,
	    PP_SUBSET   = 15,
	    PP_WHILE 	= 16,
	    PP_UNARY 	= 17,
	    PP_DOLLAR 	= 18,
	    PP_FOREIGN 	= 19,
	    PP_REPEAT 	= 20
	};

	/** @brief Precedence level of function.
	 */
	enum Precedence {
	    PREC_FN	 = 0,
	    PREC_LEFT    = 1,
	    PREC_EQ	 = 2,
	    PREC_RIGHT	 = 3,
	    PREC_TILDE	 = 4,
	    PREC_OR	 = 5,
	    PREC_AND	 = 6,
	    PREC_NOT	 = 7,
	    PREC_COMPARE = 8,
	    PREC_SUM	 = 9,
	    PREC_PROD	 = 10,
	    PREC_PERCENT = 11,
	    PREC_COLON	 = 12,
	    PREC_SIGN	 = 13,
	    PREC_POWER	 = 14,
	    PREC_DOLLAR  = 15,
	    PREC_NS	 = 16,
	    PREC_SUBSET	 = 17
	};

	/** @brief 'Arity' of the function.
	 *
	 * @return The number of arguments expected by the function.
	 * A return value of -1 signifies that any number of arguments
	 * is acceptable.
	 */
	int arity() const
	{
	    return s_function_table[m_offset].arity;
	}

	/** @brief C/C++ function implementing this R function.
	 *
	 * @return Pointer to the C/C++ function implementing this R
	 * function.
	 */
	CCODE function() const
	{
	    return m_function;
	}

	/** @brief Find a built-in function within the function table.
	 *
	 * @param name Name of the sought built-in function.
	 *
	 * @return Index (counting from 0) of the function within the
	 * table, or -1 if there is no built-in function with the
	 * given name.
	 *
	 * @deprecated Function table details ought not to be exposed
	 * outside the class.
	 */
	static int indexInTable(const char* name);

	/** @brief Kind of built-in function.
	 *
	 * (Used mainly in deparsing.)
	 *
	 * @return The Kind of the function.
	 */
	Kind kind() const
	{
	    return s_function_table[m_offset].gram.kind;
	}

	/** @brief Create a BuiltInFunction object corresponding to a
	 * table entry.
	 * 
	 * @param i Index within the table for which a BuiltInFunction
	 * object is to be created.  No range check is applied.
	 *
	 * @return A pointer to a newly created BuiltInFunction.  This
	 * will be either an OrdinaryBuiltInFunction or a
	 * SpecialBuiltInFunction, according to the table data.
	 *
	 * @deprecated Function table details ought not to be exposed
	 * outside the class.
	 */
	static BuiltInFunction* make(unsigned int i);

	/** @brief Name of function.
	 *
	 * @return The textual name of this function.
	 */
	const char* name() const
	{
	    return s_function_table[m_offset].name;
	}

	/** @brief Get table offset.
	 *
	 * @return The offset into the table of functions.
	 */
	unsigned int offset() const
	{
	    return m_offset;
	}

	/** @brief Precedence of built-in function.
	 *
	 * @return The Precedence of the function.
	 */
	Precedence precedence() const
	{
	    return s_function_table[m_offset].gram.precedence;
	}

	// Used to implement PRIMPRINT.  Get rid of it soon.
	int printHandling() const
	{
	    return m_result_printing_mode;
	}

	/** @brief Is a built-in function right-associative?
	 *
	 * @return true iff the function is right-associative.
	 */
	bool rightAssociative() const
	{
	    return s_function_table[m_offset].gram.rightassoc;
	}

	/** @brief Index of variant behaviour.
	 *
	 * Where a single C/C++ implements more than one built-in R
	 * function, this function provides the C/C++ code with an
	 * index indicating which R function is to be implemented.
	 *
	 * @return index of the required behaviour; interpretation is
	 * up to the C/C++ function called.
	 */
	unsigned int variant() const
	{
	    return s_function_table[m_offset].variant;
	}

	/** @brief Must this function be called via .Internal()?
	 *
	 * @return true iff this function must be called via
	 * .Internal()?
	 */
	bool viaDotInternal() const
	{
	    return (s_function_table[m_offset].flags%100)/10 == 1;
	}

	// Virtual function of FunctionBase:
	RObject* apply(Expression* call, Environment* env);
    protected:
	/**
	 * @param offset The required table offset.  (Not
	 * range-checked in any way.)
	 *
	 * @param evaluate true iff this is to be a BUILTINSXP;
	 *          otherwise it will be a SPECIALSXP.
	 */
	BuiltInFunction(unsigned int offset, bool evaluate)
	    : FunctionBase(evaluate ? BUILTINSXP : SPECIALSXP),
	      m_offset(offset), m_function(s_function_table[offset].cfun)
	{
	    unsigned int pmdigit = (s_function_table[offset].flags/100)%10;
	    m_result_printing_mode = ResultPrintingMode(pmdigit);
	}
    private:
	// 'Pretty-print' information:
	struct PPinfo {
	    Kind kind;
	    Precedence precedence;
	    unsigned int rightassoc;
	};

	struct TableEntry {
	    const char* name;  // name of function
	    CCODE cfun;        // pointer to relevant do_xxx function
	    unsigned int variant;  // used to select alternative
				   // behaviours within the do_xxx
				   // function
	    unsigned int flags;  // misc flags: see names.cpp
	    int	arity;  // function arity; -1 means 'any'
	    PPinfo gram;  // 'pretty-print' information
	};

	// SOFT_ON signifies that result printing should be enabled
	// before calling m_function, but that if m_function disables
	// result printing, this should not be overridden.
	enum ResultPrintingMode {FORCE_ON = 0, FORCE_OFF, SOFT_ON};

	// Actually an array:
	static TableEntry* s_function_table;

	unsigned int m_offset;
	CCODE m_function;
	ResultPrintingMode m_result_printing_mode;

	static void cleanup()
	{}

	// Put primitive functions into the base environment, and
	// internal functions into the DotInternalTable:
	static void initialize();

	// 'apply' behaviour where SPECIALSXP differs from BUILTINSXP:
	virtual RObject* innerApply(Expression* call, Environment* env) = 0;

	friend class SchwarzCounter<BuiltInFunction>;
    };
}  // namespace CXXR

// Force Environment and Symbol classes to be initialised first:
#include "CXXR/Environment.h"

namespace {
    CXXR::SchwarzCounter<CXXR::BuiltInFunction> bif_schwarz_ctr;
}

// Old-style accessor functions.  Get rid of these in due course.

inline int PRIMARITY(SEXP x)
{
    using namespace CXXR;
    BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
    return bif.arity();
}
    
inline CCODE PRIMFUN(SEXP x)
{
    using namespace CXXR;
    BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
    return bif.function();
}
    
inline int PRIMINTERNAL(SEXP x)
{
    using namespace CXXR;
    BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
    return bif.viaDotInternal();
}
    
inline int PRIMPRINT(SEXP x)
{
    using namespace CXXR;
    BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
    return bif.printHandling();
}
    
extern "C" {
#endif

#ifndef __cplusplus
    const char* PRIMNAME(SEXP x);
#else
inline const char* PRIMNAME(SEXP x)
{
    using namespace CXXR;
    BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
    return bif.name();
}
#endif
  
    /** @brief Get offset of a CXXR::BuiltInFunction.
     *
     * @param x Pointer to a CXXR::BuiltInFunction.
     *
     * @return The offset of this function within the function table.
     */
#ifndef __cplusplus
    int PRIMOFFSET(SEXP x);
#else
    inline int PRIMOFFSET(SEXP x)
    {
	using namespace CXXR;
	BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
	return bif.offset();
    }
#endif

#ifndef __cplusplus
    unsigned int PRIMVAL(SEXP x);
#else
inline unsigned int PRIMVAL(SEXP x)
{
    using namespace CXXR;
    BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
    return bif.variant();
}
#endif
  
#ifdef __cplusplus
}
#endif

#endif /* BUILTINFUNCTION_H */
