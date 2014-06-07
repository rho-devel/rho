/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
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

/** @file SEXPTYPE.h
 *
 * @brief The SEXPTYPE enumeration.
 */

#ifndef SEXPTYPE_H
#define SEXPTYPE_H

#ifdef __cplusplus
extern "C" {
#endif

    /* Comment from CR code:
     * Fundamental Data Types:  These are largely Lisp
     * influenced structures, with the exception of LGLSXP,
     * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
     * element types for S-like data objects.

     * Note that the gap of 11 and 12 below is because of
     * the withdrawal of native "factor" and "ordered" types.
     *
     *			--> TypeTable[] in ../main/util.c for  typeof()
     */

    /*  These exact numeric values are seldom used, but they are, e.g., in
     *  ../main/subassign.c
     */

    /** @enum SEXPTYPE
     *
     * @brief CR's object type identification.
     *
     * This enumeration is used within CR to identify different types
     * of R object.  In CXXR the same purpose could be (and sometimes
     * is) achieved by C++ run-time type information (RTTI), virtual
     * function despatch etc.  However, a ::SEXPTYPE field is retained
     * within each CXXR::RObject for backwards compatibility, and indeed
     * efficiency.
     */
    typedef enum {
	NILSXP	    = 0,    /**< NULL. In CXXR no CXXR::RObject has
			     * this type, but for backward
			     * compatibility TYPEOF will return ::NILSXP
			     * if passed a null pointer.
			     */
	SYMSXP	    = 1,    /**< symbols, implemented in class
			       CXXR::Symbol. */
	LISTSXP	    = 2,    /**< lists of dotted pairs, implemented in
			       class CXXR::PairList. */
	CLOSXP	    = 3,    /**< closures, implemented in class
			       CXXR::Closure. */
	ENVSXP	    = 4,    /**< environments, implemented in class
			       CXXR::Environment. */
	PROMSXP	    = 5,    /**< promises: [un]evaluated closure
			       arguments, implemented in class
			       CXXR::Promise. */
	LANGSXP	    = 6,    /**< language constructs (special lists),
			       implemented in class CXXR::Expression. */
	SPECIALSXP  = 7,    /**< special forms, implemented in class
			       CXXR::BuiltInFunction. */
	BUILTINSXP  = 8,    /**< builtin non-special forms, also
			       implemented in class
			       CXXR::BuiltInFunction. */
	CHARSXP	    = 9,    /**< "scalar" string type (internal only),
			       implemented in class CXXR::String. */
	LGLSXP	    = 10,   /**< logical vectors, implemented in class
			       CXXR::LogicalVector. */
	INTSXP	    = 13,   /**< integer vectors, implemented in class
			       CXXR::IntVector. */
	REALSXP	    = 14,   /**< real variables, implemented in class
			       CXXR::RealVector. */
	CPLXSXP	    = 15,   /**< complex variables, implemented in
			       class CXXR::ComplexVector. */
	STRSXP	    = 16,   /**< string vectors, implemented in class
			       CXXR::StringVector. */
	DOTSXP	    = 17,   /**< dot-dot-dot objects, implemented in
			       class CXXR::DottedArgs. */
	ANYSXP	    = 18,   /**< Used to make "any" args work.  No
			       CXXR::RObject has this type. */
	VECSXP	    = 19,   /**< generic vectors, implemented in class
			       CXXR::ListVector. */
	EXPRSXP	    = 20,   /**< expression vectors, implemented in
			       class CXXR::ExpressionVector. */
	BCODESXP    = 21,   /**< byte code, implemented in class
			       CXXR::ByteCode. */
	EXTPTRSXP   = 22,   /**< external pointers, implemented in
			       class CXXR::ExternalPointer. */
	WEAKREFSXP  = 23,   /**< weak references, implemented in class
			       CXXR::WeakRef. */
	RAWSXP      = 24,   /**< raw bytes, implemented in class
			       CXXR::RawVector. */
	S4SXP       = 25,   /**< S4 object not inheriting from another
			     *   ::SEXPTYPE, implemented in class
			     *   CXXR::S4Object.
			     */

	CXXSXP      = 43,   /**< object types specific to CXXR.*/
	                    /* (43 = ASCII +) */

	BAILSXP     = 44,   /**< Object used to implement indirect flow of
			     *   control in R without using a C++ exception.
			     */

	FUNSXP	    = 99    /**< Closure or Builtin.  No CXXR::RObject has
			       this type. */
    } SEXPTYPE;

#ifdef __cplusplus
}  // extern "C"
#endif

#endif /* SEXPTYPE_H */
