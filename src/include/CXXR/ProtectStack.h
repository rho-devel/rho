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

/** @file ProtectStack.h
 *
 * @brief Class CXXR::ProtectStack and associated C interface.
 *
 * CXXR::ProtectStack encapsulates the functionality of the CR pointer
 * protection stack.
 *
 * See the paragraph 'Caller Protects' in the description of class
 * CXXR::GCStackRoot for recommended coding policy.
 */

#ifndef PROTECTSTACK_H
#define PROTECTSTACK_H 1

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/NodeStack.hpp"

namespace CXXR {
    /** @brief Class implementing CR's 'pointer protection stack'.
     *
     * All members of this class are static.
     */
    class ProtectStack {
    public:
	/** @brief Object constraining lifetime of ProtectStack entries.
	 *
	 * Scope objects must be declared on the processor stack
	 * (i.e. as C++ automatic variables).  Any entry pushed onto
	 * the ProtectStack during the lifetime of a Scope object will
	 * be automatically popped off when that lifetime comes to an
	 * end, i.e. when the Scope object itself goes out of scope.
	 */
	class Scope : public NodeStack::Scope {
	public:
	    Scope()
		: NodeStack::Scope(s_stack)
	    {}
	};

	/** @brief Push a node pointer onto the PPS.
	 *
	 * Push a node pointer onto the C pointer protection stack.
	 *
	 * @param node Pointer to the node to be protected from the
	 *          garbage collector.
	 *
	 * @return Index of the stack cell thus created, for
	 *          subsequent use with reprotect().
	 */
	static unsigned int protect(RObject* node)
	{
	    return s_stack->push(node);
	}

	/** @brief Change the target of a pointer on the PPS.
	 *
	 * Change the node that a particular cell in the C pointer
	 * protection stack protects.  As a consistency check, it is
	 * required that the reprotect takes place within the same
	 * ProtectStack::Scope as the corresponding protect.
	 *
	 * @param node Pointer to the node now to be protected from
	 *          the garbage collector by the designated stack
	 *          cell.  (Not necessarily a different node from the
	 *          one currently protected.)
	 *
	 * @param index Index (as returned by protect() ) of the stack
	 *          cell to be retargeted to node.  Must be less than
	 *          the current size of the C pointer protection
	 *          stack (checked).
	 */
	static void reprotect(RObject* node, unsigned int index)
	{
	    s_stack->retarget(node, index);
	}

	/** @brief Restore PPS to a previous size.
	 *
	 * Restore the C pointer protection stack to a previous size by
	 * popping elements off the top.
	 *
	 * @param new_size The size to which the stack is to be
	 *          restored.  Must not be greater than the current
	 *          size.
	 *
	 * @note In future this method will probably cease to be
	 * available, since the use of the ProtectStack::Scope class
	 * is preferable.
	 */
	static void restoreSize(size_t new_size);

	/** @brief Current size of PPS.
	 *
	 * @return the current size of the C pointer protection stack.
	 *
	 * @note This method is intended for use in conjunction with
	 * restoreSize(), and may cease to be public in future.
	 */
	static size_t size()
	{
	    return s_stack->size();
	}

	/** @brief Pop pointers from the PPS.
	 *
	 * Pop cells from the C pointer protection stack.  As a
	 * consistency check, it is required that the unprotect takes
	 * place within the same ProtectStack::Scope as the
	 * corresponding protect.
	 *
	 * @param count Number of cells to be popped.  Must not be
	 *          larger than the current size of the C pointer
	 *          protection stack.
	 */
	static void unprotect(unsigned int count = 1)
	{
	    s_stack->pop(count);
	}

	/**
	 * Removes from the C pointer protection stack the uppermost
	 * stack cell containing a pointer to a specified node, and
	 * drops all the stack cells above it by one place.
	 *
	 * @param node Pointer to the node whose cell is to be removed
	 *          from the C pointer protection stack.
	 *
	 * @deprecated Utterly.
	 */
	static void unprotectPtr(RObject* node)
	{
	    s_stack->eraseTopmost(node);
	}

	/** @brief Conduct a const visitor to protected objects.
	 *
	 * Conduct a GCNode::const_visitor object to each node on the
	 * pointer protection stack.
	 *
	 * @param v Pointer to the const_visitor object.
	 */
	static void visitRoots(GCNode::const_visitor* v)
	{
	    s_stack->visitRoots(v);
	}
    private:
	friend class GCNode;

	static NodeStack* s_stack;

	// Not implemented:
	ProtectStack();

	// Clean up static data at end of run (called by
	// GCNode::SchwarzCtr destructor):
	static void cleanup();

	// Initialize static data (called by GCNode::SchwarzCtr
	// constructor):
	static void initialize();

	// Put all entries into the protecting state:
	static void protectAll()
	{
	    s_stack->protectAll();
	}
    };
}  // namespace CXXR

extern "C" {
#endif /* __cplusplus */

    /* ***** C interface ***** */

    typedef unsigned int PROTECT_INDEX;

    /** @brief Push a node pointer onto the C pointer protection stack.
     *
     * Push a node pointer onto the C pointer protection stack, and
     * record the index of the resulting stack cell (for subsequent
     * use with R_Reprotect).
     *
     * @param node Pointer to the node to be protected from the
     *          garbage collector.
     *
     * @param iptr Pointer to a location in which the stack cell index
     *          is to be stored.
     */
#ifndef __cplusplus
    void R_ProtectWithIndex(SEXP node, PROTECT_INDEX *iptr);
#else
    inline void R_ProtectWithIndex(SEXP node, PROTECT_INDEX *iptr)
    {
	*iptr = CXXR::ProtectStack::protect(node);
    }
#endif

    /** @brief Retarget a cell in the C pointer protection stack.
     *
     * Change the node that a particular cell in the C pointer
     * protection stack protects.  As a consistency check, it is
     * required that the reprotect takes place within the same
     * ProtectStack::Scope as the original protect.
     *
     * @param node Pointer to the node now to be protected from
     *          the garbage collector by the designated stack
     *          cell.  (Not necessarily a different node from the
     *          one currently protected.)
     *
     * @param index Index (as returned by R_ProtectWithIndex() ) of
     *          the stack cell to be retargeted to node.  Must be less
     *          than the current size of the C pointer protection
     *          stack.
     */
#ifndef __cplusplus
    void R_Reprotect(SEXP node, PROTECT_INDEX index);
#else
    inline void R_Reprotect(SEXP node, PROTECT_INDEX index)
    {
	CXXR::ProtectStack::reprotect(node, index);
    }
#endif

    /** @brief Restore C pointer protection stack to a previous size.
     *
     * Restore the C pointer protection stack to a previous size by
     * popping elements off the top.
     *
     * @param new_size The size to which the stack is to be
     *          restored.  Must not be greater than the current
     *          size.
     *
     * @deprecated This is an interface for C code to call
     * CXXR::ProtectStack::restoreSize(), which may cease to be available
     * in future.  In C++, use of the ProtectStack::Scope class is
     * preferable.
     */
    void Rf_ppsRestoreSize(size_t new_size);
    
    /** @brief Current size of C pointer protection stack.
     *
     * @return the current size of the C pointer protection stack.
     *
     * @deprecated This is an interface for C code to call
     * CXXR::ProtectStack::size(), which may cease to be public in
     * future.
     */
    size_t Rf_ppsSize();

    /** @brief Push a node pointer onto the C pointer protection stack.
     *
     * @param node Pointer to the node to be protected from the
     *          garbage collector.
     * @return a copy of \a node .
     */
#ifndef __cplusplus
    SEXP Rf_protect(SEXP node);
#else
    inline SEXP Rf_protect(SEXP node)
    {
	CXXR::ProtectStack::protect(node);
	return node;
    }
#endif

    /** @brief Pop cells from the C pointer protection stack.
     *
     * As a consistency check, it is required that the unprotect takes
     * place within the same ProtectStack::Scope as the corresponding
     * protects.
     *
     * @param count Number of cells to be popped.  Must not be
     *          larger than the current size of the C pointer
     *          protection stack.
     */
#ifndef __cplusplus
    void Rf_unprotect(int count);
#else
    inline void Rf_unprotect(int count)
    {
	CXXR::ProtectStack::unprotect(static_cast<unsigned int>(count));
    }
#endif	

    /** @brief Remove entry from pointer protection stack.
     *
     * Removes from the C pointer protection stack the uppermost stack
     * cell containing a pointer to a specified node, and drops all
     * the stack cells above it by one place.
     *
     * @param node Pointer to the node whose cell is to be removed
     *          from the C pointer protection stack.
     *
     * @deprecated Utterly.
     */
#ifndef __cplusplus
    void Rf_unprotect_ptr(SEXP node);
#else
    inline void Rf_unprotect_ptr(SEXP node)
    {
	CXXR::ProtectStack::unprotectPtr(node);
    }
#endif

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  // PROTECTSTACK_H
