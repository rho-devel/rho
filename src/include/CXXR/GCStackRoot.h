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

/** @file GCStackRoot.h
 *
 * @brief Templated class CXXR::GCStackRoot and its untemplated base class
 * CXXR::GCStackRootBase.
 *
 * CXXR::GCStackRootBase also encapsulates the functionality of the CR
 * pointer protection stack.
 *
 * See the paragraph 'Caller Protects' in the description of class
 * CXXR::GCStackRoot for recommended coding policy.
 */

#ifndef GCSTACKROOT_H
#define GCSTACKROOT_H 1

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include <vector>
#include "CXXR/GCNode.hpp"

class RCNTXT;

namespace CXXR {
    class RObject;

    /** @brief Untemplated base class for GCStackRoot.
     *
     * The preferred method for C++ code to protect a GCNode
     * from the garbage collector is to use the templated class
     * GCStackRoot, of which this is the untemplated base class, or
     * class GCRoot.
     *
     * However, GCStackRoot is not usable by C code, which should continue
     * to use ::PROTECT(), ::UNPROTECT() etc. as in CR.
     * However, these functions have been reimplemented to manipulate
     * a C pointer protection stack (as we shall call it, despite the
     * fact that it's implemented in C++) encapsulated as a static
     * member within GCStackRootBase.
     */
    class GCStackRootBase {
    public:
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
	 * public, and be accessible only by a class encapsulating R
	 * contexts.
	 */
	static void ppsRestoreSize(size_t new_size);

	/** @brief Current size of PPS.
	 *
	 * @return the current size of the C pointer protection stack.
	 *
	 * @note This method is intended for use in conjunction with
	 * ppsRestoreSize(), and like it may cease to be public in
	 * future.
	 */
	static size_t ppsSize()
	{
	    return s_pps->size();
	}

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
#ifndef NDEBUG
	static unsigned int protect(RObject* node);
#else
	static unsigned int protect(RObject* node)
	{
	    GCNode::maybeCheckExposed(node);
	    unsigned int index = s_pps->size();
	    if (node)
		node->incRefCount();
	    s_pps->push_back(node);
	    return index;
	}
#endif

	/** @brief Change the target of a pointer on the PPS.
	 *
	 * Change the node that a particular cell in the C pointer
	 * protection stack protects.  As a consistency check, it is
	 * required that the reprotect takes place within the same
	 * ::RCNTXT as the corresponding protect.  (CR does not apply this
	 * check.)
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
	static void reprotect(RObject* node, unsigned int index);

	/** @brief Pop pointers from the PPS.
	 *
	 * Pop cells from the C pointer protection stack.  As a
	 * consistency check, it is required that the unprotect takes
	 * place within the same ::RCNTXT as the corresponding protect.
	 * (CR does not apply this check.)
	 *
	 * @param count Number of cells to be popped.  Must not be
	 *          larger than the current size of the C pointer
	 *          protection stack.
	 */
	static void unprotect(unsigned int count = 1);

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
	static void unprotectPtr(RObject* node);

	/** @brief Conduct a const visitor to all 'root' GCNode objects.
	 *
	 * Conduct a GCNode::const_visitor object to each root GCNode
	 * and each node on the C pointer protection stack.
	 *
	 * @param v Pointer to the const_visitor object.
	 */
	static void visitRoots(GCNode::const_visitor* v);
    protected:
	GCStackRootBase(const GCNode* node)
	    : m_next(s_roots), m_target(node)
	{
	    s_roots = this;
	    GCNode::maybeCheckExposed(node);
	    if (m_target)
		m_target->incRefCount();
	}

	GCStackRootBase(const GCStackRootBase& source)
	    : m_next(s_roots), m_target(source.m_target)
	{
	    s_roots = this;
	    if (m_target)
		m_target->incRefCount();
	}

	~GCStackRootBase()
	{
	    if (this != s_roots)
		seq_error();
	    if (m_target && m_target->decRefCount() == 0)
		m_target->makeMoribund();
	    s_roots = m_next;
	}

	GCStackRootBase& operator=(const GCStackRootBase& source)
	{
	    if (source.m_target)
		source.m_target->incRefCount();
	    if (m_target && m_target->decRefCount() == 0)
		m_target->makeMoribund();
	    m_target = source.m_target;
	    return *this;
	}

	void redirect(const GCNode* node)
	{
	    GCNode::maybeCheckExposed(node);
	    if (node)
		node->incRefCount();
	    if (m_target && m_target->decRefCount() == 0)
		m_target->makeMoribund();
	    m_target = node;
	}

	/** @brief Access the encapsulated pointer.
	 *
	 * @return the GCNode pointer encapsulated by this object.
	 */
	const GCNode* ptr() const
	{
	    return m_target;
	}
    private:
	friend class GCNode;

	// Ye olde pointer protection stack:
#ifdef NDEBUG
	static std::vector<RObject*>* s_pps;
#else
	static std::vector<std::pair<RObject*, RCNTXT*> >* s_pps;
#endif

	static GCStackRootBase* s_roots;

	GCStackRootBase* m_next;
	const GCNode* m_target;

	// Clean up static data at end of run (called by
	// GCNode::SchwarzCtr destructor:
	static void cleanup()
	{
	    delete s_pps;
	}

	// Initialize static data (called by GCNode::SchwarzCtr
	// constructor):
	static void initialize();

	// Report out-of-sequence destructor call and abort program.
	// (We can't use an exception here because it's called from a
	// destructor.)
	static void seq_error();
    };

    /** @brief Smart pointer to protect a GCNode from garbage
     * collection.
     *
     * This class encapsulates a pointer to an object of a type
     * derived from GCNode.  For as long as the GCStackRoot object
     * exists, the GCNode that it points to will not be garbage
     * collected.
     *
     * GCStackRoot objects are intended to be allocated on the
     * processor stack: specifically, the class implementation
     * requires that GCStackRoot objects are destroyed in the reverse
     * order of creation, and the destructor checks this.
     *
     * @param T GCNode or a type publicly derived from GCNode.  This
     *          may be qualified by const, so for example a const
     *          String* may be encapsulated in a GCStackRoot using the
     *          type GCStackRoot<const String>.
     *
     * \par Caller protects:
     * Suppose some code calls a function (or class method) that takes
     * a pointer or reference to a class derived from GCNode as an
     * argument, and/or returns a pointer to a class derived from
     * GCNode as its return value.  In CXXR, the preferred coding
     * approach is that the \e calling \e code should take
     * responsibility for protecting the arguments from the garbage
     * collector before calling the function, and likewise take
     * responsibility for protecting the returned value.  This is
     * because the calling code is in a better position to decide
     * whether any additional steps are necessary to achieve this, and
     * what they should be.  (The calling code may also need to protect
     * other objects: objects that are neither arguments to or values
     * returned from the called function, but which would otherwise be
     * vulnerable if the called function gave rise to a garbage
     * collection.)
     */
    template <class T = RObject>
    class GCStackRoot : public GCStackRootBase {
    public:
	/**
	 * @param node Pointer the node to be pointed to, and
	 *          protected from the garbage collector, or a null
	 *          pointer.
	 */
    explicit GCStackRoot(T* node = 0)
    : GCStackRootBase(node) {}

	/** @brief Copy constructor.
	 *
	 * The constructed GCStackRoot will protect the same GCNode as
	 * source.  (There is probably no reason to use this
	 * constructor.)
	 */
	GCStackRoot(const GCStackRoot& source) : GCStackRootBase(source) {}

	/**
	 * This will cause this GCStackRoot to protect the same GCNode as
	 * is protected by source.  (There is probably no reason to
	 * use this method.)
	 */
	GCStackRoot& operator=(const GCStackRoot& source)
	{
	    GCStackRootBase::operator=(source);
	    return *this;
	}

	/**
	 * This will cause this GCStackRoot to point to and protect node,
	 * instead of the node (if any) it currently points to and
	 * protects.
	 *
	 * @param node Pointer to the GCNode that is now to be pointed
	 *          to and protected from the garbage collector.
	 */
	GCStackRoot& operator=(T* node)
	{
	    GCStackRootBase::redirect(node);
	    return *this;
	}

	/** @brief Access member via encapsulated pointer.
	 *
	 * @return the pointer currently encapsulated by the node.
	 */
	T* operator->() const
	{
	    return get();
	}

	/** @brief Dereference the encapsulated pointer.
	 *
	 * @return a reference to the object pointed to by the
	 * encapsulated pointer.  The effect is undefined if this
	 * object encapsulates a null pointer.
	 */
	T& operator*() const
	{
	    return *get();
	}

	/** @brief Implicit conversion to encapsulated pointer type.
	 *
	 * @return the pointer currently encapsulated by the node.
	 * The pointer is of type \a T* const to prevent its use as
	 * an lvalue, the effect of which would probably not be what
	 * the programmer wanted.
	 */
	operator T*() const
	{
	    return get();
	}

	/** @brief Access the encapsulated pointer.
	 *
	 * @return the pointer currently encapsulated by the node.
	 */
	T* get() const
	{
	    return static_cast<T*>(const_cast<GCNode*>(ptr()));
	}
    };
}

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
	*iptr = CXXR::GCStackRootBase::protect(node);
    }
#endif

    /** @brief Retarget a cell in the C pointer protection stack.
     *
     * Change the node that a particular cell in the C pointer
     * protection stack protects.  As a consistency check, it is
     * required that the reprotect takes place within the same RCNTXT
     * as the original protect.  (CR does not apply this check.)
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
	CXXR::GCStackRootBase::reprotect(node, index);
    }
#endif

    /** @brief Check that C pointer protection stack has expected size.
     *
     * Check that the C pointer protection stack has the expected
     * size, and print a warning if not.
     *
     * @param op Operation being performed.
     *
     * @param save The expected size of the pointer protection stack.
     *
     * @todo A warning seems too mild a response in this eventuality.
     */
    void Rf_check_stack_balance(SEXP op, unsigned int save);

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
     * CXXR::GCStackRootBase::ppsRestoreSize(), which may cease to be
     * public in future.
     */
    void Rf_ppsRestoreSize(size_t new_size);
    
    /** @brief Current size of C pointer protection stack.
     *
     * @return the current size of the C pointer protection stack.
     *
     * @deprecated This is an interface for C code to call
     * CXXR::GCStackRootBase::ppsSize(), which may cease to be public in
     * future.
     */
    size_t Rf_ppsSize();

    /** @brief Push a node pointer onto the C pointer protection stack.
     *
     * @param node Pointer to the node to be protected from the
     *          garbage collector.
     * @return a copy of \a node .
     */
    SEXP Rf_protect(SEXP node);

    /** @brief Pop cells from the C pointer protection stack.
     *
     * As a consistency check, it is required that the unprotect takes
     * place within the same RCNTXT as the corresponding protect.  (CR
     * does not apply this check.)
     *
     * @param count Number of cells to be popped.  Must not be
     *          larger than the current size of the C pointer
     *          protection stack.
     */
    void Rf_unprotect(int count);

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
	CXXR::GCStackRootBase::unprotectPtr(node);
    }
#endif

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  // GCSTACKROOT_H
