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

/** @file GCRoot.h
 *
 * @brief Templated class CXXR::GCRoot and its untemplated base class
 * CXXR::GCRootBase.
 *
 * GCRootBase also encapsulates the functionality of the CR pointer
 * protection stack.
 */

#ifndef GCROOT_H
#define GCROOT_H 1

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include <vector>
#include "RCNTXT.h"
#include "CXXR/GCNode.hpp"

namespace CXXR {
    class RObject;

    /** @brief Untemplated base class for GCRoot.
     *
     * The preferred method for C++ code to protect a GCNode
     * from the garbage collector is to use the templated class
     * GCRoot, of which this is the untemplated base class.
     *
     * However, GCRoot is not usable by C code, which should continue
     * to use PROTECT(), UNPROTECT() etc. as in CR.
     * However, these functions have been reimplemented to manipulate
     * a C pointer protection stack (as we shall call it, despite the
     * fact that it's implemented in C++) encapsulated as a static
     * member within GCRootBase.
     */
    class GCRootBase {
    public:
	explicit GCRootBase(GCNode* node)
	    : m_index(s_roots.size())
	{
	    if (node) node->expose();
	    s_roots.push_back(node);
	}

	GCRootBase(const GCRootBase& source)
	    : m_index(s_roots.size())
	{
	    s_roots.push_back(s_roots[source.m_index]);
	}

	~GCRootBase()
	{
	    s_roots.pop_back();
	    if (m_index != s_roots.size())
		seq_error();
	}

	GCRootBase& operator=(const GCRootBase& source)
	{
	    s_roots[m_index] = s_roots[source.m_index];
	    return *this;
	}

	GCRootBase& operator=(GCNode* node)
	{
	    if (node) node->expose();
	    s_roots[m_index] = node;
	    return *this;
	}

	/** @brief Restore PPS to a previous size.
	 *
	 * Restore the C pointer protection stack to a previous size by
	 * popping elements off the top.
	 * @param new_size The size to which the stack is to be
	 *          restored.  Must not be greater than the current
	 *          size.
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
	    return s_pps.size();
	}

	/** @brief Push a node pointer onto the PPS.
	 *
	 * Push a node pointer onto the C pointer protection stack.
	 * @param node Pointer to the node to be protected from the
	 *          garbage collector.
	 * @return Index of the stack cell thus created, for
	 *          subsequent use with reprotect().
	 */
	static unsigned int protect(RObject* node)
	{
	    unsigned int index = s_pps.size();
	    if (node) node->expose();
#ifdef NDEBUG
	    s_pps.push_back(node);
#else
	    s_pps.push_back(std::make_pair(node, R_GlobalContext));
#endif
	    return index;
	}

	/** @brief Access the encapsulated pointer.
	 *
	 * @return the GCNode pointer encapsulated by this object.
	 */
	GCNode* ptr() const
	{
	    return s_roots[m_index];
	}

	/** @brief Change the target of a pointer on the PPS.
	 *
	 * Change the node that a particular cell in the C pointer
	 * protection stack protects.  As a consistency check, it is
	 * required that the reprotect takes place within the same
	 * ::RCNTXT as the corresponding protect.  (CR does not apply this
	 * check.)
	 * @param node Pointer to the node now to be protected from
	 *          the garbage collector by the designated stack
	 *          cell.  (Not necessarily a different node from the
	 *          one currently protected.)
	 * @param index Index (as returned by protect() ) of the stack
	 *          cell to be retargeted to node.  Must be less than
	 *          the current size of the C pointer protection
	 *          stack.
	 */
	static void reprotect(RObject* node, unsigned int index);

	/** @brief Pop pointers from the PPS.
	 *
	 * Pop cells from the C pointer protection stack.  As a
	 * consistency check, it is required that the unprotect takes
	 * place within the same ::RCNTXT as the corresponding protect.
	 * (CR does not apply this check.)
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

	/** @brief Conduct a visitor to all 'root' GCNode objects.
	 *
	 * Conduct a GCNode::visitor object to each root GCNode
	 * and each node on the C pointer protection stack.
	 *
	 * @param v Pointer to the visitor object.
	 */
	static void visitRoots(GCNode::visitor* v);
    private:
	// There may be a case, at least in some C++ library
	// implementations, for using a deque instead of a vector in
	// the following, so that memory is released as the stack
	// shrinks.
	static std::vector<GCNode*> s_roots;

	// Ye older pointer protection stack:
#ifdef NDEBUG
	static std::vector<RObject*> s_pps;
#else
	static std::vector<std::pair<RObject*, RCNTXT*> > s_pps;
#endif

	unsigned int m_index;

	static void seq_error();
    };

    /** @brief Smart pointer to protect a GCNode from garbage
     * collection.
     *
     * This class encapsulates a pointer to an object of a type
     * derived from GCNode.  For as long as the GCRoot object exists,
     * the GCNode that it points to will not be garbage collected.
     *
     * GCRoot objects are intended to be allocated on the stack, or at
     * file or global scope: the class implementation requires that
     * GCRoot objects are destroyed in the reverse order of creation,
     * and the destructor checks this.
     *
     * @param Ptr A pointer to GCNode or a type publicly derived from
     *          GCNode.  There is at present no provision for const
     *          pointers to be encapsulated within a GCRoot.
     */
    template <typename Ptr = RObject*>
    class GCRoot : public GCRootBase {
    public:
	/**
	 * @param node Pointer the node to be pointed to, and
	 *          protected from the garbage collector, or a null
	 *          pointer.
	 */
	explicit GCRoot(Ptr node = 0) : GCRootBase(node) {}

	/** Copy constructor.
	 *
	 * The constructed GCRoot will protect the same GCNode as
	 * source.  (There is probably no reason to use this
	 * constructor.)
	 */
	GCRoot(const GCRoot& source) : GCRootBase(source) {}

	/** Upcast constructor
	 *
	 * This constructor enables a GCRoot<Derived*> to be
	 * implicitly converted to a GCRoot<Base*>.
	 */
	template <class U> GCRoot(const GCRoot<U>& source)
	    : GCRootBase(Ptr(source))
	{}

	/**
	 * This will cause this GCRoot to protect the same GCNode as
	 * is protected by source.  (There is probably no reason to
	 * use this method.)
	 */
	GCRoot operator=(const GCRoot& source)
	{
	    GCRootBase::operator=(source);
	    return *this;
	}

	/**
	 * This will cause this GCRoot to point to and protect node,
	 * instead of the node (if any) it currently points to and
	 * protects.
	 *
	 * @param node Pointer to the GCNode that is now to be pointed
	 *          to and protected from the garbage collector.
	 */
	GCRoot operator=(Ptr node)
	{
	    GCRootBase::operator=(node);
	    return *this;
	}

	/** @brief Access member via encapsulated pointer.
	 *
	 * @return the pointer currently encapsulated by the node.
	 */
	Ptr const operator->() const
	{
	    return static_cast<Ptr>(ptr());
	}

	/**
	 * @return the pointer currently encapsulated by the node.
	 * The pointer is of type Ptr const to prevent its use as an
	 * lvalue, the effect of which would probably not be what the
	 * programmer wanted.
	 */
	operator Ptr const() const
	{
	    return static_cast<Ptr>(ptr());
	}
    };
}

extern "C" {
#endif /* __cplusplus */

    /* ***** C interface ***** */

    typedef unsigned int PROTECT_INDEX;

    /**
     * Push a node pointer onto the C pointer protection stack, and
     * record the index of the resulting stack cell (for subsequent
     * use with R_Reprotect).
     * @param node Pointer to the node to be protected from the
     *          garbage collector.
     * @param iptr Pointer to a location in which the stack cell index
     *          is to be stored.
     */
#ifndef __cplusplus
    void R_ProtectWithIndex(SEXP node, PROTECT_INDEX *iptr);
#else
    inline void R_ProtectWithIndex(SEXP node, PROTECT_INDEX *iptr)
    {
	*iptr = CXXR::GCRootBase::protect(node);
    }
#endif

    /**
     * Change the node that a particular cell in the C pointer
     * protection stack protects.  As a consistency check, it is
     * required that the reprotect takes place within the same
     * RCNTXT as the original protect.  (CR does not apply this
     * check.)
     * @param node Pointer to the node now to be protected from
     *          the garbage collector by the designated stack
     *          cell.  (Not necessarily a different node from the
     *          one currently protected.)
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
	CXXR::GCRootBase::reprotect(node, index);
    }
#endif

    /**
     * Check that the C pointer protection stack has the expected size,
     * and print a warning if not.
     * @param op Operation being performed.
     * @param save The expected size of the pointer protection stack.
     *
     * @todo A warning seems too mild a response in this eventuality.
     */
    void Rf_check_stack_balance(SEXP op, unsigned int save);

    /**
     * Restore the C pointer protection stack to a previous size by
     * popping elements off the top.
     * @param new_size The size to which the stack is to be
     *          restored.  Must not be greater than the current
     *          size.
     * @deprecated This is an interface for C code to call
     * GCRootBase::ppsRestoreSize(), which may cease to be public in
     * future.
     */
    void Rf_ppsRestoreSize(size_t new_size);
    
    /**
     * @return the current size of the C pointer protection stack.
     *
     * @deprecated This is an interface for C code to call
     * GCRootBase::ppsSize(), which may cease to be public in
     * future.
     */
    size_t Rf_ppsSize();

    /**
     * Push a node pointer onto the C pointer protection stack.
     * @param node Pointer to the node to be protected from the
     *          garbage collector.
     * @return a copy of \a node .
     */
#ifndef __cplusplus
    SEXP Rf_protect(SEXP node);
#else
    inline SEXP Rf_protect(SEXP node)
    {
	CXXR::GCRootBase::protect(node);
	return node;
    }
#endif

    /**
     * Pop cells from the C pointer protection stack.  As a
     * consistency check, it is required that the unprotect takes
     * place within the same RCNTXT as the corresponding protect.  (CR
     * does not apply this check.)
     * @param count Number of cells to be popped.  Must not be
     *          larger than the current size of the C pointer
     *          protection stack.
     */
#ifndef __cplusplus
    void Rf_unprotect(int count);
#else
    inline void Rf_unprotect(int count)
    {
	CXXR::GCRootBase::unprotect(count);
    }
#endif

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
#ifndef __cplusplus
    void Rf_unprotect_ptr(SEXP node);
#else
    inline void Rf_unprotect_ptr(SEXP node)
    {
	CXXR::GCRootBase::unprotectPtr(node);
    }
#endif

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  // GCROOT_H
