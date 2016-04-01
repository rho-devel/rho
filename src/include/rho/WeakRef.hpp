/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file WeakRef.h
 * @brief Class rho::WeakRef and associated C interface.
 */

#ifndef WEAKREF_HPP
#define WEAKREF_HPP

#include <list>
#include "rho/Allocator.hpp"
#include "rho/FunctionBase.hpp"
#include "rho/RObject.hpp"

namespace rho {
    /** @brief Weak reference.
     *
     * Refer to <em>Stretching the storage manager: weak pointers and
     * stable names in Haskell</em> by Peyton Jones, Marlow, and
     * Elliott (at <a
     * href="www.research.microsoft.com/Users/simonpj/papers/weak.ps.gz">www.research.microsoft.com/Users/simonpj/papers/weak.ps.gz</a>)
     * for the motivation and implementation of this class.
     *
     * Each WeakRef has a key and, optionally, a value and/or a
     * finalizer.  The finalizer may either be a C function or an R
     * object.  The mark-sweep garbage collector will consider the
     * value and finalizer to be reachable provided the key is
     * reachable.
     *
     * If, during a garbage collection, the key is found not to be
     * reachable then the finalizer (if any) will be run, and the weak
     * reference object will be 'tombstoned', so that subsequent calls
     * to key() and value() will return null pointers.
     *
     * A WeakRef object with a reachable key will not be garbage
     * collected even if the WeakRef object is not itself reachable.
     *
     * @note A WeakRef object takes steps to ensure that the reference
     * counts of itself and its key, value and R finalizer (if they
     * exist) never fall to zero until the WeakRef is tombstoned.
     * Consequently these objects will only be garbage collected as
     * part of a mark-sweep collection.  In particular, it can be
     * guaranteed that the finalizer of a WeakRef will be run as part
     * of the same mark-sweep collection in which the key of that
     * WeakRef is garbage-collected (having been found to be
     * unreachable).
     *
     * @todo It would probably make more sense for this class to
     * inherit directly from GCNode, and for the key, value etc. to be
     * pointers to GCNode.
     */
    class WeakRef : public RObject {
    public:
	/**
	 * @param key Pointer to the key of the WeakRef.  It is not
	 * forbidden but probably pointless for the key to be null: in
	 * this event the reference will immediately be tombstoned,
	 * and its finalizer (if any) will never be run.
	 *
	 * @param value Pointer to the value of the WeakRef (may be
	 *          null)
	 *
	 * @param R_finalizer Pointer to an R object to be evaluated
	 *          as a finalizer (may be null).  The finalizer will
	 *          be called with the key of the WeakRef object as
	 *          its argument, and at the time of call the key and
	 *          finalizer will be protected from the garbage
	 *          collector.  However, the WeakRef object itself
	 *          will already have been tombstoned.
	 *
	 * @param finalize_on_exit True iff the finalizer should be
	 *          run when rho exits.
	 */
	WeakRef(RObject* key, RObject* value, FunctionBase* R_finalizer = nullptr,
		bool finalize_on_exit = false);

	/**
	 * @param key Pointer to the key of the WeakRef.  It is
	 *          not forbidden but probably pointless for the key
	 *          to be null: in this event the reference will
	 *          immediately be tombstoned, and its finalizer (if
	 *          any) will never be run.
	 *
	 * @param value Pointer to the value of the WeakRef
	 *          (may be null).  The finalizer will be called with
	 *          a pointer to the key of the WeakRef object
	 *          as its argument, and at the time of call the key
	 *          object will be protected from the garbage
	 *          collector.  However, the WeakRef object
	 *          itself will already have been tombstoned.
	 *
	 * @param C_finalizer Pointer to an C function to be invoked
	 *          as a finalizer (may be null).
	 *
	 * @param finalize_on_exit True iff the finalizer should be
	 *          run when rho exits.
	 */
	WeakRef(RObject* key, RObject* value, R_CFinalizer_t C_finalizer,
		bool finalize_on_exit = false);

	~WeakRef();

	/** @brief Integrity check.
	 *
	 * Aborts the program with an error message if the class is
	 * found to be internally inconsistent.
	 *
	 * @return true, if it returns at all.  The return value is to
	 * facilitate use with \c assert.
	 */
	static bool check();

	/**
	 * @return Pointer to the key of the WeakRef.
	 */
	RObject* key() const {return m_key;}

	/** @brief Run finalizers with 'finalize_on_exit' specified.
	 *
	 * Run the finalizers of all (non-tombstoned) WeakRef object
	 * for which 'finalize_on_exit' was specified.
	 */
	static void runExitFinalizers();

	/** @brief Run finalizers.
	 *
	 * This is called by GCManager::gc() immediately after garbage
	 * collection, and runs the finalizers of any weak references that
	 * were identified during the garbage collection as being
	 * ready to finalize; when the call exits, all such weak
	 * references will have been tombstoned.  (Consequently,
	 * calling this method at any other time will effectively be a
	 * no-op.)
	 *
	 * @return true iff any finalizers are actually run (whether
	 * successfully or not).
	 */
	static bool runFinalizers();

	/**
	 * @return Pointer to the value of the WeakRef.
	 */
	RObject* value() const {return m_value;}

	// Virtual functions of RObject:
	unsigned int packGPBits() const override;
	void unpackGPBits(unsigned int gpbits) override;
    protected:
	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	typedef std::list<WeakRef*, Allocator<WeakRef*> > WRList;
	static WRList* getLive();
	static WRList* getFinalizationPending();
	static WRList* getTombstone();

	static int s_count;  // Count of references in existence (for
			     // debugging)

	GCEdge<> m_key;
	GCEdge<> m_value;
	GCEdge<FunctionBase> m_Rfinalizer;
	GCEdge<> m_self;  // Each WeakRef refers to itself, to stop
			  // WeakRef nodes being deleted by gclite().
	R_CFinalizer_t m_Cfinalizer;
	WRList::iterator m_lit;
	bool m_ready_to_finalize;
	bool m_finalize_on_exit;

	void finalize();

	/** Mark nodes reachable via weak references.
	 *
	 * This function implements the algorithm in Sec. 6.2 of the
	 * Peyton-Jones et al. paper.  If a WeakRef has a marked key,
	 * its value and R finalizer and their descendants are marked.
	 * If the key is not marked, and there is a finalizer, then
	 * the WeakRef is placed on a finalization pending list.  If
	 * the key is not marked and there is no finalizer, the
	 * WeakRef is tombstoned.
	 */
	static void markThru();

	// Tombstone the node:
	void tombstone();

	// Transfer the WeakRef from list 'from' to list 'to':
	void transfer(WRList* from, WRList* to)
	{
	    to->splice(to->end(), *from, m_lit);
	}

	// Return pointer to the list (live, finalization_pending or
	// tombstone) on which - according to its internal data -
	// the object currently should be listed (and quite possibly
	// is listed).
	WRList* wrList() const;

	friend class GCNode;
    };
}  // namespace rho

#endif /* WEAKREF_HPP */
