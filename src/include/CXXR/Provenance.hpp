/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) Chris A. Silles 2009-12.
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

/** @file Provenance.hpp
 *
 * @brief Class Provenance.
 */

#ifndef PROVENANCE_HPP
#define PROVENANCE_HPP 1

#include <sys/time.h>
#include <ctime>
#include <set>

#include "CXXR/CommandChronicle.hpp"
#include "CXXR/GCEdge.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/RObject.h"
#include "CXXR/Symbol.h"

namespace CXXR {
    /** @brief Provenance of a Frame::Binding state.
     *
     * This class records the provenance of an R binding, or more
     * accurately the provenance of the state of an R binding at a
     * particular time, where the state of a Frame::Binding in
     * particular includes its value.  However, for brevity, in the
     * documentation of this class we will use the expression "this
     * binding" to refer to the Frame::Binding object, <em>and the
     * state of that Frame::Binding object,</em> to which this
     * Provenance object relates.  (It should be understood that this
     * Frame::Binding object may subsequently have changed its state,
     * or ceased to exist altogether.)
     *
     * In particular a Provenance object records the time this binding
     * was established, and the top-level command that gave rise to
     * it.  It also maintains links to information about the
     * provenance of the parents and children of the binding.  The
     * parents of a binding state <em>b</em> are the binding states
     * which were read by the top-level command before creating the
     * binding state <em>b</em>, and may therefore have influenced the
     * state <em>b</em>; children are defined analogously.
     *
     * @par Xenogenesis:
     * Normally the value (and more generally) the state of a binding
     * created during the evaluation of a top-level command will
     * depend solely on the values of bindings previously read during
     * the evaluation of that top-level command, and possibly on other
     * aspects of the internal state of the R interpreter when
     * evaluation of the command started.  If this is \e not the case,
     * then the binding (state) is described as <b>xenogenous</b>, and
     * this property is recorded in the Provenance object.  Moreover,
     * if a binding is xenogenous, the \e value of the binding is also
     * recorded within the Provenance object.
     */
    class Provenance : public GCNode {
    public:
	/** @brief For sorting Provenance objects by timestamp.
	 *
	 * This is a function object class for comparing two Provenance
	 * objects according to their timestamps.
	 */
	class CompTime {
	public:
	    /** @brief Comparison operator
	     *
	     * @param lhs Non-null pointer to Provenance object.
	     *
	     * @param rhs Non-null pointer to Provenance object.
	     *
	     * @return true iff the timestamp of \a lhs is earlier
	     * than the timestamp of \a rhs.
	     */
	    bool operator()(const Provenance* lhs, const Provenance* rhs) const
	    {
		return (lhs->m_timestamp.tv_sec == rhs->m_timestamp.tv_sec) ?
		    (lhs->m_timestamp.tv_usec < rhs->m_timestamp.tv_usec) :
		    (lhs->m_timestamp.tv_sec < rhs->m_timestamp.tv_sec);
	    }
	};

	/** @brief Set of <tt>const Provenance*</tt> sorted by
	 *  timestamp.
	 */
	typedef std::set<const Provenance*, Provenance::CompTime> Set;

	/** @brief Constructor.
	 *
	 * @param sym Pointer to the Symbol bound by this binding.
	 *
	 * @param chron Pointer to the CommandChronicle for the
	 *          command whose evaluation gave rise to this
	 *          binding.
	 */
	Provenance(const Symbol* sym, const CommandChronicle* chron);

	~Provenance()
	{
	    announceDeath(); // Necessary house-keeping
	}

	/** @brief Ancestors of a set of Provenance objects.
	 *
	 * @param roots An arbitrary Provenance::Set.
	 *
	 * @return Pointer to a Provenance::Set which is the closure
	 * of \a roots under the 'parent' relationship.  This return
	 * value is allocated from the free store, and the calling
	 * code is responsible for deleting it in due course.
	 */
	static Set* ancestors(const Set& roots);

	/** @brief Children of this Provenance object.
	 *
	 * @return The set of Provenance objects relating to the children of
	 * this binding.
	 */
	const Set& children() const
	{
	    return m_children;
	}

	/** @brief CommandChronicle for this Provenance object.
	 *
	 * @return pointer to the CommandChronicle for the top-level
	 * command that gave rise to this binding.
	 */
	const CommandChronicle* chronicle() const
	{
	    return m_chronicle;
	}

	/** @brief Top-level command giving rise to this binding.
	 *
	 * @return pointer to the top-level command which was being
	 * evaluated when this binding was established.
	 */
	const RObject* command() const
	{
	    return m_chronicle->command();
	}

	/** @brief Descendants of a set of Provenance objects.
	 *
	 * @param roots An arbitrary Provenance::Set.
	 *
	 * @return Pointer to a Provenance::Set which is the closure
	 * of \a roots under the 'child' relationship.  This return
	 * value is allocated from the free store, and the calling
	 * code is responsible for deleting it in due course.
	 */
	static Set* descendants(const Set& roots);

	/** @brief Formatted timestamp as a CXXR::String.
	 *
	 * @return pointer to a CXXR::String representing the
	 * timestamp.  The formatting of this string is currently
	 * locale-dependent.
	 *
	 * @deprecated This should instead use R's native date-time
	 * classes.
	 */
	const String* getTime() const;

	/** @brief Parents of this Provenance object.
	 *
	 * @return Iterator range comprising all parents of this
	 * binding.
	 */
	std::pair<CommandChronicle::ParentVector::const_iterator,
		  CommandChronicle::ParentVector::const_iterator>
	parents() const;

	/** @brief Symbol bound by this binding.
	 *
	 * @return pointer to this Symbol bound by this binding.
	 */
	const Symbol* symbol() const
	{
	    return m_symbol;
	}

	/** @brief Value of xenogenous binding.
	 *
	 * @return if this binding is xenogenous, the stored value of
	 * the binding.  Otherwise returns a null pointer.
	 */
	const RObject* value() const
	{
	    return m_value;
	}

	/** @brief Is this binding xenogenous?
	 *
	 * @return true iff this binding is xenogenous.
	 */
	bool isXenogenous() const
	{
	    return m_xenogenous;
	}

	/** @brief Return serial number.
	 *
	 * During each session, each Provenance object created is
	 * given a unique serial number.  This serial number is not
	 * preserved across sessions.
	 *
	 * @return This Provenance object's serial number.
	 */
	unsigned int serialNumber() const
	{
	    return m_serial;
	}

	/** @brief Declare binding to be xenogenous.
	 *
	 * By default, a Provenance is assumed to be associated with a
	 * non-xenogenous binding.  This assumption can be overridden
	 * by calling this function.
	 *
	 * @param value The value of the xenogenous binding, which (as
	 *          a result of this call) is recorded inside the
	 *          Provenance object.
	 */
	void setXenogenous(const RObject* value);

	/** @brief Timestamp of this binding.
	 *
	 * @return the timestamp associated with this binding,
	 * expressed as the number of seconds since the Unix epoch
	 * (1970-01-01 00:00:00 UTC).
	 */
	double timestamp() const;

	// Virtual functions of GCNode:
	void detachReferents() override;
	void visitReferents(const_visitor*) const override;
    private:
	static unsigned int s_next_serial;

	mutable Set m_children;
	struct timeval m_timestamp;

	unsigned int m_serial;  // Within a session each Provenance
	  // object is given a unique serial number.  This serial
	  // number is not preserved during serialisation.

	size_t m_num_parents;
	GCEdge<const Symbol> m_symbol;
	GCEdge<const CommandChronicle> m_chronicle;
	GCEdge<const RObject> m_value;
	bool m_xenogenous;

	// Do away with compiler-generated copy constructor
	Provenance(const Provenance&);

	void announceBirth();

	void announceDeath();

	void deregisterChild(const Provenance* child) const
	{
	    m_children.erase(child);
	}

	void registerChild(const Provenance* child) const
	{
	    m_children.insert(child);
	}
    };
} // namespace CXXR

#endif
