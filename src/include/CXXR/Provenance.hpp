/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/* This file incorporates material Copyright (C) Chris A. Silles 2009-12.
 */

/*
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
#include <boost/serialization/access.hpp>
#include <boost/serialization/base_object.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/split_member.hpp>

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
	void detachReferents();
	void visitReferents(const_visitor*) const;
    private:
	friend class boost::serialization::access;

	static unsigned int s_next_serial;

	mutable Set m_children;
	struct timeval m_timestamp;

	unsigned int m_serial;  // Within a session each Provenance
	  // object is given a unique serial number.  This serial
	  // number is not preserved during serialisation.

	unsigned int m_num_parents;
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

	template <class Archive>
	void load(Archive& ar, const unsigned int version);
		
	template <class Archive>
	void save(Archive& ar, const unsigned int version) const;

	template <class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    boost::serialization::split_member(ar, *this, version);
	}
    };
} // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::Provenance)

namespace boost {
    namespace serialization {
	/** @brief Template specialisation.
	 *
	 * This specialisation is required because CXXR::Provenance
	 * does not have a default constructor.  See the
	 * boost::serialization documentation for further details.
	 *
	 * @tparam Archive archive class from which deserialisation is
	 *           taking place.
	 *
	 * @param ar Archive from which deserialisation is taking
	 *           place.
         *
	 * @param chron Pointer to the location at which a
	 *          CXXR::CommandChronicle object is to be constructed.
	 *
	 * @param version Ignored.
	 */
	template<class Archive>
	void load_construct_data(Archive& ar, CXXR::Provenance* prov,
				 const unsigned int version)
	{
	    using namespace CXXR;
	    GCStackRoot<const Symbol> symbol;
	    GCNPTR_SERIALIZE(ar, symbol);
	    GCStackRoot<const CommandChronicle> chronicle;
	    GCNPTR_SERIALIZE(ar, chronicle);
	    new (prov) Provenance(symbol, chronicle);
	}

	/** @brief Template specialisation.
	 *
	 * This specialisation is required to ensure that the symbol
	 * and chronicle of a CXXR::Provenance object are serialised
	 * within an archive before the Provenance object itself is
	 * serialised, so that on deserialisation the symbol and
	 * chronicle can be made available to load_construct_data().
	 * See the boost::serialization documentation for further
	 * details.
	 *
	 * @tparam Archive archive class to which serialisation is
	 *           taking place.
	 *
	 * @param ar Archive to which serialisation is taking place.
         *
	 * @param chron Non-null pointer to the CXXR::Provenance
	 *          object about to be serialised.
	 *
	 * @param version Ignored.
	 */
	template<class Archive>
	void save_construct_data(Archive& ar, const CXXR::Provenance* prov,
				 const unsigned int version)
	{
	    using namespace CXXR;
	    const Symbol* symbol = prov->symbol();
	    GCNPTR_SERIALIZE(ar, symbol);
	    const CommandChronicle* chronicle = prov->chronicle();
	    GCNPTR_SERIALIZE(ar, chronicle);
	}
    }  // namespace serialization
}  // namespace boost

// ***** Implementation of non-inlined templated members *****

template <class Archive>
void CXXR::Provenance::load(Archive& ar, const unsigned int version)
{
    ar >> BOOST_SERIALIZATION_BASE_OBJECT_NVP(GCNode);
    ar >> boost::serialization::make_nvp("sec", m_timestamp.tv_sec);
    ar >> boost::serialization::make_nvp("usec", m_timestamp.tv_usec);
    ar >> BOOST_SERIALIZATION_NVP(m_num_parents);
    GCNPTR_SERIALIZE(ar, m_value);
    ar >> BOOST_SERIALIZATION_NVP(m_xenogenous);

    announceBirth();
}
		
template <class Archive>
void CXXR::Provenance::save(Archive& ar, const unsigned int version) const
{
    ar << BOOST_SERIALIZATION_BASE_OBJECT_NVP(GCNode);
    ar << boost::serialization::make_nvp("sec", m_timestamp.tv_sec);
    ar << boost::serialization::make_nvp("usec", m_timestamp.tv_usec);
    ar << BOOST_SERIALIZATION_NVP(m_num_parents);
    GCNPTR_SERIALIZE(ar, m_value);
    ar << BOOST_SERIALIZATION_NVP(m_xenogenous);
}

#endif
