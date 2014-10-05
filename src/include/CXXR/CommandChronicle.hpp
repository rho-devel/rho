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

/** @file CommandChronicle.hpp
 *
 * @brief Class CommandChronicle.
 */

#ifndef COMMANDCHRONICLE_HPP
#define COMMANDCHRONICLE_HPP 1

#include <set>
#include <vector>
#include <boost/serialization/access.hpp>
#include <boost/serialization/nvp.hpp>

#include "CXXR/GCEdge.hpp"
#include "CXXR/GCStackRoot.hpp"
#include "CXXR/RObject.h"

namespace CXXR {
    class Provenance;

    /** @brief Record of bindings read by top-level command.
     *
     * This class maintains a record of the provenances of any
     * provenance-tracked bindings read in the course of evaluating a
     * top-level command.  The record includes only binding states
     * which were in existence before evaluation of the top-level
     * command started: i.e. the record ignores binding states which
     * are created and subsequently read back in the course of
     * evaluating the top-level command.
     *
     * The record is in the form of a vector, ordered according to the
     * time when a particular binding state is \e first read during
     * the evaluation of the top-level command.
     */
    class CommandChronicle : public GCNode {
    public:
	/** @brief Vector type used to record bindings read.
	 *
	 * This is the type of vector used to record the provenances
	 * of the binding states read during the evaluation of a
	 * top-level command.
	 */
	typedef std::vector<GCEdge<const Provenance> > ParentVector;

	/** @brief Constructor.
	 *
	 * @param command_arg Pointer to the top-level command to
	 *          which this Chronicle relates.  This will usually,
	 *          but not necessarily, be an Expression.
	 */
	CommandChronicle(const RObject* command_arg)
	    : m_command(command_arg)
	{}

	/** @brief Vector of bindings read.
	 *
	 * @return a reference to the vector of the provenances of
	 * provenance-tracked binding states read during the
	 * evaluation of the top-level command.
	 */
	const ParentVector& bindingsRead() const
	{
	    return m_reads;
	}

	/** @brief Close the CommandChronicle to new entries.
	 *
	 * This function is to be called to signify that evaluation of
	 * the top-level command is complete, and that there will
	 * therefore be no further calls to readBinding() in respect
	 * of this object.  This is a cue to release housekeeping data
	 * structures.
	 *
	 * @note The class does not check that there are no further
	 * calls to readBinding() in respect of this object, but such
	 * calls would result in erroneous behaviour.
	 */
	void close()
	{
	    m_seen.clear();
	}

	/** @brief The top-level command.
	 *
	 * @return pointer to the top-level command to which this
	 * CommandChronicle object relates.
	 */
	const RObject* command() const
	{
	    return m_command;
	}

	/** @brief Report reading of a provenance-tracked binding.
	 *
	 * This function should be called whenever the top-level
	 * command reads a Frame::Binding with non-null Provenance.
	 *
	 * @param bdgprov Non-null pointer to the Provenance of a
	 *          Frame::Binding read by the top-level command.
	 */
	void readBinding(const Provenance* bdgprov);

	/** @brief Report writing of a provenance-tracked binding.
	 *
	 * This function should be called whenever the top-level
	 * command creates or changes the state of a Frame::Binding
	 * with non-null Provenance.
	 *
	 * @param bdgprov Non-null pointer to the Provenance of a
	 *          Frame::Binding created or modified by the
	 *          top-level command.
	 */
	void writeBinding(const Provenance* bdgprov);

	// Virtual functions of GCNode:
	void detachReferents() override;
	void visitReferents(const_visitor* v) const override;
    private:
	friend class boost::serialization::access;
	friend class Provenance;

	ParentVector m_reads;

	std::set<unsigned int> m_seen;  // Set of serial numbers of
	  // all Provenance objects associated with bindings so far
	  // read or written during the evaluation of the top-level
	  // command.

	GCEdge<const RObject> m_command;

	// Although it is possible that a CommandChronicle object may
	// not have been closed by the time it is serialised, it is
	// assumed that it *will* have been closed before any
	// subsequent deserialisation of it takes place.  Consequently
	// the m_seen set is not itself serialised.
	template<class Archive>
	void serialize(Archive & ar, const unsigned int version) {
	    using namespace boost::serialization;
	    ar & BOOST_SERIALIZATION_BASE_OBJECT_NVP(GCNode);
	    size_t sz = m_reads.size();
	    ar & boost::serialization::make_nvp("size", sz);
	    m_reads.resize(sz);
	    for (size_t i = 0; i < sz; ++i) {
		GCEdge<const Provenance>& parent = m_reads[i];
		GCNPTR_SERIALIZE(ar, parent);
	    }
	}
    };
}  // namespace CXXR

BOOST_CLASS_EXPORT_KEY(CXXR::CommandChronicle)

namespace boost {
    namespace serialization {
	/** @brief Template specialisation.
	 *
	 * This specialisation is required because
	 * CXXR::CommandChronicle does not have a default constructor.
	 * See the boost::serialization documentation for further
	 * details.
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
	void load_construct_data(Archive& ar, CXXR::CommandChronicle* chron,
				 const unsigned int version)
	{
	    using namespace CXXR;
	    GCStackRoot<> command;
	    GCNPTR_SERIALIZE(ar, command);
	    new (chron) CommandChronicle(command);
	}

	/** @brief Template specialisation.
	 *
	 * This specialisation is required to ensure that the command
	 * of a CXXR::CommandChronicle is serialised within an archive
	 * before the CommandChronicle itself is serialised, so that
	 * on deserialisation this command can be made available to
	 * load_construct_data().  See the boost::serialization
	 * documentation for further details.
	 *
	 * @tparam Archive archive class to which serialisation is
	 *           taking place.
	 *
	 * @param ar Archive to which serialisation is taking place.
         *
	 * @param chron Non-null pointer to the CXXR::CommandChronicle
	 *          object about to be serialised.
	 *
	 * @param version Ignored.
	 */
	template<class Archive>
	void save_construct_data(Archive& ar,
				 const CXXR::CommandChronicle* chron,
				 const unsigned int version)
	{
	    using namespace CXXR;
	    const RObject* command = chron->command();
	    GCNPTR_SERIALIZE(ar, command);
	}
    }  // namespace serialization
}  // namespace boost

#endif // COMMANDCHRONICLE_HPP
