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

/** @file ProvenanceTracker.h
 *
 * Class CXXR::ProvenanceTracker.
 */

#ifndef PROVENANCETRACKER_H
#define PROVENANCETRACKER_H

#ifdef __cplusplus

#include "CXXR/Frame.hpp"

namespace CXXR {

#ifndef PROVENANCE_TRACKING
    // Vestigial implementation of ProvenanceTracker:
    struct ProvenanceTracker {
	static void flagXenogenesis()
	{}
    };
#else
    /** @brief Management of provenance tracking.
     *
     * This class, all of whose members are static, provides
     * high-level management of the provenance-tracking facilities.
     */
    class ProvenanceTracker {
    public:
	/** @brief Object associating provenance tracking with a
	 *  top-level command.
	 *
	 * An object of this type should be in existence during the
	 * period that the interpreter is evaluating a top-level
	 * command, and associates newly created bindings (i.e.\ new
	 * Frame::Binding objects, or new states of such objects)
	 * within provenance-tracked Frames with that top-level
	 * command.
	 *
	 * CommandScope objects are intended to be allocated on the
	 * processor stack.
	 */
	class CommandScope {
	public:
	    /** @brief Constructor.
	     *
	     * @param command Top-level command whose evaluation the
	     *          lifetime of this object will span.
	     *
	     * @note In the event that a CommandScope object is
	     * already in existence (i.e. the CommandScope being
	     * constructed is nested within another, possibly as a
	     * result of a browser call) then the provenance of
	     * bindings will continue to be ascribed to the command of
	     * the outermost CommandScope, so the parameter \a command
	     * is effectively ignored.
	     */
	    CommandScope(const RObject* command);

	    ~CommandScope();

	    /** @brief Flag up xenogenesis.
	     *
	     * This function is called by class ProvenanceTracker to
	     * indicate to the provenance tracker that in evaluating
	     * the top-level command, the behaviour of the interpreter
	     * has been influenced by something external to it,
	     * e.g. by reading an external file, or by accepting user
	     * input.  Provenance-tracked bindings created by the
	     * top-level command subsequently to this call will be
	     * flagged as having xenogenous provenance.
	     */
	    void flagXenogenesis()
	    {
		m_xenogenetic = true;
	    }

	    /** @brief Monitor reading of bindings.
	     *
	     * Class ProvenanceTracker will call this function
	     * whenever evaluation of the top-level command results in
	     * the reading of a binding within a provenance-tracked
	     * Frame.
	     *
	     * @param bdg reference to the binding read.
	     */
	    void monitorRead(const Frame::Binding& bdg);

	    /** @brief Monitor writing of bindings.
	     *
	     * Class ProvenanceTracker will call this function
	     * whenever evaluation of the top-level command results in
	     * the writing of a binding within a provenance-tracked
	     * Frame.
	     *
	     * @param bdg reference to the binding written.
	     */
	    void monitorWrite(const Frame::Binding& bdg);
	private:
	    GCStackRoot<CommandChronicle> m_chronicle;
	    bool m_xenogenetic;
	};

	/** @brief Flag up xenogenesis.
	 *
	 * This function is called to indicate to the provenance
	 * tracker that in evaluating the current top-level command
	 * (if any), the behaviour of the interpreter has been
	 * influenced by something external to it, e.g. by reading an
	 * external file, or by accepting user input.
	 * Provenance-tracked bindings created by the top-level
	 * command subsequently to this call will be flagged as having
	 * xenogenous provenance.
	 */
	static void flagXenogenesis();

	/** @brief Read monitor.
	 *
	 * This function is set up as the read monitor for class
	 * Frame, to ensure that reading of bindings within
	 * provenance-tracked Frames is reported to the provenance
	 * tracker.
	 */
	static void monitorRead(const Frame::Binding& bdg);

	/** @brief Write monitor.
	 *
	 * This function is set up as the write monitor for class
	 * Frame, to ensure that writing of bindings within
	 * provenance-tracked Frames is reported to the provenance
	 * tracker.
	 */
	static void monitorWrite(const Frame::Binding& bdg);

	/** @brief Establish read and write monitoring for provenance
	 *  tracking.
	 *
	 * This function sets up the read and write monitors of class
	 * Frame appropriately for provenance tracking, and enables
	 * the provenance tracking of bindings within the global
	 * environment.
	 */
	static void setMonitors();
    private:
	static CommandScope* s_scope;  // Pointer to the current scope
	  // or null if none.  If scopes are nested, this points to
	  // the outermost scope.

	// Declared private to prevent instantiation of this class:
	ProvenanceTracker();
    };
#endif  // PROVENANCE_TRACKING
} // namespace CXXR

extern "C" {
#endif // __cplusplus

    /** @brief Flag up xenogenesis.
     *
     * This function is called to indicate to the provenance tracker
     * that in evaluating the current top-level command, the behaviour
     * of the interpreter has been influenced by something external to
     * it, e.g. by reading an external file, or by accepting user
     * input.  Provenance-tracked bindings created by the top-level
     * command subsequently to this call will be flagged as having
     * xenogenous provenance.
     */
    void flagXenogenesis();

#ifdef __cplusplus
}  // extern "C"
#endif

#endif // PROVENANCETRACKER_H
