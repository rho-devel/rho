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
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file GCManager.hpp
 * @brief Class CXXR::GCManager.
 *
 * @todo Reinstate garbage collection timing.
 * @todo Update DEBUG_GC_SUMMARY etc.
 */

#ifndef GCMANAGER_HPP
#define GCMANAGER_HPP

#include <cstddef>
#include <iosfwd>

namespace CXXR {
    /** @brief Class for managing garbage collection.
     * 
     * This class only has static members.  A mark-sweep garbage
     * collection can be initiated explicitly by calling
     * GCManager::gc().  Also, GCNode::operator new() will
     * automatically initiate a mark-sweep GC if the number of bytes
     * allocated via CXXR::MemoryBank exceeds a threshold level
     * supplied by GCManager::triggerLevel() This threshold value
     * varies during the run, subject to a minimum value specified in
     * the enableGC() method.
     */
    class GCManager {
    public:
	/** @brief Initiate a mark-sweep garbage collection.
	 *
	 * It is currently an error to initiate a mark-sweep garbage
	 * collection while a GCNode object is under construction.
	 */
	static void gc();

	/** @brief Maximum number of bytes used.
	 *
	 * @return the maximum number of bytes used (up to the time of
	 *         the most recent garbage collection.)
	 *
	 * @note In CXXR, the record of the maximum number of bytes
	 * used is reviewed (and updated if necessary) only at the
	 * start of a mark-sweep garbage collection, and so will almost
	 * certainly underestimate the true maximum.
	 */
	static size_t maxBytes() {return s_max_bytes;}

	/** @brief Maximum number of GCNode objects allocated.
	 * 
	 * @return the maximum number of GCNode objects allocated (up
	 * to the time of the most recent garbage collection.)
	 *
	 * @note This method is provided for compatibility with CR.
	 * The number of GCNode objects doesn't directly affect the
	 * operation of garbage collection in CXXR.
	 *
	 * @note In CXXR, the record of the maximum number of GCNode
	 * objects allocated is reviewed (and updated if necessary)
	 * only at the start of a mark-sweep garbage collection, and
	 * so will almost certainly underestimate the true maximum.
	 */
	static size_t maxNodes() {return s_max_nodes;}

	/** @brief Reset the tallies of the maximum numbers of bytes and
	 *  GCNode objects.
	 *
	 * This method resets the record of the maximum number of
	 * bytes allocated to the current number of bytes allocated,
	 * and similarly for the maximum number of GCNode objects.
	 */
	static void resetMaxTallies();

	/** @brief Enable mark-sweep garbage collection.
	 *
	 * No automatic mark-sweep garbage collection of GCNode
	 * objects will take place until this method has been called.
	 *
	 * @param initial_threshold  Initial value for the collection
	 *          threshold.  The threshold will never be made less
	 *          than this value during the run (or until the
	 *          threshold is changed by a subsequent call to
	 *          setGCThreshold() ).
	 */
	static void setGCThreshold(size_t initial_threshold);

	/** @brief Set/unset monitors on mark-sweep garbage collection.
	 *
	 * @param pre_gc If not a null pointer, this function will be
	 *          called just before garbage collection begins,
	 *          e.g. to carry out timing.  It must not itself give
	 *          rise to a garbage collection.
	 *
	 * @param post_gc If not a null pointer, this function will be
	 *          called just after garbage collection is completed.
	 *          It  must not itself give rise to a garbage
	 *          collection.
	 */
	static void setMonitors(void (*pre_gc)() = 0,
				void (*post_gc)() = 0)
	{
	    s_pre_gc = pre_gc;
	    s_post_gc = post_gc;
	}

	/** @brief Set the output stream for garbage collection reporting.
	 *
	 * @param os Pointer to the output stream to which reporting
	 *          should be directed.  If NULL, suppresses reporting.
	 *
	 * @return The previous value of the output stream pointer.
	 */
	static std::ostream* setReporting(std::ostream* os = 0);

	/** @brief Turn garbage collection torture on or off.
	 *
	 * @param on The required torturing status.
	 *
	 * @note GC torture is no longer implemented in CXXR, so this
	 * function is a no-op.
	 */
	static void torture(bool on) {}

	/** @brief Current threshold level for mark-sweep garbage
	 * collection.
	 *
	 * @return The current threshold level.  When GCNode::operator
	 * new is on the point of requesting memory from MemoryBank,
	 * if it finds that the number of bytes already allocated via
	 * MemoryBank is at least as great as this threshold level, it
	 * may initiate a mark-sweep garbage collection.
	 */
	static size_t triggerLevel() {return s_threshold;}
    private:
	static size_t s_threshold;
	static size_t s_min_threshold;

	static size_t s_max_bytes;
	static size_t s_max_nodes;

	static std::ostream* s_os;  // Pointer to output stream for GC
				    // reporting, or NULL.

	// Callbacks e.g. for timing:
	static void (*s_pre_gc)();
	static void (*s_post_gc)();

	// Detailed control of the garbage collection is carried out
	// here.
	static void gcController();

	// Initialize static data associated with garbage collection.
	friend void initializeMemorySubsystem();
	static void initialize();

	GCManager() = delete;
    };
}  // namespace CXXR

#endif /* GCMANAGER_HPP */
