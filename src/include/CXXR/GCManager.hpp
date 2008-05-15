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
     * This class only has static members.  When CXXR::MemoryBank indicates
     * that it is on the point of requesting additional memory from
     * the operating system, the class decides whether to initiate a
     * garbage collection, and if so how many levels to collect.
     *
     * In the current implementation of GCManager, when cued by CXXR
     * as above, a garbage collection will be carried out if the
     * number of bytes currently allocated via CXXR::MemoryBank is at least
     * as great as a threshold value.  This threshold value varies
     * during the run, subject to a minimum value specified in the
     * initialize() method.
     */
    class GCManager {
    public:
	/** @brief Adjust the garbage collection threshold.
	 *
	 *  Adjust the garbage collection threshold in the light of
	 *  current allocations, and the space demand currently being
	 *  addressed.
	 *
	 * @param bytes_needed If specified, the number of bytes
	 *          currently being sought by CXXR::MemoryBank.
	 */
	static void adjustThreshold(size_t bytes_needed = 0);

	/** @brief Initiate a garbage collection.
	 *
	 * Note that enableGC() must have been called before this
	 * method is used.
	 *
	 * @param bytes_wanted An indication of the number of bytes
	 *          wanted in the event that prompted garbage
	 *          collection.  If in doubt, set it to 0.
	 *
	 * @param full If this is true, a garbage collection of all
	 *          generations of nodes is forced.  Otherwise
	 *          GCManager decides for itself how many generations
	 *          should be collected.
	 */
	static void gc(size_t bytes_wanted, bool full = false);

	/** @brief Enable garbage collection.
	 *
	 * No automatic garbage collection of GCNode objects will take
	 * place until this method has been called; nor may a
	 * collection may be initiated 'manually' by calling gc().
	 * The effect of calling enableGC() more than once during a
	 * single program run is undefined.
	 *
	 * @param initial_threshold  Initial value for the collection
	 *          threshold.
	 */
	static void enableGC(size_t initial_threshold);

	/**
	 * @return true iff garbage collection torture is enabled.
	 */
	static bool isTortured() {return s_tortured;}

	/** @brief Maximum number of bytes used.
	 *
	 * @return the maximum number of bytes used (up to the time of
	 *         the most recent garbage collection.)
	 */
	static size_t maxBytes() {return s_max_bytes;}

	/** @brief Maximum number of GCNode objects allocated.
	 * 
	 * @return the maximum number of GCNode objects allocated (up
	 * to the time of the most recent garbage collection.)
	 *
	 * @note This method is provided for compatibility with CR.
	 * The number of GCNode objects doesn't directly affect the operation
	 * of garbage collection in CXXR.
	 */
	static size_t maxNodes() {return s_max_nodes;}

	/** @brief Number of generations used by garbage collector.
	 * This will be at least 2, since one generation (Generation
	 * 0) is for newly created nodes still enjoying infant
	 * immunity.
	 *
	 * @return The number of generations into which GCNode objects
	 * are ranked by the garbage collector.
	 */
	static size_t numGenerations() {return s_num_old_generations + 2;}

	/** @brief Reset the tallies of the maximum numbers of bytes and
	 *  GCNode objects.
	 *
	 * This method resets the record of the maximum number of
	 * bytes allocated to the current number of bytes allocated,
	 * and similarly for the maximum number of GCNode objects.
	 */
	static void resetMaxTallies();

	/** @brief Set/unset monitors on garbage collection.
	 *
	 * No garbage collection of GCNode objects will take place
	 * until this method has been called.  The effect of calling
	 * it more than once during a single program run is undefined.
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
	 * If enabled, every time that CXXR::MemoryBank indicates that it is
	 * about to request additional memory from the operating
	 * system, a garbage collection is carried out.
	 *
	 * @param on The required torturing status.
	 */
	static void torture(bool on) {s_tortured = on;}

	/** @brief Current GC threshold level.
	 *
	 * @return The current threshold level.  When CXXR::MemoryBank
	 * indicates that it is on the point of requesting additional
	 * memory from the operating system, garbage collection will
	 * be triggered if the number of bytes currently allocated via
	 * CXXR::MemoryBank is at least as great as this level.
	 */
	static size_t triggerLevel() {return s_threshold;}
    private:
	static const size_t s_num_old_generations = 2;
	static const unsigned int s_collect_counts_max[s_num_old_generations];
	static unsigned int s_gen_gc_counts[s_num_old_generations + 1];
	
	static size_t s_threshold;
	static size_t s_min_threshold;

	static size_t s_max_bytes;
	static size_t s_max_nodes;

	static bool s_tortured;  // If this is true, every cue from
				 // CXXR::MemoryBank leads to a garbage
				 // collection.
	static std::ostream* s_os;  // Pointer to output stream for GC
				    // reporting, or NULL.

	// Not implemented.  Declared to stop the compiler generating
	// a constructor.
	GCManager();

	// Clean up static data associated with garbage collection.
	static void cleanup() {}

	// Callback for CXXR::MemoryBank to cue a garbage collection:
	static bool cue(size_t bytes_wanted, bool force);

	// Callbacks e.g. for timing:
	static void (*s_pre_gc)();
	static void (*s_post_gc)();

	// Detailed control of the garbage collection, in particular
	// choosing how many generations to collect, is carried out
	// here.
	static void gcGenController(size_t bytes_wanted, bool full);

	/** Choose how many generations to collect according to a rota.
	 *
	 * There are three levels of collections.  Level 0 collects only
	 * the youngest generation, Level 1 collects the two youngest
	 * generations, and Level 2 collects all generations.  This
	 * function decides how many old generations to collect according
	 * to a rota.  Most collections are Level 0.  However, after every
	 * collect_counts_max[0] Level 0 collections, a Level 1 collection
	 * will be carried out; similarly after every
	 * collect_counts_max[1] Level 1 collections a Level 2 collection
	 * will be carried out.
	 *
	 * @param minlevel (<= 2, not checked) This parameter places a
	 *          minimum on the number of old generations to be
	 *          collected.  If minlevel is higher than the number of
	 *          generations that genRota would have chosen for itself,
	 *          the position in the rota is advanced accordingly.
	 */
	static unsigned int genRota(unsigned int minlevel);
    };
}  // namespace CXXR

#endif /* GCMANAGER_HPP */
