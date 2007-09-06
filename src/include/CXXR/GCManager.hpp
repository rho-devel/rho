/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Andrew Runnalls (C) 2007
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file GCManager.hpp
 * Class GCManager.
 *
 * @todo Reinstate garbage collection timing.
 * @todo Update DEBUG_GC_SUMMARY etc.
 */

#ifndef GCMANAGER_HPP
#define GCMANAGER_HPP

#include <cstddef>

namespace CXXR {
    /** Class for managing garbage collection.
     * 
     * This class only has static members.  When CXXR::Heap indicates
     * that it is on the point of requesting additional memory from
     * the operating system, the class decides whether to initiate a
     * garbage collection, and if so how many levels to collect.
     *
     * In the current implementation of GCManager, when cued by CXXR
     * as above, a garbage collection will be carried out if the
     * number of bytes currently allocated via CXXR::Heap is at least
     * as great as a threshold value.  This threshold values varies
     * during the run, subject to a user-specifiable maximum; by
     * default, the maximum is set to
     * <tt>numeric_limits<size_t>::max()</tt>.
     */
    struct GCManager {
	/** Adjust the garbage collection threshold in the light of
	 *  current allocations, and the space demand currently being
	 *  addressed.
	 *
	 * @param bytes_needed If specified, the number of bytes
	 *          currently being sought by CXXR::Heap.
	 */
	static void adjustThreshold(size_t bytes_needed = 0);

	/** Initiate a garbage collection.
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

	/** Initialize static members.
	 *
	 * This method must be called before any GCNodes are created.
	 * If called more than once in a single program run, the
	 * second and subsequent calls do nothing.
	 *
	 * @param initial_threshold  Initial value for the collection
	 *          threshold.
	 */
	static void initialize(size_t initial_threshold);

	/**
	 * @return true iff garbage collection torture is enabled.
	 */
	bool isTortured() {return s_tortured;}

	/**
	 * @return the current maximum permitted value of the collection
	 * threshold.
	 */
	static size_t maxThreshold() {return s_max_threshold;}

	/**
	 * @return the maximum number of bytes used (up to the time of
	 *         the most recent garbage collection.)
	 */
	static size_t maxBytes() {return s_max_bytes;}

	/** Set maximum permitted value for the collection threshold.
	 *
	 * @param newmax The new maximum value required.  An attempt
	 *          to set this maximum value to less than the current
	 *          value of the actual threshold will be silently
	 *          ignored.
	 */
	static void setMaxThreshold(size_t newmax);

	/** Turn garbage collection torture on or off.  If enabled,
	 * every time that CXXR::Heap indicates that it is about to
	 * request additional memory from the operating system, a
	 * garbage collection is carried out.
	 *
	 * @param on The required torturing status.
	 */
	static void torture(bool on) {s_tortured = on;}

	// To be private in future:

	static size_t s_threshold;
	static size_t s_max_threshold;
	static size_t s_min_threshold;

	static size_t s_max_bytes;

	static bool s_tortured;  // If this is true, every cue from
				 // CXXR::Heap leads to a garbage
				 // collection.

	// Callback for CXXR::Heap to cue a garbage collection:
	static bool cue(size_t bytes_wanted, bool force);
    };
}  // namespace CXXR

#endif /* GCMANAGER_HPP */
