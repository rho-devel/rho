/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
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

/** @file ListFrame.hpp
 *
 * @brief Class CXXR::ListFrame.
 */

#ifndef LISTFRAME_HPP
#define LISTFRAME_HPP

#include <map>

#include "CXXR/Allocator.hpp"
#include "CXXR/Frame.hpp"

namespace CXXR {
    /** @brief Lightweight implementation of CXXR::Frame.
     *
     * This implementation is intended for Frames which might contain
     * only a small number of Bindings.  Look-up time is very fast and few
     * memory allocations are required as long as the number of bindings
     * is small.
     * For large numbers of bindings, lookups and insertions are still O(1),
     * but not as fast.
     *
     * This is implemented via a fixed-size unordered array of bindings.
     * For small sizes, the linear scan through the array is fast and cache
     * efficient.
     * If more bindings are added to the frame than the array can store,
     * the remaining bindings are added to a hashmap.  This ensures that
     * the frame can handle large numbers of bindings reasonably efficiently.
     */
    class ListFrame : public Frame {
    public:
	explicit ListFrame(size_t list_size = 16);
	ListFrame(const ListFrame &pattern);
	
        // Virtual functions of Frame (qv):
	void visitBindings(std::function<void(const Binding*)> f)
	    const override;
	ListFrame* clone() const override;
	void lockBindings() override;
	std::size_t size() const override;

    protected:

	// The main array that bindings are stored in.
	// Note that the array cannot be moved or expanded, as there is a
	// bunch of code (most notably the global cache) that keeps pointers
	// to bindings.
	Binding* m_bindings;
	size_t m_bindings_size;
	size_t m_used_bindings_size;
	
	// Used to store any bindings that don't fit in m_bindings.
	// Usually this is nullptr.
	std::map<const Symbol*, Binding>* m_overflow;

	// Declared protected to ensure that ListFrame objects are
	// created only using 'new':
	~ListFrame() override;

	ListFrame& operator=(const ListFrame&) = delete;

	static bool isSet(const Binding& binding)
	{
	    return binding.frame() != nullptr;
	}
	
	static void unsetBinding(Binding* binding);

	// Virtual functions of Frame (qv):
	void v_clear() override;
	bool v_erase(const Symbol* symbol) override;
	Binding* v_obtainBinding(const Symbol* symbol) override;
	Binding* v_binding(const Symbol* symbol) override;
	const Binding* v_binding(const Symbol* symbol) const override;
    };
}  // namespace CXXR
#endif // LISTFRAME_HPP
