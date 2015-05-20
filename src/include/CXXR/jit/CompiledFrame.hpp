/*
 *  R : A Computer Language for Statistical Data Analysis
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

#ifndef CXXR_JIT_COMPILED_FRAME_HPP
#define CXXR_JIT_COMPILED_FRAME_HPP

#include "CXXR/ListFrame.hpp"
#include "CXXR/GCEdge.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"
#include <map>

namespace CXXR {
class Symbol;

namespace JIT {
class FrameDescriptor;

/*
 * A CompiledFrame is a frame which stores the bindings for the symbols in the
 * FrameDescriptor in a known location, for efficient lookup.
 * Symbols that are not in the FrameDescriptor get overflowed to a table on the
 * side.
 */
class CompiledFrame : public ListFrame {
public:
    explicit CompiledFrame(const FrameDescriptor* descriptor);
    CompiledFrame(const CompiledFrame& pattern);

    // Binding must exist, or returns null.
    Binding* binding(int location)
    {
    	assert(location >= 0);
    	assert(location < m_descriptor->getNumberOfSymbols());
    	Binding* binding = m_bindings + location;
    	if (isSet(*binding)) {
    	    return binding;
    	} else {
    	    return nullptr;
    	}
    }

    const Binding* binding(int location) const
    {
    	return const_cast<CompiledFrame*>(this)->binding(location);
    }

    Binding* obtainBinding(const Symbol* symbol, int location)
    {
    	assert(location >= 0);
    	assert(location < m_descriptor->getNumberOfSymbols());
    	Binding* binding = m_bindings + location;
    	if (!isSet(*binding)) {
	    initializeBinding(binding, symbol);
	}
	return binding;
    }

    CompiledFrame* clone() const override;

    const FrameDescriptor* getDescriptor() const
    {
	return m_descriptor;
    }

protected:
    Binding* v_obtainBinding(const Symbol* symbol) override;

    void detachReferents() override;
    void visitReferents(const_visitor* v) const override;
private:
    ~CompiledFrame() override;

    GCEdge<const FrameDescriptor> m_descriptor;

    CompiledFrame& operator=(const CompiledFrame&) = delete;
};

} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_COMPILED_FRAME_HPP
