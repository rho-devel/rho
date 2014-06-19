/*CXXR
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

/*
 *  R : A Computer Language for Statistical Data Analysis
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

#include "CXXR/Frame.hpp"
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
class CompiledFrame : public Frame {
public:
    CompiledFrame(const FrameDescriptor* descriptor);
    CompiledFrame(const CompiledFrame& pattern);
    ~CompiledFrame() override;

    Binding* binding(const Symbol* symbol) override;

    const Binding* binding(const Symbol* symbol) const override
    {
	return const_cast<CompiledFrame*>(this)->binding(symbol);
    }

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

    BindingRange bindingRange() const override;

    CompiledFrame* clone() const override;

    void lockBindings() override;

    std::size_t size() const override
    {
	return m_descriptor->getNumberOfSymbols()
	       + (m_extension ? m_extension->size() : 0);
    }

    const FrameDescriptor* getDescriptor() const
    {
	return m_descriptor;
    }

protected:
    void v_clear() override;
    bool v_erase(const Symbol* symbol) override;
    Binding* v_obtainBinding(const Symbol* symbol) override;

private:
    Binding* m_bindings;

    const FrameDescriptor* m_descriptor;

    // Used to store any bindings not described in the descriptor.
    // Usually this is nullptr.
    std::map<const Symbol*, Binding>* m_extension;

    static bool isSet(const Binding& binding)
    {
	return binding.frame() != nullptr;
    }

    static void unsetBinding(Binding* binding);

    CompiledFrame& operator=(const CompiledFrame&) = delete;
};

} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_COMPILED_FRAME_HPP
