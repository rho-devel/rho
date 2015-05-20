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

#define R_NO_REMAP

#include "CXXR/jit/CompiledFrame.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"
#include "CXXR/StdFrame.hpp"

namespace CXXR {
namespace JIT {

CompiledFrame::CompiledFrame(const FrameDescriptor* descriptor)
    : ListFrame(descriptor->getNumberOfSymbols())
{
    m_descriptor = descriptor;
    m_used_bindings_size = m_bindings_size;
}

CompiledFrame::CompiledFrame(const CompiledFrame& pattern)
    : CompiledFrame(pattern.m_descriptor)
{
    importBindings(&pattern);
    if (pattern.isLocked()) {
	lock(false);
    }
}

CompiledFrame::~CompiledFrame()
{ }

CompiledFrame* CompiledFrame::clone() const
{
    return new CompiledFrame(*this);
}

void CompiledFrame::detachReferents()
{
    ListFrame::detachReferents();
    m_descriptor.detach();
}

void CompiledFrame::visitReferents(const_visitor* v) const
{
    ListFrame::visitReferents(v);
    (*v)(m_descriptor);
}

// Unlike ListFrame, the location to insert symbols is controlled by
// the descriptor.
Frame::Binding* CompiledFrame::v_obtainBinding(const Symbol* symbol)
{
    int location = m_descriptor->getLocation(symbol);
    if (location != -1) {
	return m_bindings + location;
    }
    if (!m_overflow) {
	m_overflow = new std::map<const Symbol*, Binding>();
    }
    return &((*m_overflow)[symbol]);
}


} // namespace JIT
} // namespace CXXR
