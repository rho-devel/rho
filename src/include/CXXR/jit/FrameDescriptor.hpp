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

#ifndef CXXR_JIT_FRAME_DESCRIPTOR_HPP
#define CXXR_JIT_FRAME_DESCRIPTOR_HPP

#include <vector>

namespace CXXR {
class Closure;
class Symbol;

namespace JIT {

/**
 * A FrameDescriptor creates a static mapping between the symbols expected to
 * be used in a function and integers that can be used as array offsets.
 *
 * This is used to create the layout for CompiledFrame.
 *
 * Note that it is not guaranteed that all symbols used in the function will
 * be listed in the FrameDescriptor.
 */
class FrameDescriptor {
public:
    explicit FrameDescriptor(const Closure* closure);

    explicit FrameDescriptor(std::initializer_list<const Symbol*> formals,
			     std::initializer_list<const Symbol*> locals);

    // Returns the index where the symbol is stored.  Returns -1 if the
    // symbol has not been added to the descriptor.
    int getLocation(const Symbol* symbol) const;

    //* Check if the symbol is one of the formal parameters to the function.
    bool isFormalParameter(const Symbol* symbol) const
    {
	return isFormalParameter(getLocation(symbol));
    }

    /**
     * Check if the symbol at location is one of the formal parameters to the
     * function.
     */
    bool isFormalParameter(int location) const
    {
	return location >= 0 && location < m_num_formals;
    }

    int getNumberOfSymbols() const
    {
	return m_local_vars.size();
    };

private:
    std::vector<const Symbol*> m_local_vars;
    int m_num_formals;
};

} // namespace JIT
} // namespace CXXR

#endif // CXXR_JIT_FRAME_DESCRIPTOR_HPP
