/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */
#ifndef RHO_STRUTIL_HPP
#define RHO_STRUTIL_HPP

#include <sstream>

namespace rho {
namespace internal {
    inline void strcat_helper(std::ostringstream& stream) { }

    template<typename Arg, typename... Args>
    inline void strcat_helper(std::ostringstream& stream,
                              Arg arg, Args... args) {
        stream << arg;
        strcat_helper(stream, args...);
    }
}

template<typename... Args>
std::string StrCat(Args... args) {
    std::ostringstream stream;
    internal::strcat_helper(stream, args...);
    return stream.str();
}

}  // namespace rho

#endif  // RHO_STRUTIL_HPP
