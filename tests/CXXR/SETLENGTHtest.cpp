/*
 *  R : A Computer Language for Statistical Data Analysis
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

/** @file Test of SETLENGTH().
 */

#include <algorithm>
#include <iostream>
#include <iterator>
#include <functional>

#include "CXXR/GCStackRoot.hpp"
#include "CXXR/StringVector.h"
#include "Defn.h"

using namespace std;
using namespace CXXR;

namespace {
    void show(const StringVector* sv)
    {
	std::transform(sv->begin(), sv->end(),
		       ostream_iterator<const char*>(cout, "|"),
		       std::mem_fun(&String::c_str));
	const StringVector* names = sv->names();
	if (names) {
	    cout << "  Names: ";
	    std::transform(names->begin(), names->end(),
			   ostream_iterator<const char*>(cout, "|"),
			   std::mem_fun(&String::c_str));
	}
	cout << "\n";
    }
}

int main()
{
    Rf_InitMemory();
    Rf_InitNames();

    GCStackRoot<StringVector> sv(StringVector::create(4));
    (*sv)[0] = String::obtain("fee");
    (*sv)[1] = String::obtain("fie");
    (*sv)[2] = String::obtain("fo");
    (*sv)[3] = String::obtain("fum");
    const char* names[] = {"I", "II", "III", "IV"};
    GCStackRoot<StringVector> namesv(StringVector::create(4));
    for (size_t i = 0; i < 4; ++i)
	(*namesv)[i] = String::obtain(names[i]);
    sv->setNames(namesv);
    show(sv);
    SETLENGTH(sv, 3);
    show(sv);
    SETLENGTH(sv, 2);
    show(sv);
    SETLENGTH(sv, 1);
    show(sv);
    SETLENGTH(sv, 0);
    show(sv);
    return 0;
}
