/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2007 Andrew Runnalls.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <iostream>
#include "RCNTXT.h"
#include "CXXR/IntVector.h"
#include "CXXR/PairList.h"
#include "CXXR/RealVector.h"

using namespace std;
using namespace CXXR;

RCNTXT* R_GlobalContext;

int main() {
    cout << "sizeof(bool): " << sizeof(bool)
	 << "\nsizeof(GCNode): " << sizeof(GCNode)
	 << "\nsizeof(RObject): " << sizeof(RObject)
         << "\nsizeof(PairList): " << sizeof(PairList)
	 << "\nsizeof(IntVector): " << sizeof(IntVector)
	 << "\nsizeof(RealVector): " << sizeof(RealVector) << '\n';
    return 0;
}


    
