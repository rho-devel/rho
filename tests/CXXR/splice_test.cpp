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
#include <list>

using namespace std;

int main() {
    list<int> l1, l2;
    l1.push_back(42);
    list<int>::iterator lit1 = l1.insert(l1.end(), 21);
    l1.push_back(64);
    list<int>::iterator lit2 = l1.insert(l1.end(), 32);
    l2.splice(l2.end(), l1, lit2);
    l2.splice(l2.end(), l1, lit1);
    cout << "*lit2 = " << *lit2 << "\n++lit2;\n";
    ++lit2;
    cout << "*lit2 = " << *lit2 << '\n';
    return 0;
}


    
