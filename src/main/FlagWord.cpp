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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/** @file FlagWord.cpp
 *
 * Implementation of class FlagWord.
 */

#include "CXXR/FlagWord.hpp"

using namespace CXXR;

unsigned short FlagWord::s_mask[] = {1, 2, 4, 8, 0x10, 0x20, 0x40, 0x80,
				     0x100, 0x200, 0x400, 0x800,
				     0x1000, 0x2000, 0x4000, 0x8000};
