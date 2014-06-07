/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-14 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/** @file Evaluator_Context.cpp
 *
 * Implementation of class CXXR::Evaluator::Context.
 *
 * @note The long name Evaluator_Context.cpp is used rather than
 * Context.cpp because the latter would differ only in case from the
 * filename of context.cpp derived from CR's context.c.
 */

#include "CXXR/Evaluator_Context.hpp"

using namespace std;
using namespace CXXR;
