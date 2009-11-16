/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/** @file Test of class ArgMatcher.
 */

#include "CXXR/ArgMatcher.hpp"

#define R_INTERFACE_PTRS

#include <algorithm>
#include <fstream>
#include <iostream>
#include "boost/regex.hpp"
#include "Rinterface.h"
#include "CXXR/CachedString.h"
#include "CXXR/GCStackRoot.h"
#include "CXXR/PairList.h"
#include "CXXR/Symbol.h"

using namespace std;
using namespace CXXR;

extern "C" {
    void WriteConsoleEx(const char* buf, int, int)
    {
	cout << buf << endl;
    }
}

namespace {
    boost::basic_regex<char> kv_regex("(\\w*)\\s*:\\s*(\\w*)");

    PairList* getArgs(const char* filename, bool names_as_symbols)
    {
	ifstream astrm(filename);
	if (!astrm) {
	    cerr << "Cannot open file " << filename << '\n';
	    exit(1);
	}
	typedef vector<pair<string, string> > Vps;
	Vps keyvals;
	// Read key-value pairs:
	{
	    string line;
	    while (getline(astrm, line)) {
		boost::smatch kv_match;
		if (!boost::regex_match(line, kv_match, kv_regex)) {
		    cerr << "Lines must match (\\w*)\\s*:\\s*(\\w*)\n";
		    exit(1);
		}
		cout << kv_match[1] << " : " << kv_match[2] << endl;
		keyvals.push_back(make_pair(kv_match[1], kv_match[2]));
	    }
	    if (!astrm.eof()) {
		cerr << "Read error in " << filename << '\n';
		exit(1);
	    }
	}
	// Construct result:
	{
	    PairList* ans = 0;
	    for (Vps::const_reverse_iterator rit = keyvals.rbegin();
		 rit != keyvals.rend(); ++rit) {
		const string& namestr = (*rit).first;
		const string& valstr = (*rit).second;
		CachedString* value
		    = const_cast<CachedString*>(CachedString::obtain(valstr.c_str()));
		const RObject* tag = 0;
		if (!namestr.empty()) {
		    if (names_as_symbols)
			tag = Symbol::obtain(namestr.c_str());
		    else tag = CachedString::obtain(namestr.c_str());
		}
		ans = PairList::construct(value, ans,
					  const_cast<RObject*>(tag));
	    }
	    return ans;
	}
    }

    void usage(const char* cmd)
    {
	cerr << "Usage: " << cmd << " formal_args_file supplied_args_file\n";
	exit(1);
    }
}

int main(int argc, char* argv[]) {
    if (argc != 3)
	usage(argv[0]);
    // Set up error reporting:
    ptr_R_WriteConsoleEx = WriteConsoleEx;
    // Process formals:
    cout << "Formal arguments:\n\n";
    GCStackRoot<PairList> formals(getArgs(argv[1], true));
    GCStackRoot<ArgMatcher>
	matcher(GCNode::expose(new ArgMatcher(formals, 0)));
    // Process supplied arguments:
    cout << "\nSupplied arguments:\n\n";
    GCStackRoot<PairList> supplied(getArgs(argv[2], false));
    return 0;
}


    
