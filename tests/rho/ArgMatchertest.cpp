/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
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

/** @file Test of class ArgMatcher.
 */

#define R_NO_REMAP

#include "rho/ArgMatcher.hpp"

#define R_INTERFACE_PTRS

#include <algorithm>
#include <fstream>
#include <iostream>
#include "boost/regex.hpp"

// For Rf_InitOptions():
#include "Defn.h"

// Inclusion of Defn.h must precede this:
#include "Rinterface.h"

// Otherwise expanded to Rf_match:
#undef match

#include "rho/CommandTerminated.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/ListFrame.hpp"
#include "rho/PairList.hpp"
#include "rho/Symbol.hpp"

using namespace std;
using namespace rho;

extern "C" {
    void WriteConsoleEx(const char* buf, int, int)
    {
	cout << buf << endl;
    }

    void DoNothing() { }
}

namespace {
    Environment *fenv;

    string kv_regex_string("([\\w\\.]*)\\s*:\\s*(\\w*)");
    boost::basic_regex<char> kv_regex(kv_regex_string);

    PairList* getArgs(const char* filename)
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
		    cerr << "Lines must match_" << kv_regex_string << '\n';
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
	    GCStackRoot<PairList> ans;
	    for (Vps::const_reverse_iterator rit = keyvals.rbegin();
		 rit != keyvals.rend(); ++rit) {
		const string& namestr = (*rit).first;
		const string& valstr = (*rit).second;
		GCStackRoot<> value;
		if (valstr.empty())
		    value = Symbol::missingArgument();
		else value
		    = const_cast<String*>(String::obtain(valstr.c_str()));
		const RObject* tag = 0;
		if (!namestr.empty())
		    tag = Symbol::obtain(namestr.c_str());
		ans = PairList::cons(value, ans, const_cast<RObject*>(tag));
	    }
	    return ans;
	}
    }

    string getString(const RObject* value)
    {
	const String* str = dynamic_cast<const String*>(value);
	if (!str) {
	    cerr << "String expected.\n";
	    abort();
	}
	return str->stdstring();
    }

    string originString(Frame::Binding::Origin origin)
    {
	static string origstr[] = {"EXPLICIT  ", "MISSING   ", "DEFAULTED "};
	return origstr[origin];
    }

    void showValue(RObject* value);

    void showConsCell(ConsCell* cell)
    {
	const Symbol* tag = SEXP_downcast<const Symbol*>(cell->tag());
	if (tag)
	    cout << tag->name()->c_str() << ' ';
	cout << ": ";
	showValue(cell->car());
    }

    void showValue(RObject* value)
    {
	if (!value)
	    cout << "NULL";
	else {
	    switch (value->sexptype()) {
	    case CHARSXP:
		{
		    cout << getString(value);
		}
		break;
	    case DOTSXP:
		{
		    ConsCell* cell = static_cast<ConsCell*>(value);
		    cout << "DottedArgs(";
		    showConsCell(cell);
		    cell = cell->tail();
		    while (cell) {
			cout << ", ";
			showConsCell(cell);
			cell = cell->tail();
		    }
		    cout << ')';
		}
		break;
	    case PROMSXP:
		{
		    Promise* prom = static_cast<Promise*>(value);
		    cout << "Promise("
			 << getString(prom->valueGenerator())
			 << ", ";
		    Environment* env = prom->environment();
		    if (env == fenv)
			cout << "fenv)";
		    else {
			cerr << "Unexpected environment.\n";
			abort();
		    }
		}
		break;
	    case SYMSXP:
		{
		    if (value == Symbol::missingArgument())
			cout << "Symbol::missingArgument()";
		    else {
			Symbol* sym = static_cast<Symbol*>(value);
			cout << "Symbol(" << getString(sym->name()) << ')';
		    }
		}
		break;
	    default:
		cerr << "Unexpected SEXPTYPE.\n";
		abort();
	    }
	}
    }

    void showFrame(const Frame* frame)
    {
	typedef map<const String*, const Frame::Binding*,
	    String::Comparator> FrameMap;
	FrameMap fmap;
	// Get bindings sorted by name:
	{
	    GCStackRoot<PairList> pl(frame->asPairList());
	    while (pl) {
		const Symbol* sym = dynamic_cast<const Symbol*>(pl->tag());
		if (!sym) {
		    cerr << "Binding tag isn't a Symbol.\n";
		    abort();
		}
		fmap[sym->name()] = frame->binding(sym);
		pl = pl->tail();
	    }
	}
	// Write out bindings in name order:
	for (FrameMap::const_iterator it = fmap.begin();
	     it != fmap.end(); ++it) {
	    const Frame::Binding* bdg = (*it).second;
	    string tag = bdg->symbol()->name()->stdstring();
	    RObject* value = bdg->unforcedValue();
	    cout << originString(bdg->origin()) << tag << " : ";
	    showValue(value);
	    cout << endl;
	}
    }

    void usage(const char* cmd)
    {
	cerr << "Usage: " << cmd
	     << " formal_args_file supplied_args_file [prior_bindings]\n";
	exit(1);
    }
}

int main(int argc, char* argv[]) {
    if (argc < 3 || argc > 4)
	usage(argv[0]);
    // Set up error reporting:
    ptr_R_WriteConsoleEx = WriteConsoleEx;
    ptr_R_ResetConsole = ptr_R_FlushConsole = 
        ptr_R_ClearerrConsole = DoNothing;
    
    Rf_InitArithmetic();
    Rf_InitMemory();
    Rf_InitNames();
    Rf_InitGlobalEnv();
    Rf_InitOptions();

    Evaluator evalr;

    // Set up Environments:
    GCStackRoot<Frame> ff(new ListFrame);
    GCStackRoot<Environment> fenvrt(new Environment(0, ff));
    fenv = fenvrt;
    // Process formals:
    cout << "Formal arguments:\n\n";
    GCStackRoot<PairList> formals(getArgs(argv[1]));
    GCStackRoot<ArgMatcher> matcher;
    try {
    	matcher = new ArgMatcher(formals);
    } catch (CommandTerminated) {
	cerr << "ArgMatchertest: Error encountered while processing formals" << endl;
	return 0;
    }
    // Process supplied arguments:
    cout << "\nSupplied arguments:\n\n";
    ArgList supplied(getArgs(argv[2]), ArgList::RAW);
    // Set up frame and prior bindings (if any):
    Frame* frame = fenv->frame();
    if (argc == 4) {
	cout << "\nPrior bindings:\n\n";
	GCStackRoot<PairList> prior_bindings(getArgs(argv[3]));
	for (PairList* pb = prior_bindings; pb; pb = pb->tail()) {
	    const Symbol* tag = static_cast<const Symbol*>(pb->tag());
	    Frame::Binding* bdg = frame->obtainBinding(tag);
	    bdg->setValue(pb->car(), Frame::Binding::EXPLICIT);
	}
    }
    // Perform match and show result:
    try {
	matcher->match(fenv, &supplied);
    } catch (CommandTerminated) {
    	cerr << "ArgMatchertest: Error encountered while matching arguments" << endl;
	return 0;
    }
    cout << "\nMatch result:\n\n";
    showFrame(frame);
    return 0;
}


    
