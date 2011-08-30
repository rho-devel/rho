/*CXXR $Id:$
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

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file provenance.cpp
 *
 * @brief Provenance-related R functions
 */

#include <fstream>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <set>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include "CXXR/Provenance.hpp"
#include "CXXR/Parentage.hpp"

using namespace std;
using namespace CXXR;

SEXP attribute_hidden do_castestfun(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int n;
	if ((n=length(args))!=1)
		errorcall(call,_("%d arguments passed to 'castestfun' which requires 1"),n);

	if (TYPEOF(CAR(args))!=SYMSXP)
		errorcall(call,_("castestfun expects Symbol argument"));
    /*GCStackRoot<IntVector> v(GCNode::expose(new IntVector(3)));
    (*v)[0]=1;
    (*v)[1]=2;
    (*v)[2]=3;
    return v;*/
	/*GCStackRoot<LogicalVector> a(SEXP_downcast<LogicalVector*>(CAR(args)));
	GCStackRoot<LogicalVector> v(GCNode::expose(new LogicalVector(1)));
	if ((*a)[0]==true)
		(*v)[0]=true;
	else
		(*v)[0]=false;
	return v;*/
	Symbol* sym=SEXP_downcast<Symbol*>(CAR(args));
	Environment* env=static_cast<Environment*>(rho);
	// Let's try to get the binding for given symbol...
	pair<Environment*, Frame::Binding*> bdg=CXXR::findBinding(sym,env);
	Frame::Binding* binding=CXXR::findBinding(sym,env).second;
	if (binding!=NULL)
		printf("Binding located :-)\n");
	GCStackRoot<IntVector> inv(GCNode::expose(new IntVector(3)));
	(*inv)[0]=1;
	(*inv)[1]=2;
	(*inv)[2]=3;
	GCStackRoot<IntVector> inv2(GCNode::expose(new IntVector(2)));
	(*inv2)[0]=4;
	(*inv2)[1]=5;

	GCStackRoot<StringVector> str(GCNode::expose(new StringVector(2)));
	(*str)[0]=const_cast<CachedString*>(CachedString::obtain("ivOne"));
	(*str)[1]=const_cast<CachedString*>(CachedString::obtain("ivTwo"));

	GCStackRoot<ListVector> rc(GCNode::expose(new ListVector(2)));
	(*rc)[0]=inv;
	(*rc)[1]=inv2;

	setAttrib(rc, R_NamesSymbol, str);
	return rc;
}

SEXP attribute_hidden do_hasProvenance (SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int n;
	if ((n=length(args))!=1)
		errorcall(call,_("%d arguments passed to 'hasProvenance' which requires 1"),n);

	if (TYPEOF(CAR(args))!=SYMSXP)
		errorcall(call,_("hasProvenance expects Symbol argument"));

	Symbol* sym=SEXP_downcast<Symbol*>(CAR(args));
	Environment* env=static_cast<Environment*>(rho);
	Frame::Binding* bdg=findBinding(sym,env).second;
	GCStackRoot<LogicalVector> v(GCNode::expose(new LogicalVector(1)));
	(*v)[0]=bdg->hasProvenance();
	return v;
}

SEXP attribute_hidden do_provenance (SEXP call, SEXP op, SEXP args, SEXP rho)
{
	const int nfields=5;
	int n;
	if ((n=length(args))!=1)
		errorcall(call,_("%d arguments passed to 'provenance' which requires 1"),n);

	if (TYPEOF(CAR(args))!=SYMSXP)
		errorcall(call,_("provenance expects Symbol argument"));
	Symbol* sym=SEXP_downcast<Symbol*>(CAR(args));
	Environment* env=static_cast<Environment*>(rho);
	Frame::Binding* bdg=findBinding(sym,env).second;
	if (!bdg)
		errorcall(call,_("invalid Symbol passed to 'provenance'"));
	Provenance* provenance=const_cast<Provenance*>(bdg->getProvenance());
	if (!provenance)
		errorcall(call,_("object does not have any provenance"));
	Parentage* parentage=provenance->getParentage();
	Provenance::Set* children=provenance->children();

	GCStackRoot<ListVector> list(GCNode::expose(new ListVector(nfields)));
	GCStackRoot<StringVector> timestamp(GCNode::expose(new StringVector(1)));
	GCStackRoot<StringVector> names(GCNode::expose(new StringVector(nfields)));

	(*timestamp)[0]=const_cast<CachedString*>(provenance->getTime());

	(*names)[0]=const_cast<CachedString*>(CachedString::obtain("command"));
	(*names)[1]=const_cast<CachedString*>(CachedString::obtain("symbol"));
	(*names)[2]=const_cast<CachedString*>(CachedString::obtain("timestamp"));
	(*names)[3]=const_cast<CachedString*>(CachedString::obtain("parents"));
	(*names)[4]=const_cast<CachedString*>(CachedString::obtain("children"));

	(*list)[0]=provenance->getCommand();
	(*list)[1]=provenance->getSymbol();
	(*list)[2]=timestamp;
	(*list)[3]=(parentage) ?
		    parentage->asStringVector() : 
		    static_cast<RObject*>(R_NilValue) ;
	(*list)[4]=(!children->empty()) ?
		     Provenance::setAsStringVector(children) :
		     static_cast<RObject*>(R_NilValue);

	setAttrib(list,R_NamesSymbol,names);

	return list;
}

SEXP attribute_hidden do_provCommand (SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int n;
	if ((n=length(args))!=1)
		errorcall(call,_("%d arguments passed to 'provCommand' which requires 1"),n);

	if (TYPEOF(CAR(args))!=SYMSXP)
		errorcall(call,_("provCommand expects Symbol argument"));

	Symbol* sym=SEXP_downcast<Symbol*>(CAR(args));
	Environment* env=static_cast<Environment*>(rho);
	Frame::Binding* bdg=findBinding(sym,env).second;
	return bdg->getProvenance()->getCommand();
}

SEXP attribute_hidden do_pedigree (SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int n;
	if ((n=length(args))!=1)
		errorcall(call,_("%d arguments passed to 'pedigree' which requires 1"),n);

	Environment* env=static_cast<Environment*>(rho);
	
	Provenance::Set *provs;

	if (TYPEOF(CAR(args))==SYMSXP) {
		Symbol* sym=SEXP_downcast<Symbol*>(CAR(args));
		Frame::Binding* bdg=findBinding(sym,env).second;
		Provenance* prov=const_cast<Provenance*>(bdg->getProvenance());
		provs=new Provenance::Set();
		provs->insert(prov);
	} else if (TYPEOF(CAR(args))==LANGSXP) {
		SEXP rc;
		rc=eval(CAR(args),rho);
		if (TYPEOF(rc)!=STRSXP)
			errorcall(call,_("Expression supplied to pedigree does not yield a String vector"));

		provs=new Provenance::Set();
		StringVector *sv=static_cast<StringVector*>(rc);
		for (int i=0;i<length(rc);i++) {
			Symbol *sym=Symbol::obtain((*sv)[i]->c_str());
			Frame::Binding *bdg=findBinding(sym,env).second;
			if (bdg) {
				Provenance *prov=const_cast<Provenance*>(bdg->getProvenance());
				if (prov)
					provs->insert(prov);
			}
		}
	} else
		errorcall(call,_("pedigree expects a Symbol or Expression that evaluates to a String vector"));

	Provenance::Set *ancestors=Provenance::ancestors(provs);
	for (Provenance::Set::iterator it=ancestors->begin();
	     it!=ancestors->end();
	     it++) {
		Provenance* p=(*it);
		PrintValue(p->getCommand());
	}
	delete provs;
	delete ancestors;
	return R_NilValue;
}


SEXP attribute_hidden do_bserialize (SEXP call, SEXP op, SEXP args, SEXP rho)
{
    std::ofstream ofs("bserialize.out");
	
    const int n=length(args);
    if (n>0)
	errorcall(call,_("%d arguments passed to 'bserialize' which requires 0"),n);

    Environment* env=new Environment(0);
    GCStackRoot<Environment> envProtect(GCNode::expose(env));
    Frame* frame=env->frame();

    if (n==0) {
	Frame *gEnvFrame=static_cast<Environment*>(R_GlobalEnv)->frame();
	frame->import(gEnvFrame);
    }


    boost::archive::text_oarchive oa(ofs);
    oa << env;

    return R_NilValue;
}

SEXP attribute_hidden do_bdeserialize (SEXP call, SEXP op, SEXP args, SEXP rho)
{
	std::ifstream ifs("bserialize.out");
	boost::archive::text_iarchive ia(ifs);
	Environment* env;
	ia >> env;

	GCStackRoot<Environment> envProtect(GCNode::expose(env)); // Protect from GC

	Environment* envGlobal = static_cast<Environment*>(R_GlobalEnv);
	Frame* frame = envGlobal->frame();
	frame->import(env->frame());

	return R_NilValue;
}
