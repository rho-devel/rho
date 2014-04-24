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

/** @file Subscripting.cpp
 *
 * Implementation of class CXXR::Subscripting and associated functions.
 */

#include <set>
#include <tr1/unordered_map>
#include "CXXR/RAllocStack.h"
#include "CXXR/RealVector.h"
#include "CXXR/Subscripting.hpp"

using namespace CXXR;

std::pair<const IntVector*, std::size_t>
Subscripting::canonicalize(const IntVector* raw_indices, std::size_t range_size)
{
    const std::size_t rawsize = raw_indices->size();
    bool anyNA = false;
    bool anyneg = false;
    std::size_t zeroes = 0;
    int max_index = 0;
    for (std::size_t i = 0; i < rawsize; ++i) {
	int index = (*raw_indices)[i];
	if (isNA(index))
	    anyNA = true;
	else if (index < 0)
	    anyneg = true;
	else if (index == 0)
	    ++zeroes;
	else if (index > max_index)
	    max_index = index;
    }
    if (!anyneg) {
	// Check if raw_indices is already in the required form:
	if (zeroes == 0)
	    return std::make_pair(raw_indices, max_index);
	// Otherwise suppress zeroes:
	GCStackRoot<IntVector> ans(CXXR_NEW(IntVector(rawsize - zeroes)));
	std::size_t iout = 0;
	for (std::size_t iin = 0; iin < rawsize; ++iin) {
	    int index = (*raw_indices)[iin];
	    if (index != 0)
		(*ans)[iout++] = index;
	}
	return std::pair<const IntVector*, std::size_t>(ans, std::size_t(max_index));
    } else {  // Negative subscripts
	if (anyNA || max_index > 0)
	    Rf_error(_("only 0's may be mixed with negative subscripts"));
	// Create a LogicalVector to show which elements are to be retained:
	GCStackRoot<LogicalVector>
	    lgvec(CXXR_NEW(LogicalVector(range_size, 1)));
	for (std::size_t i = 0; i < rawsize; ++i) {
	    std::size_t index = std::size_t(-(*raw_indices)[i]);
	    if (index != 0 && index <= range_size)
		(*lgvec)[index - 1] = 0;
	}
	return canonicalize(lgvec, range_size);
    }
}

std::pair<const IntVector*, std::size_t>
Subscripting::canonicalize(const LogicalVector* raw_indices, std::size_t range_size)
{
    const std::size_t rawsize = raw_indices->size();
    if (rawsize == 0)
	return std::make_pair(CXXR_NEW(IntVector(0)), 0);
    std::size_t nmax = std::max(range_size, rawsize);
    // Determine size of answer:
    std::size_t anssize = 0;
    for (std::size_t i = 0; i < nmax; ++i)
	if ((*raw_indices)[i%rawsize] != 0)
	    ++anssize;
    // Create canonical index vector:
    GCStackRoot<IntVector> ans(CXXR_NEW(IntVector(anssize)));
    {
	std::size_t iout = 0;
	for (std::size_t iin = 0; iin < nmax; ++iin) {
	    int logical = (*raw_indices)[iin % rawsize];
	    if (isNA(logical))
		(*ans)[iout++] = NA<int>();
	    else if (logical != 0)
		(*ans)[iout++] = int(iin + 1);
	}
    }
    return std::pair<const IntVector*, std::size_t>(ans, nmax);
}

std::pair<const IntVector*, std::size_t>
Subscripting::canonicalize(const RObject* subscripts, std::size_t range_size,
			   const StringVector* range_names)
{
    if (!subscripts)
	return std::pair<const IntVector*, std::size_t>(CXXR_NEW(IntVector(0)), 0);
    switch (subscripts->sexptype()) {
    case LGLSXP:
	return canonicalize(static_cast<const LogicalVector*>(subscripts),
			    range_size);
    case INTSXP:
	return canonicalize(static_cast<const IntVector*>(subscripts),
			    range_size);
    case REALSXP:
	{
	    const RealVector* rsub = static_cast<const RealVector*>(subscripts);
	    std::size_t rawsize = rsub->size();
	    GCStackRoot<IntVector> isub(CXXR_NEW(IntVector(rawsize)));
	    for (std::size_t i = 0; i < rawsize; ++i)
		(*isub)[i] = int((*rsub)[i]);
	    return canonicalize(isub, range_size);
	}
    case STRSXP:
	return canonicalize(static_cast<const StringVector*>(subscripts),
			    range_size, range_names);
    case SYMSXP:
	{
	    const Symbol* sym = static_cast<const Symbol*>(subscripts);
	    if (sym == Symbol::missingArgument()) {
		// If a subscript argument is missing, we take this as
		// meaning 'select everything':
		GCStackRoot<IntVector> ivec(CXXR_NEW(IntVector(range_size)));
		for (std::size_t i = 0; i < range_size; ++i)
		    (*ivec)[i] = int(i + 1);
		return std::pair<const IntVector*, std::size_t>(ivec, range_size);
	    }
	    // Else deliberate fall through to default case:
	}
    default:
	Rf_error(_("invalid subscript type '%s'"), subscripts->typeName());
    }
    return std::pair<const IntVector*, std::size_t>(0, 0); // -Wall
}

std::pair<const IntVector*, std::size_t>
Subscripting::canonicalize(const StringVector* raw_indices, std::size_t range_size,
			   const StringVector* range_names)
{
    const std::size_t rawsize = raw_indices->size();
    typedef std::tr1::unordered_map<GCRoot<String>, std::size_t> Nmap;
    Nmap names_map;
    std::size_t max_index = (range_names ? 0 : range_size);
    GCStackRoot<IntVector> ans(CXXR_NEW(IntVector(rawsize)));
    GCStackRoot<ListVector> use_names;  // For the use.names attribute
    // Process the supplied subscripts in order:
    for (std::size_t iraw = 0; iraw < rawsize; ++iraw) {
	String* subscript = (*raw_indices)[iraw];
	if (subscript == String::NA())
	    (*ans)[iraw] = NA<int>();
	else {
	    GCRoot<String> csubscript(SEXP_downcast<String*>(subscript));
	    // Coerce to UTF8 if necessary:
	    if (csubscript->encoding() != CE_UTF8) {
		RAllocStack::Scope scope;
		const char* utf8s = Rf_translateCharUTF8(csubscript);
		csubscript = String::obtain(utf8s, CE_UTF8);
	    }
	    // Have we met this name already?
	    Nmap::const_iterator it = names_map.find(csubscript);
	    if (it != names_map.end())  // Yes
		(*ans)[iraw] = int((*it).second);
	    else {  // No
		// If we haven't yet worked right through range_names,
		// continue searching for a match from where we left
		// off, in the process continuing to build a mapping
		// from names to indices:
		bool found = false;
		while (max_index < range_size && !found) {
		    String* name = (*range_names)[max_index++];
		    if (name != String::NA()) {
			GCRoot<String> cname(SEXP_downcast<String*>(name));
			if (cname != String::blank()) {
			    // Coerce to UTF8 if necessary:
			    if (cname->encoding() != CE_UTF8) {
				RAllocStack::Scope scope;
				const char* utf8s = Rf_translateCharUTF8(cname);
				cname = String::obtain(utf8s, CE_UTF8);
			    }
			    // Insert this name into names_map
			    // provided it isn't already there:
			    Nmap::const_iterator nmit = names_map.find(cname);
			    if (nmit == names_map.end())
				names_map[cname] = max_index;
			    found = (cname == csubscript);
			}
		    }
		}
		if (found) 
		    (*ans)[iraw] = int(max_index);
		else { // The supplied subscript is a new name:
		    if (!use_names)
			use_names = CXXR_NEW(ListVector(rawsize));
		    ++max_index;
		    (*use_names)[iraw] = csubscript;
		    (*ans)[iraw] = int(max_index);
		    names_map[csubscript] = max_index;
		}
	    }
	}
    }
    // Set up the use.names attribute if necessary:
    if (max_index > range_size)
	ans->setAttribute(UseNamesSymbol, use_names);
    return std::pair<const IntVector*, std::size_t>(ans, max_index);
}

const ListVector*
Subscripting::canonicalizeArraySubscripts(const VectorBase* v,
					  const PairList* subscripts)
{
    const IntVector* dims = v->dimensions();
    if (!dims)
	Rf_error(_ ("not a matrix/array"));
    std::size_t ndims = dims->size();
    const ListVector* dimnames = v->dimensionNames();
    GCStackRoot<ListVector> ans(CXXR_NEW(ListVector(ndims)));
    const PairList* pl = subscripts;
    for (std::size_t d = 0; d < ndims; ++d) {
	if (!pl)
	    Rf_error(_("too few subscripts"));
	std::size_t dimsize = std::size_t((*dims)[d]);
	const StringVector* names
	    = (dimnames ? static_cast<StringVector*>((*dimnames)[d].get()) : 0);
	std::pair<const IntVector*, std::size_t> pr
	    = canonicalize(pl->car(), dimsize, names);
	if (pr.second > dimsize)
	    Rf_error(_("subscript out of bounds"));
	// Since the return value is a const pointer, this const_cast
	// is tolerable:
	(*ans)[d] = const_cast<IntVector*>(pr.first);
	pl = pl->tail();
    }
    if (pl)
	Rf_error(_("too many subscripts"));
    return ans;
}
	
std::size_t Subscripting::createDimIndexers(DimIndexerVector* dimindexers,
				       const IntVector* source_dims,
				       const ListVector* indices)
{
    std::size_t ndims = source_dims->size();
    double dresultsize = 1.0;
    std::size_t resultsize = 1;
    for (std::size_t d = 0; d < ndims; ++d) {
	DimIndexer& di = (*dimindexers)[d];
	const IntVector* iv = static_cast<IntVector*>((*indices)[d].get());
	di.nindices = iv->size();
	dresultsize *= double(di.nindices);
	if (dresultsize > std::numeric_limits<size_t>::max())
	    Rf_error(_("dimensions would exceed maximum size of array"));
	resultsize *= di.nindices;
	di.indices = iv;
	di.indexnum = 0;
    }
    (*dimindexers)[0].stride = 1;
    for (std::size_t d = 1; d < ndims; ++d)
	(*dimindexers)[d].stride
	    = (*dimindexers)[d - 1].stride * std::size_t((*source_dims)[d - 1]);
    return resultsize;
}

bool Subscripting::dropDimensions(VectorBase* v)
{
    GCStackRoot<const IntVector> dims(v->dimensions());
    if (!dims)
	return false;
    std::size_t ndims = dims->size();
    // Count the number of dimensions with extent != 1 :
    std::size_t ngooddims = 0;
    for (std::size_t d = 0; d < ndims; ++d)
	if ((*dims)[d] != 1)
	    ++ngooddims;
    if (ngooddims == ndims)
	return false;
    GCStackRoot<ListVector> dimnames(const_cast<ListVector*>(v->dimensionNames()));
    v->setDimensionNames(0);
    if (ngooddims > 1) {
	// The result will still be an array/matrix.
	bool havenames = false;
	// Set up new dimensions attribute:
	{
	    GCStackRoot<IntVector> newdims(CXXR_NEW(IntVector(ngooddims)));
	    std::size_t dout = 0;
	    for (std::size_t din = 0; din < ndims; ++din) {
		std::size_t dsize = std::size_t((*dims)[din]);
		if (dsize != 1) {
		    (*newdims)[dout++] = int(dsize);
		    if (dimnames && (*dimnames)[din])
			havenames = true;
		}
	    }
	    v->setDimensions(newdims);
	}
	if (havenames) {
	    // Set up new dimnames attribute:
	    StringVector* dimnamesnames
		= const_cast<StringVector*>(dimnames->names());
	    GCStackRoot<ListVector> newdimnames(CXXR_NEW(ListVector(ngooddims)));
	    GCStackRoot<StringVector>
		newdimnamesnames(CXXR_NEW(StringVector(ngooddims)));
	    std::size_t dout = 0;
	    for (std::size_t din = 0; din < ndims; ++din)
		if ((*dims)[din] != 1) {
		    (*newdimnames)[dout] = (*dimnames)[din];
		    if (dimnamesnames)
			(*newdimnamesnames)[dout] = (*dimnamesnames)[din];
		    ++dout;
		}
	    if (dimnamesnames)
		newdimnames->setNames(newdimnamesnames);
	    v->setDimensionNames(newdimnames);
	}
    } else if (ngooddims == 1) {
	// Reduce to a vector.
	v->setDimensions(0);
	if (dimnames) {
	    std::size_t d = 0;
	    while ((*dims)[d] == 1)
		++d;
	    v->setNames(static_cast<StringVector*>((*dimnames)[d].get()));
	}
    } else /* ngooddims == 0 */ {
	v->setDimensions(0);
	v->setDimensionNames(0);
	// In this special case it is ambiguous which dimnames to use
	// for the sole remaining element, so we set up a name only if
	// just one dimension has names.
	if (dimnames) {
	    StringVector* newnames = 0;
	    std::size_t count = 0;
	    for (std::size_t d = 0; d < ndims; ++d) {
		RObject* dnd = (*dimnames)[d];
		if (dnd) {
		    newnames = static_cast<StringVector*>(dnd);
		    ++count;
		}
	    }
	    if (count == 1)
		v->setNames(newnames);
	}
    }
    return true;
}

void Subscripting::processUseNames(VectorBase* v, const IntVector* indices)
{
    const ListVector* usenames;
    {
	RObject* unmattr = indices->getAttribute(UseNamesSymbol);
	usenames = SEXP_downcast<const ListVector*>(unmattr);
    }
    if (!usenames)
	return;
    GCStackRoot<StringVector> newnames;
    {
	RObject* nmattr = v->getAttribute(NamesSymbol);
	if (nmattr)
	    newnames = SEXP_downcast<StringVector*>(nmattr);
	else newnames = CXXR_NEW(StringVector(v->size()));
    }
    for (std::size_t i = 0; i < usenames->size(); ++i) {
	RObject* newname = (*usenames)[i];
	if (newname) {
	    int index = (*indices)[i];
	    if (!isNA(index))
		(*newnames)[VectorBase::size_type(index) - 1] = SEXP_downcast<String*>(newname);
	}
    }
    v->setAttribute(NamesSymbol, newnames);
}

void Subscripting::setArrayAttributes(VectorBase* subset,
				      const VectorBase* source,
				      const DimIndexerVector& dimindexers,
				      bool drop)
{
    std::size_t ndims = source->dimensions()->size();
    // Dimensions:
    {
	GCStackRoot<IntVector> newdims(CXXR_NEW(IntVector(ndims)));
	for (std::size_t d = 0; d < ndims; ++d)
	    (*newdims)[d] = int(dimindexers[d].nindices);
	subset->setDimensions(newdims);
    }
    // Dimnames:
    {
	const ListVector* dimnames = source->dimensionNames();
	if (dimnames) {
	    GCStackRoot<ListVector> newdimnames(CXXR_NEW(ListVector(ndims)));
	    for (std::size_t d = 0; d < ndims; ++d) {
		const DimIndexer& di = dimindexers[d];
		// 0-length dims have NULL dimnames:
		if (di.nindices > 0) {
		    const StringVector* sv
			= static_cast<const StringVector*>((*dimnames)[d].get());
		    if (sv)
			(*newdimnames)[d] = vectorSubset(sv, di.indices);
		}
	    }
	    const StringVector* dimnamesnames = dimnames->names();
	    if (dimnamesnames)
		newdimnames->setNames(dimnamesnames->clone());
	    subset->setDimensionNames(newdimnames);
	}
    }
    if (drop)
	dropDimensions(subset);
}

void Subscripting::setVectorAttributes(VectorBase* subset,
				       const VectorBase* source,
				       const IntVector* indices)
{
    // Names:
    {
	const StringVector* sourcenames = source->names();
	if (!sourcenames) {
	    // Use row names if this is a one-dimensional array:
	    const ListVector* dimnames = source->dimensionNames();
	    if (dimnames && dimnames->size() == 1)
		sourcenames = static_cast<const StringVector*>((*dimnames)[0].get());
	}
	if (sourcenames)
	    subset->setNames(vectorSubset(sourcenames, indices));
    }
    // R_SrcrefSymbol:
    {
	RObject* attrib = source->getAttribute(SrcrefSymbol);
	if (attrib && attrib->sexptype() == VECSXP) {
	    const ListVector* srcrefs = static_cast<const ListVector*>(attrib);
	    subset->setAttribute(SrcrefSymbol, vectorSubset(srcrefs, indices));
	}
    }
}
