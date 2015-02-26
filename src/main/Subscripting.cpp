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

/** @file Subscripting.cpp
 *
 * Implementation of class CXXR::Subscripting and associated functions.
 */

#include <set>
#include <unordered_map>
#include "CXXR/RAllocStack.h"
#include "CXXR/RealVector.h"
#include "CXXR/Subscripting.hpp"

using namespace CXXR;

// ***** Class Subscripting::Indices *****

void Subscripting::Indices::applyNewNames(VectorBase* v) const
{
    if (!m_use_names)
	return;
    GCStackRoot<StringVector> newnames;
    {
	RObject* nmattr = v->getAttribute(NamesSymbol);
	if (nmattr)
	    newnames = SEXP_downcast<StringVector*>(nmattr);
	else newnames = StringVector::create(v->size());
    }
    for (std::size_t i = 0; i < m_use_names->size(); ++i) {
	RObject* newname = (*m_use_names)[i];
	if (newname) {
	    std::size_t index = (*this)[i];
	    if (index != 0)
		(*newnames)[index - 1] = SEXP_downcast<String*>(newname);
	}
    }
    v->setAttribute(NamesSymbol, newnames);
}    
    
void
Subscripting::Indices::initialize(const RObject* subscripts,
				  std::size_t range_size,
				  const StringVector* range_names)
{
    if (!subscripts)
	return;
    m_max_index = 0;
    switch (subscripts->sexptype()) {
    case LGLSXP:
	initialize(static_cast<const LogicalVector*>(subscripts), range_size);
	break;
    case INTSXP:
	initialize(static_cast<const IntVector*>(subscripts), range_size);
	break;
    case REALSXP:
	initialize(static_cast<const RealVector*>(subscripts), range_size);
	break;
    case STRSXP:
	initialize(static_cast<const StringVector*>(subscripts), range_size,
		   range_names);
	break;
    case SYMSXP:
	{
	    const Symbol* sym = static_cast<const Symbol*>(subscripts);
	    if (sym != Symbol::missingArgument())
		Rf_error(_("invalid subscript type '%s'"), sym->typeName());
	    // If a subscript argument is missing, we take this as
	    // meaning 'select everything':
	    resize(range_size);
	    m_max_index = range_size;
	    for (std::size_t i = 0; i < range_size; ++i)
		(*this)[i] = i + 1;
	}
	break;
    default:
	Rf_error(_("invalid subscript type '%s'"), subscripts->typeName());
    }
}

void Subscripting::Indices::initialize(const IntVector* raw_indices,
				       std::size_t range_size)
{
    const std::size_t rawsize = raw_indices->size();
    bool anyNA = false;
    bool anyneg = false;
    std::size_t zeroes = 0;
    m_max_index = 0;
    for (std::size_t i = 0; i < rawsize; ++i) {
	int index = (*raw_indices)[i];
	if (isNA(index))
	    anyNA = true;
	else if (index < 0)
	    anyneg = true;
	else if (index == 0)
	    ++zeroes;
	else if (index > int(m_max_index))
	    m_max_index = index;
    }
    if (!anyneg) {
	resize(rawsize - zeroes);
	std::size_t iout = 0;
	// Suppress zeroes, and replace NA by zero:
	for (std::size_t iin = 0; iin < rawsize; ++iin) {
	    int index = (*raw_indices)[iin];
	    if (index != 0)
		(*this)[iout++] = (isNA(index) ? 0 : index);
	}
	m_min_lhssize = m_max_index;
    } else {  // Negative subscripts
	if (anyNA || m_max_index > 0)
	    Rf_error(_("only 0's may be mixed with negative subscripts"));
	// Create a vector whose elements are non-zero for elements to
	// be retained:
	std::vector<char> retvec(range_size, 1);
	std::size_t removed = 0;
	for (std::size_t i = 0; i < rawsize; ++i) {
	    std::size_t index = std::size_t(-(*raw_indices)[i]);
	    if (index != 0 && index <= range_size) {
		char& elt = retvec[index - 1];
		if (elt != 0)
		    ++removed;
		elt = 0;
	    }
	}
	// Now set up vector and copy in the elements to be retained:
	resize(range_size - removed);
	m_max_index = 0;
	std::size_t iout = 0;
	for (std::size_t iin = 0; iin < range_size; ++iin)
	    if (retvec[iin]) {
		m_max_index = iin + 1;
		(*this)[iout++] = m_max_index;
	    }
	m_min_lhssize = range_size;
    }
}

void Subscripting::Indices::initialize(const RealVector* raw_indices,
				       std::size_t range_size)
{
    const std::size_t rawsize = raw_indices->size();
    bool anynonfinite = false;
    bool anyneg = false;
    std::size_t zeroes = 0;
    m_max_index = 0;
    for (std::size_t i = 0; i < rawsize; ++i) {
	double index = (*raw_indices)[i];
	if (!std::isfinite(index))
	    anynonfinite = true;
	else if (index < 0.0)
	    anyneg = true;
	else if (index == 0.0)
	    ++zeroes;
	else if (index > double(m_max_index))
	    m_max_index = std::size_t(index);
    }
    if (!anyneg) {
	resize(rawsize - zeroes);
	std::size_t iout = 0;
	// Suppress zeroes, and replace nonfinite values by zero:
	for (std::size_t iin = 0; iin < rawsize; ++iin) {
	    double index = (*raw_indices)[iin];
	    if (index != 0.0)
		(*this)[iout++]
		    = (std::isfinite(index) ? std::size_t(index) : 0);
	}
	m_min_lhssize = m_max_index;
    } else {  // Negative subscripts
	if (anynonfinite || m_max_index > 0)
	    Rf_error(_("only 0's may be mixed with negative subscripts"));
	// Create a vector whose elements are non-zero for elements to
	// be retained:
	std::vector<char> retvec(range_size, 1);
	std::size_t removed = 0;
	for (std::size_t i = 0; i < rawsize; ++i) {
	    std::size_t index = std::size_t(-(*raw_indices)[i]);
	    if (index != 0 && index <= range_size) {
		char& elt = retvec[index - 1];
		if (elt != 0)
		    ++removed;
		elt = 0;
	    }
	}
	// Now set up vector and copy in the elements to be retained:
	resize(range_size - removed);
	m_max_index = 0;
	std::size_t iout = 0;
	for (std::size_t iin = 0; iin < range_size; ++iin)
	    if (retvec[iin]) {
		m_max_index = iin + 1;
		(*this)[iout++] = m_max_index;
	    }
	m_min_lhssize = range_size;
    }
}

void Subscripting::Indices::initialize(const LogicalVector* raw_indices,
				       std::size_t range_size)
{
    const std::size_t rawsize = raw_indices->size();
    if (rawsize == 0)
	return;
    m_min_lhssize = std::max(range_size, rawsize);
    // Determine size of answer:
    std::size_t anssize = 0;
    for (std::size_t i = 0; i < m_min_lhssize; ++i)
    {
	Logical included = (*raw_indices)[i%rawsize];
	if (included.isTrue() || included.isNA())
	    ++anssize;
    }
    // Create Indices vector:
    resize(anssize);
    m_max_index = 0;
    std::size_t iout = 0;
    for (std::size_t iin = 0; iin < m_min_lhssize; ++iin) {
	Logical logical = (*raw_indices)[iin % rawsize];
	if (logical.isNA())
	    (*this)[iout++] = 0;
	else if (logical.isTrue()) {
	    m_max_index = iin + 1;
	    (*this)[iout++] = m_max_index;
	}
    }
}

void Subscripting::Indices::initialize(const StringVector* raw_indices,
				       std::size_t range_size,
				       const StringVector* range_names)
{
    const std::size_t rawsize = raw_indices->size();
    typedef std::unordered_map<GCRoot<String>, std::size_t> Nmap;

    Nmap names_map;
    m_max_index = (range_names ? 0 : range_size);
    resize(rawsize);
    // Process the supplied subscripts in order:
    for (std::size_t iraw = 0; iraw < rawsize; ++iraw) {
	String* subscript = (*raw_indices)[iraw];
	if (subscript == String::NA())
	    (*this)[iraw] = 0;
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
		(*this)[iraw] = (*it).second;
	    else {  // No
		// If we haven't yet worked right through range_names,
		// continue searching for a match from where we left
		// off, in the process continuing to build a mapping
		// from names to indices:
		bool found = false;
		while (m_max_index < range_size && !found) {
		    String* name = (*range_names)[m_max_index++];
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
				names_map[cname] = m_max_index;
			    found = (cname == csubscript);
			}
		    }
		}
		if (found) 
		    (*this)[iraw] = m_max_index;
		else { // The supplied subscript is a new name:
		    if (!m_use_names)
			m_use_names = ListVector::create(rawsize);
		    ++m_max_index;
		    (*m_use_names)[iraw] = csubscript;
		    (*this)[iraw] = m_max_index;
		    names_map[csubscript] = m_max_index;
		}
	    }
	}
    }
    m_min_lhssize = m_max_index;
}

// ***** Class Subscripting *****

std::pair<VectorBase*, std::size_t>
Subscripting::canonicalize(const RObject* raw_indices, std::size_t range_size,
			   const StringVector* range_names)
{
    Indices indices;
    indices.initialize(raw_indices, range_size, range_names);
    std::size_t maxindex = indices.maximumIndex();
    if (maxindex > std::size_t(std::numeric_limits<int>::max())) {
	GCStackRoot<RealVector> canvec(RealVector::create(indices.size()));
	for (std::size_t i = 0; i < indices.size(); ++i) {
	    std::size_t index = indices[i];
	    (*canvec)[i] = (index == 0 ? NA<double>() : double(index));
	}
	return std::make_pair(canvec.get(), maxindex);
    } else {
	GCStackRoot<IntVector> canvec(IntVector::create(indices.size()));
	for (std::size_t i = 0; i < indices.size(); ++i) {
	    std::size_t index = indices[i];
	    (*canvec)[i] = (index == 0 ? NA<int>() : int(index));
	}
	return std::make_pair(canvec.get(), maxindex);
    }
}
	
void
Subscripting::canonicalizeArraySubscripts(std::vector<Indices>* indicesvec,
					  const VectorBase* v,
					  const PairList* subscripts)
{
    const IntVector* dims = v->dimensions();
    if (!dims)
	Rf_error(_ ("not a matrix/array"));
    std::size_t ndims = dims->size();
    indicesvec->resize(ndims);
    const ListVector* dimnames = v->dimensionNames();
    const PairList* pl = subscripts;
    for (std::size_t d = 0; d < ndims; ++d) {
	if (!pl)
	    Rf_error(_("too few subscripts"));
	std::size_t dimsize = std::size_t((*dims)[d]);
	const StringVector* names
	    = (dimnames ? static_cast<StringVector*>((*dimnames)[d].get()) : nullptr);
	Indices& indices = (*indicesvec)[d];
        indices.initialize(pl->car(), dimsize, names);
	if (indices.maximumIndex() > dimsize)
	    Rf_error(_("subscript out of bounds"));
	pl = pl->tail();
    }
    if (pl)
	Rf_error(_("too many subscripts"));
}
	
std::size_t
Subscripting::createDimIndexers(DimIndexerVector* dimindexers,
				const IntVector* source_dims,
				const std::vector<Indices>& indicesvec)
{
    std::size_t ndims = source_dims->size();
    double dresultsize = 1.0;
    std::size_t resultsize = 1;
    for (std::size_t d = 0; d < ndims; ++d) {
	DimIndexer& di = (*dimindexers)[d];
	std::size_t dsz = indicesvec[d].size();
	dresultsize *= double(dsz);
	if (dresultsize > std::numeric_limits<size_t>::max())
	    Rf_error(_("dimensions would exceed maximum size of array"));
	resultsize *= dsz;
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
    v->setDimensionNames(nullptr);
    if (ngooddims > 1) {
	// The result will still be an array/matrix.
	bool havenames = false;
	// Set up new dimensions attribute:
	{
	    GCStackRoot<IntVector> newdims(IntVector::create(ngooddims));
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
	    GCStackRoot<ListVector> newdimnames(ListVector::create(ngooddims));
	    GCStackRoot<StringVector>
		newdimnamesnames(StringVector::create(ngooddims));
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
	v->setDimensions(nullptr);
	if (dimnames) {
	    std::size_t d = 0;
	    while ((*dims)[d] == 1)
		++d;
	    v->setNames(static_cast<StringVector*>((*dimnames)[d].get()));
	}
    } else /* ngooddims == 0 */ {
	v->setDimensions(nullptr);
	v->setDimensionNames(nullptr);
	// In this special case it is ambiguous which dimnames to use
	// for the sole remaining element, so we set up a name only if
	// just one dimension has names.
	if (dimnames) {
	    StringVector* newnames = nullptr;
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

void Subscripting::setArrayAttributes(VectorBase* subset,
				      const VectorBase* source,
				      const std::vector<Indices>& indicesvec,
				      bool drop)
{
    std::size_t ndims = source->dimensions()->size();
    // Dimensions:
    {
	// ***** FIXME *****: what if dim > MAX_INT?
	GCStackRoot<IntVector> newdims(IntVector::create(ndims));
	for (std::size_t d = 0; d < ndims; ++d)
	    (*newdims)[d] = int(indicesvec[d].size());
	subset->setDimensions(newdims);
    }
    // Dimnames:
    {
	const ListVector* dimnames = source->dimensionNames();
	if (dimnames) {
	    GCStackRoot<ListVector> newdimnames(ListVector::create(ndims));
	    for (std::size_t d = 0; d < ndims; ++d) {
		const Indices& indices = indicesvec[d];
		// 0-length dims have NULL dimnames:
		if (indices.size() > 0) {
		    const StringVector* sv
			= static_cast<const StringVector*>((*dimnames)[d].get());
		    if (sv)
			(*newdimnames)[d] = vectorSubset(sv, indices);
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
				       const Indices& indices)
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
