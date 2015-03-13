/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file RHandle.hpp
 *
 * @brief Class template CXXR::RHandle.
 */

#ifndef RHANDLE_HPP
#define RHANDLE_HPP

#include "CXXR/ElementTraits.hpp"
#include "CXXR/GCEdge.hpp"
#include "CXXR/RObject.h"

namespace CXXR {
    class RObject;

    /** @brief Smart pointer used to control the copying of RObjects.
     *
     * This class encapsulates a T* pointer, where T is derived from
     * RObject, and is used to manage the copying of subobjects when
     * an RObject is copied.  For most purposes, it behaves
     * essentially like a GCEdge<T>.  However, when a RHandle is
     * copied, it checks whether the object, \a x say, that it points
     * to is clonable.  If it is, then the copied RHandle will point to
     * a clone of \a x ; if not, then the copy will point to \a x
     * itself.
     *
     * @param T RObject or a class publicly derived from RObject.
     */
    template <class T = RObject>
    class RHandle : public GCEdge<T> {
    public:
	RHandle()
	{}

	// explicit RHandle(T* target) is intentionally not defined here.
	//
	// This prevents object initializers of the form
	//     Foo::Foo() : m_handle(expression_that_might_gc()) { ... }
	// In that case, the expression is executed while the handle is
	// uninitialized.  If it causes a garbage collection, the GC's mark
	// routine will attempt to follow the uninitialized handle, causing
	// errors.
	// Object initializers should be written as:
	//     Foo::Foo() { m_handle = expression_that_might_gc(); ... }
	// which properly initialized the handle prior to doing the allocation.


	// Most RHandles cannot be copied directly.  To duplicate a RHandle,
	// use:
	//     ElementTraits::duplicate_element(value);
	// This is done to ensure that all points where objects are duplicated
	// get explicitly noted and that no unexpected duplications occur.
	//
	// Note:
	//  The RHandle copy constructor and assignment operator are declared,
	//  but not implemented, except for RHandle<String>.
	RHandle(const RHandle<T>& pattern);
	RHandle<T>& operator=(const RHandle<T>& source);

	/** @brief Assignment from pointer.
	 *
	 * Note that this does not attempt to clone \a newtarget: it
	 * merely changes this RHandle to point to \a newtarget.
	 */
	RHandle<T>& operator=(T* newtarget)
	{
	    GCEdge<T>::operator=(newtarget);
	    return *this;
	}
    };  // class template RHandle

    // Partial specializations of ElementTraits:
    namespace ElementTraits {
	template <class T>
	struct MustConstruct<RHandle<T> > : boost::mpl::true_
	{};

	template <class T>
	struct MustDestruct<RHandle<T> >  : boost::mpl::true_
	{};

	template<typename T>
	struct Duplicate<RHandle<T>> {
		T* operator()(const RHandle<T>& value) const {
			return RObject::clone(value.get());
	    }
	};

	template <class T>
	struct Serialize<RHandle<T> > {
	    template <class Archive>
	    void operator()(Archive& ar, RHandle<T>& item)
	    {
		GCNPTR_SERIALIZE(ar, item);
	    }
	};

	template <class T>
	struct NAFunc<RHandle<T> > {
	    const RHandle<T>& operator()() const
	    {
		static RHandle<T> na;
		return na;
	    }
	};

	template <class T>
	struct IsNA<RHandle<T> > {
	    bool operator()(const RHandle<T>& t) const
	    {
		return false;
	    }
	};
    }  // namespace ElementTraits
}  // namespace CXXR

#endif // RHANDLE_HPP
