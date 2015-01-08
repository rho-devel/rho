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

/*
 *  R : A Computer Language for Statistical Data Analysis
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/** @file GCNode_PtrS11n.hpp
 *
 * @brief Class GCNode::PtrS11n
 */

#ifndef GCNODE_PTRS11N_HPP
#define GCNODE_PTRS11N_HPP 1

#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/identity.hpp>

#include "CXXR/GCNode.hpp"
#include "CXXR/S11nScope.hpp"

namespace CXXR {
    /** @brief Serialization/deserialization of pointers to GCNode objects.
     *
     * This class, all of whose members are static, provides the
     * mechanism by which pointers to objects inheriting from GCNode
     * should be serialized and deserialized.
     *
     * boost::serialization includes mechanisms for ensuring that if,
     * before serialization, several pointers to be serialized point
     * to the same object of a class type, then after deserialization,
     * the deserialized pointers will also all point to a single
     * object.  During deserialization, boost::serialization creates
     * this object using the appropriate <tt>operator new</tt> when
     * the first pointer of the group is deserialized; subsequent
     * pointers of the group are then simply set to point to that
     * object.
     *
     * However, in the CXXR application of boost::serialization, there
     * is a complication in that by the time the previous user session
     * is deserialized, the interpreter will already have partially
     * established the GCNode-GCEdge graph.  It will, for example,
     * already have set up the base environment, and defined many
     * Symbol and BuiltInFunction objects.  It is necessary that the
     * GCNode graph read from an archive is integrated correctly with
     * this pre-existing graph.  It is important, for example, that
     * pointers that pointed to the NA string in the previous session
     * are deserialized to point to the NA string already established
     * in the new session prior to deserialization (whereas, left to
     * itself, boost::serialization would be apt to make them point to
     * a \e duplicate of that string read from the archive).
     *
     * The way CXXR handles this is that whenever a non-null pointer
     * to an object of a class inheriting from GCNode is deserialized,
     * class GCNode::PtrS11n (specifically the templated function
     * GCNode::PtrS11n::invoke()) will call the function
     * S11nScope::relocate() on the pointer value.  This function will
     * use a look-up table within the innermost S11nScope.  If this
     * function returns a null pointer, then this signifies that no
     * special handling is required, and the pointer being
     * deserialized is left unchanged from the value given to it by
     * boost deserialization.
     *
     * However, if S11nScope::relocate() returns a non-null pointer
     * <tt>p</tt>, this signifies that the pointer value yielded by
     * boost deserialization needs to be replaced by <tt>p</tt>.  For
     * example suppose that in the previous session a particular
     * pointer pointed to the NA string.  When that pointer is
     * deserialized, boost::deserialization will create a String
     * object <tt>s</tt> which is merely a \e proxy for the NA string;
     * however, invoking S11nScope::relocate() on the address of that
     * object will return a pointer to the new session's NA string.
     * GCNode::PtrS11n::invoke() will make the pointer being
     * deserialized point to that.
     *
     * Whenever one of these 'proxy' objects is deserialized, the
     * deserialization logic should create an appropriate entry in the
     * look-up table used by S11nScope::relocate() by calling
     * S11nScope::defineRelocation().  Once that is done, there is no
     * need to retain the proxy object, and S11nScope::defineRelocation() will
     * automatically expose it to garbage collection.
     *
     * Note that proxy objects may be malformed, i.e. violate the
     * invariants of their class: no use should be made of them.
     */
    class GCNode::PtrS11n {
    private:
	friend class GCNode;

	template <class Ptr>
	struct Payload {
	    typedef typename Ptr::type type;
	};

	template <class T>
	struct Payload<T*> {
	    typedef T type;
	};

	template<class Archive>
	struct Loader {
	    template <typename Ptr>
	    static void invoke(Archive& ar, Ptr& ptr, const char* name)
	    {
		GCNode* target;
		ar >> boost::serialization::make_nvp(name, target);
		if (target) {
		    GCNode* reloc = S11nScope::relocate(target);
		    if (reloc)
			target = reloc;
		    // Note that the target may already have been
		    // exposed, e.g. as a result of deserialising
		    // another pointer pointing to it.
		    else if (!target->isExposed())
			target->expose();
		}
		ptr = static_cast<typename Payload<Ptr>::type*>(target);
	    }
	};

	template<class Archive>
	struct Saver {
	    template <typename Ptr>
	    static void invoke(Archive& ar, const Ptr& ptr, const char* name)
	    {
		const GCNode* target
		    = static_cast<typename Payload<Ptr>::type*>(ptr);
		ar << boost::serialization::make_nvp(name, target);
	    }
	};
    public:
	/** @brief Serialize/deserialize a pointer to a GCNode object.
	 *
	 * See class description for details.
	 *
	 * @tparam Archive an archive class compatible with
	 *           boost::serialization.  Serialization or
	 *           deserialization will be performed according to
	 *           whether this is an output or an input archive.
	 *
	 * @tparam Ptr either a pointer type (possibly const) to
	 *           GCNode or a class inheriting from GCNode, or a
	 *           smart pointer encapsulating such a pointer.
	 *
	 *           If Ptr is a smart pointer, then it is required
	 *           that Ptr::type* define the type of the
	 *           encapsulated pointer, and that class Ptr allow
	 *           assignment to an from Ptr::type*.
	 *
	 * @param ar The archive to be read/written.
	 *
	 * @param ptr Reference to the pointer to be
	 *          serialized/deserialized.
	 *
	 * @param name The name (e.g. XML tag) to be attached to this
	 *          pointer within the archive.
	 */
	template<class Archive, typename Ptr>
	static void invoke(Archive& ar, Ptr& ptr, const char* name)
	{
	    using namespace boost::mpl;
	    typedef typename eval_if<typename Archive::is_saving,
		identity<Saver<Archive> >,
		identity<Loader<Archive> > >::type typex;
	    typex::invoke(ar, ptr, name);
	}
    };

} // namespace CXXR

/** @brief Syntactic sugar for
 *         GCNode::PtrS11n::invoke(<em>ar</em>, <em>name</em>, "<em>name</em>").
 */
#define GCNPTR_SERIALIZE(ar, name) \
	GCNode::PtrS11n::invoke(ar, name, #name )

#endif  // GCNODE_PTRS11N_HPP
