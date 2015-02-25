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

/** @file S11nScope.hpp
 *
 * @brief Class S11nScope.
 */

#ifndef S11NSCOPE_HPP
#define S11NSCOPE_HPP 1

#include <map>
#include <boost/utility.hpp>
#include "CXXR/GCNode.hpp"

namespace CXXR {
    /** @brief Class providing supplementary information for serialization.
     *
     * This class is used to provide supplementary information to
     * control the behaviour of deserialization.  It is mandatory that
     * any use of boost::serialization to deserialize GCNode objects
     * takes place within the lifetime of a S11nScope object.
     *
     * One function of this class is to manage relocation during
     * deserialization, as described for class GCNode::PtrS11n .  This
     * is achieved using the functions defineRelocation() and
     * relocate().
     *
     * S11nScope objects are intended to be allocated on the processor
     * stack: specifically, the class implementation requires that
     * S11nScope objects are destroyed in the reverse order of
     * creation, and the destructor checks this.
     */
    class S11nScope : public GCNode::GCInhibitor {
    public:
	/** @brief Primary constructor.
	 */
	S11nScope();

	~S11nScope();

	/** @brief Define a deserialization relocation.
	 *
	 * This function defines a deserialization relocation within
	 * the innermost S11nScope.
	 *
	 * @param from Non-null pointer value to be relocated.
	 *
	 * @param to Non-null pointer value to which \a from is to be mapped.
	 */
	static void defineRelocation(GCNode* from, GCNode* to);

	/** @brief Apply defined relocations.
	 *
	 * @param from Pointer possibly to be relocated.
	 *
	 * @return If a relocation of \a from has been defined within
	 * the innermost S11nScope, returns the pointer value to which
	 * \a from is mapped.  Otherwise returns a null pointer.
	 */
	static GCNode* relocate(GCNode* from);
    private:
	static S11nScope* s_innermost;

	S11nScope* m_next;

	std::map<GCNode*, GCNode*> m_relocations;

	// Report out-of-sequence destructor call and abort program.
	// (We can't use an exception here because it's called from a
	// destructor.)
	static void seqError();
    };
}  // namespace CXXR

#endif  // S11NSCOPE_HPP
