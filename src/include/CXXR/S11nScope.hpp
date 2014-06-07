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

/** @file S11nScope.hpp
 *
 * @brief Class S11nScope.
 */

#ifndef S11NSCOPE_HPP
#define S11NSCOPE_HPP 1

#include <map>
#include <boost/utility.hpp>

namespace CXXR {
    class GCNode;

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
    class S11nScope : public boost::noncopyable {
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
