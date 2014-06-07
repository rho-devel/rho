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
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file HeterogeneousList.hpp
 *
 * @brief Templated class CXXR::HeterogeneousList and its untemplated
 * base class HeterogeneousListBase.
 */

#ifndef HETEROGENEOUSLIST_HPP
#define HETEROGENEOUSLIST_HPP

#include <iterator>

namespace CXXR {
    template <class Node> class HeterogeneousList;

    /** @brief Untemplated base class for HeterogeneousList.
     */
    class HeterogeneousListBase {
    public:
	/** @brief Base class for nodes in any HeterogeneousList.
	 */
	class Link {
	public:
	    /** @brief Default constructor.
	     *
	     * Constructs a free Link (i.e. a Link not yet on any list).
	     */
	    Link()
		: m_prev(this), m_next(this)
	    {}

	    /** @brief Appending constructor
	     *
	     * @param list Non-null pointer to a list inheriting from
	     *          HeterogeneousListBase.  The constructed link
	     *          will be inserted at the end of \a list.
	     */
	    Link(HeterogeneousListBase* list)
		: m_prev(list->m_peg->m_prev), m_next(m_prev->m_next)
	    {
		m_prev->m_next = this;
		m_next->m_prev = this;
	    }

	    /** @brief Detach Link from any list.
	     *
	     * This function detaches the Link from any list it may
	     * currently be on, turning it back into a free Link.
	     */
	    void freeLink()
	    {
		m_prev->m_next = m_next;
		m_next->m_prev = m_prev;
		m_prev = this;
		m_next = this;
	    }
	protected:
	    friend class HeterogeneousListBase;

	    /** @note The destructor is protected to ensure that Link
	     * objects are allocated using 'new'.  (See Meyers 'More
	     * Effective C++' Item 27.) Derived classes should likewise
	     * declare their destructors private or protected.
	     */
	    virtual ~Link()
	    {
		m_prev->m_next = m_next;
		m_next->m_prev = m_prev;
	    }
	private:
	    Link* m_prev;
	    Link* m_next;
	};

	/** @brief Create an empty list.
	 */
	HeterogeneousListBase()
	    : m_peg(new Link)
	{}

	/** @brief Destructor.
	 *
	 * This deletes all objects remaining on the list. 
	 */
	~HeterogeneousListBase()
	{
	    clear();
	    delete m_peg;
	}

	/** @brief Delete all the objects on the list, leaving an
	 * empty list.
	 */
	void clear();

	/** @brief Is the list empty?
	 *
	 * @return true iff the list is empty.
	 */
	bool empty() const
	{
	    return m_peg->m_next == m_peg;
	}

	/** @brief Convert all objects on the list to free Links.
	 *
	 * This leaves the List empty.
	 */
	void freeLinks();
    protected:
	/** @brief Get link preceding a specified link.
	 *
	 * @param link Pointer to the link whose predecessor is desired.
	 *
	 * @return Pointer to the predecessor of \a link.
	 */
	static Link* predecessor(Link* link)
	{
	    return link->m_prev;
	}

	/** @brief Get link following a specified link.
	 *
	 * @param link Pointer to the link whose successor is desired.
	 *
	 * @return Pointer to the successor of \a link.
	 */
	static Link* successor(Link* link)
	{
	    return link->m_next;
	}
    private:
	template <class Node> friend class HeterogeneousList;

	Link* m_peg;  // dummy link representing end(); its successor
		// is begin().

	// Establish a bidirectional link with 'aft' as the successor
	// of 'fore':
	static void link(Link* fore, Link* aft)
	{
	    fore->m_next = aft;
	    aft->m_prev = fore;
	}

	// Move 'mover' from its current list location to just before 'pos':
	static void splice_link(Link* pos, Link* mover)
	{
	    link(mover->m_prev, mover->m_next);
	    link(pos->m_prev, mover);
	    link(mover, pos);
	}

	// Move all the nodes starting at 'from' up to but not
	// including 'to' so that they appear in order just before
	// 'pos'.  A no-op if from==to or pos==to.
	static void splice_links(Link* pos, Link* from, Link* to);

	// Move all the nodes on 'other' so that they appear in order
	// at the end of this list, leaving 'other' as an empty list.
	// A no-op if other == this.
	void splice_list(HeterogeneousListBase* other)
	{
	    splice_links(m_peg, other->m_peg->m_next, other->m_peg);
	}
    };

    /** @brief Doubly-linked list of objects derived from a class \a
     *  Node.
     *
     * @tparam Node Base class for object on the list.  \a Node must
     *           itself be derived from HeterogeneousListBase::Link.
     */
    template <class Node>
    class HeterogeneousList : public HeterogeneousListBase {
    public:
	/** @brief const_iterator for iterating over a HeterogeneousList.
	 */
	class const_iterator
	    : public std::iterator<std::bidirectional_iterator_tag,
				   const Node*>
	{
	public:
	    const Node* operator*() const
	    {
		return static_cast<const Node*>(m_link);
	    }

	    bool operator!=(const_iterator other) const
	    {
		return m_link != other.m_link;
	    }

	    const_iterator operator++()
	    {
		m_link = successor(m_link);
		return *this;
	    }
	private:
	    friend class HeterogeneousList<Node>;

	    Link* m_link;

	    explicit const_iterator(Link* link)
		: m_link(link)
	    {}
	};

	/** @brief Last element of list.
	 *
	 * @return A pointer to the last Node on the list.  Effect
	 * undefined if the list is empty.
	 */
	Node* back()
	{
	    return static_cast<Node*>(predecessor(m_peg));
	}

	/** @brief Last element of list (const variant).
	 *
	 * @return A pointer to the last Node on the list.  Effect
	 * undefined if the list is empty.
	 */
	const Node* back() const
	{
	    return static_cast<const Node*>(predecessor(m_peg));
	}

	const_iterator begin() const
	{
	    return const_iterator(successor(m_peg));
	}

	const_iterator end() const
	{
	    return const_iterator(m_peg);
	}

	/** @brief First element of list.
	 *
	 * @return A pointer to the first Node on the list.  Effect
	 * undefined if the list is empty.
	 */
	Node* front()
	{
	    return static_cast<Node*>(successor(m_peg));
	}

	/** @brief First element of list (const variant).
	 *
	 * @return A const pointer to the first Node on the list.  Effect
	 * undefined if the list is empty.
	 */
	const Node* front() const
	{
	    return static_cast<const Node*>(successor(m_peg));
	}

	/** @brief Move node to the end of this list.
	 *
	 * @param node Pointer to the node to be moved.  The node may
	 *          currently be on this list or another list, or be a
	 *          free Link.
	 */
	void splice_back(const Node* node)
	{
	    splice_link(m_peg, asLink(node));
	}

	/** @brief Move contents of another list to the back of this
	 *  list.
	 *
	 * @param other Pointer to the list whose contents are to be
	 *          moved.  The nodes of \a other are transferred,
	 *          preserving their order, to the back of this list,
	 *          leaving \a other as an empty list.  It is
	 *          permissible for \a other to point to this list
	 *          itself, in which case the function is a no-op.
	 */
	void splice_back(HeterogeneousList<Node>* other)
	{
	    splice_list(other);
	}
    private:
	// If C++ had mutable inheritance, we'd have Node mutably
	// inherit from Link.  But it doesn't, so instead we cast away
	// const as we upcast:
	Link* asLink(const Node* node)
	{
	    return const_cast<Node*>(node);
	}
    };
}  // namespace CXXR

#endif // HETEROGENEOUSLIST_HPP
