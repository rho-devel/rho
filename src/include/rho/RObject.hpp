/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2007   The R Development Core Team.
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

/** @file RObject.h
 *
 * @brief Class rho::RObject and associated C interface functions.
 */

#ifndef ROBJECT_H
#define ROBJECT_H

#include "R_ext/Boolean.h"
#include "rho/SEXPTYPE.hpp"
#include "rho/GCEdge.hpp"
#include "rho/unrho.hpp"

/** @brief Namespace for the rho project.
 *
 * rho is a project to refactorize the R interpreter into C++.
 */
namespace rho {
    class Environment;
    class PairList;
    class Symbol;

    /** @brief Replacement for CR's SEXPREC.
     *
     * This class is the rough equivalent within rho of the SEXPREC
     * union within CR.  However, all functionality relating to
     * garbage collection has been factored out into the base class
     * GCNode, and as rho development proceeds other functionality
     * will be factored out into derived classes (corresponding
     * roughly, but not exactly, to different ::SEXPTYPE values within
     * CR), or outside the RObject hierarchy altogether.
     *
     * Eventually this class may end up simply as the home of R
     * attributes.
     *
     * @note The word 'object' in the name of this class is used in
     * the sense in which the 'blue book' (Becker <em>et al.</em>
     * [1988]) uses the phrase 'data object'.  Roughly speaking,
     * rho::RObject is a base class for the sorts of data items whose
     * existence would be reported by the R function
     * <tt>objects()</tt>.  In particular, it does not imply that
     * the object belongs to an R class.
     *
     * @invariant The class currently aims to enforce the following
     * invariants in regard to each RObject:
     * <ul>
     *
     * <li><tt>m_has_class</tt> is true iff the object has the class
     * attribute.</li>
     *
     * <li>Each attribute in the list of attributes must have a Symbol
     * as its tag.  Null tags are not allowed.</li>
     *
     * <li>Each attribute must have a distinct tag: no duplicates
     * allowed.</li>
     *
     * <li>No attribute may have a null value: an attempt to set the
     * value of an attribute to null will result in the removal of the
     * attribute altogether.
     * </ul>
     * The CR code in attrib.cpp applies further consistency
     * conditions on attributes, but these are not yet enforced via
     * the class interface.
     *
     * @par <tt>const RObject*</tt> policy:
     * There is an inherent tension between the way CR is implemented
     * and the 'const-correctness' that C++ programmers seek, and this
     * particularly arises in connection with pointers to objects of
     * classes derived from RObject.  CR accesses such objects
     * exclusively using ::SEXP, which is a non-const pointer.  (The
     * occasional use within the CR code of <tt>const SEXP</tt> is
     * misguided: the compiler interprets this in effect as
     * <tt>RObject* const</tt>, not as <tt>const RObject*</tt>.)  One
     * possible policy would be simply never to use <tt>const T*</tt>,
     * where \c T is \c RObject* or a class inheriting from it: that
     * would remove any need for <tt>const_cast</tt>s at the interface
     * between new rho code and code inherited from CR.  But rho
     * tries to move closer to C++ idiom than that, notwithstanding
     * the resulting need for <tt>const_cast</tt>s at the interface,
     * and applies a policy driven by the following considerations:
     * <ol>
     *
     * <li>RObject::evaluate() cannot return a <code>const
     * RObject*</code>, because some functions return a pointer to an
     * <code>Environment</code>, which may well need subsequently to
     * be modified e.g. by inserting or changing bindings.</li>
     *
     * <li>This in turn means that RObject::evaluate() cannot itself
     * be a <code>const</code> function, because the default
     * implementation returns <code>this</code>. (Another view would
     * be that the default implementation is an elided copy.)  Also,
     * Promise objects change internally when they are evaluated
     * (though this might conceivably be swept up by
     * <code>mutable</code>).</li>
     *
     * <li>It is a moot point whether FunctionBase::apply() can be
     * <code>const</code>.  Closure::apply() entails evaluating the
     * body, and if the body is regarded as part of the Closure
     * object, that would point to <code>apply()</code> not being
     * <code>const</code>. (Note that some of the types which
     * Rf_mkCLOSXP() accepts as a Closure body use the default
     * RObject::evaluate(), so Point&nbsp;2 definitely applies.)</li>
     *
     * <li>Should PairList objects and suchlike emulate (roughly
     * speaking) (a) <code>list&lt;pair&lt;const RObject*, const
     * RObject*&gt; &gt;</code> (where the first element of the pair
     * is the tag and the second the 'car'),
     * (b) <code>list&lt;pair&lt;const RObject*, RObject*&gt;
     * &gt;</code> or (c) <code>list&lt;pair&lt;RObject*, RObject*&gt;
     * &gt;</code> ? Since the 'cars' of list elements will often need
     * to be evaluated, Point&nbsp;2 rules out (a).  At present rho
     * follows (b).</li>
     *
     * <li>Since Symbol objects may well need to be evaluated,
     * Symbol::obtain() returns a non-const pointer; similarly,
     * String::obtain() returns a non-const pointer to a
     * String object.</li>
     * </ol>
     *
     * @todo Incorporate further attribute consistency checks within
     * the class interface.  Possibly make setAttribute() virtual so
     * that these consistency checks can be tailored according to the
     * derived class.
     */
    class RObject : public GCNode {
    public:
	/** @brief Class of function object that does nothing to an RObject.
	 *
	 * This struct is typically used as a default template
	 * parameter, for example in FixedVector.
	 */
	struct DoNothing {
	    /** @brief Does nothing.
	     */
	    static void initialize(RObject*)
	    {}
	};

	enum class Duplicate {
	    DEEP, SHALLOW
	};

	/** @brief Get object attributes.
	 *
	 * @return Pointer to the attributes of this object.
	 *
	 * @note Callers should beware that derived classes may
	 * override this function with one that gives rise to garbage
	 * collection.
	 */
	virtual const PairList* attributes() const;

	/** @brief Remove all attributes.
	 */
	virtual void clearAttributes();

	/** @brief Return pointer to a copy of this object.
	 *
	 * This function creates a copy of this object, and returns a
	 * pointer to that copy.
	 *
	 * Generally this function (and the copy constructors it
	 * utilises) will attempt to create a 'deep' copy of the
	 * object; this follows standard practice within C++, and it
	 * is intended to extend this practice as rho development
	 * continues.
	 *
	 * However, if the pattern object contains unclonable
	 * subobjects, then the created copy will at the relevant
	 * places simply contain pointers to those subobjects, i.e. to
	 * that extent the copy is 'shallow'.  This is managed using
	 * the smart pointers defined by nested class RObject::Handle.
	 *
	 * @return a pointer to a clone of this object.  Returns the original
	*     object if it cannot be cloned.
	 *
	 * @note Derived classes should exploit the covariant return
	 * type facility to return a pointer to the type of object
	 * being cloned.
	 */
	virtual RObject* clone() const {
	    return const_cast<RObject*>(this);
	}

	/** @brief Return a pointer to a copy of an object or the object itself
	 *    if it isn't cloneable.
	 *
	 * @tparam T RObject or a type derived from RObject.
	 *
	 * @param pattern Either a null pointer or a pointer to the
	 *          object to be cloned.
	 *
	 * @return Pointer to a clone of \a pattern, or \a pattern
	 * if \a pattern cannot be cloned or is itself a null pointer.
	 */
	template <class T>
	static T* clone(const T* pattern)
	{
	    return pattern ? pattern->clone() : nullptr;
	}

	/** @brief Copy an attribute from one RObject to another.
	 *
	 * @param name Non-null pointer to the Symbol naming the
	 *          attribute to be copied.
	 *
	 * @param source Non-null pointer to the object from which
	 *          the attribute are to be copied.  If \a source does
	 *          not have an attribute named \a name , then the
	 *          function has no effect.
	 */
	void copyAttribute(const Symbol* name, const RObject* source)
	{
	    RObject* att = source->getAttribute(name);
	    if (att)
		setAttribute(name, att);
	}

	/** @brief Copy attributes from one RObject to another.
	 *
	 * Any existing attributes of \a *this are discarded.
	 *
	 * @param source Non-null pointer to the object from which
	 *          attributes are to be copied.
	 *
	 * @param deep If deep, copy the full set of attributes.  Otherwise
	 *          do a shallow copy.
	 */
	void copyAttributes(const RObject* source, Duplicate depth);

	/** @brief Evaluate object in a specified Environment.
	 *
	 * @param env Pointer to the environment in which evaluation
	 *          is to take place.
	 *
	 * @return Pointer to the result of evaluation.
	 */
	virtual RObject* evaluate(Environment* env);

	/** @brief Get the value a particular attribute.
	 *
	 * @param name Pointer to a \c Symbol giving the name of the
	 *          sought attribute.
	 *
	 * @return pointer to the value of the attribute with \a name,
	 * or a null pointer if there is no such attribute.
	 *
	 * @note Implementers of derived classes should ensure that
	 * any function overriding this <em>will not</em> give rise to
	 * garbage collection.
	 */
	virtual RObject* getAttribute(const Symbol* name) const;

	/** @brief Has this object any attributes?
	 *
	 * @return true iff this object has any attributes.
	 *
	 * @note Implementers of derived classes should ensure that
	 * any function overriding this <em>will not</em> give rise to
	 * garbage collection.
	 */
	virtual bool hasAttributes() const
	{
	    return RObject::attributes() != nullptr;
	}

	/** @brief Has this object the class attribute?
	 *
	 * @return true iff this object has the class attribute.
	 */
	bool hasClass() const
	{
	    return m_type & s_class_mask;
	}

	/** @brief Is this an S4 object?
	 *
	 * @return true iff this is an S4 object.
	 */
	bool isS4Object() const
	{
	    return (m_type & s_S4_mask);
	}

	/** @brief Carry out memory tracing.
	 *
	 * This function is a no-op unless rho is built with
	 * R_MEMORY_PROFILING defined (as will happen if it is
	 * configured with --enable-memory-profiling).
	 *
	 * This function should be called if <tt>this</tt> has been
	 * created as a copy of \a src, or if <tt>this</tt> has been
	 * derived in some way from \a src1.  When memory profiling is
	 * enabled, if \a src points to an RObject with the
	 * memoryTraced() property set, this property will be
	 * propagated to <tt>this</tt>.  Also the creation of this
	 * object will be reported, along with the current context
	 * stack.
	 *
	 * @param src Non-null pointer to an RObject.
	 */
	void maybeTraceMemory(const RObject* src)
	{
#ifdef R_MEMORY_PROFILING
	    if (src->memoryTraced())
		traceMemory(src, 0, 0);
#endif
	}

	/** @brief Carry out memory tracing.
	 *
	 * This function is a no-op unless rho is built with
	 * R_MEMORY_PROFILING defined (as will happen if it is
	 * configured with --enable-memory-profiling).
	 *
	 * This function should be called if <tt>this</tt> has been
	 * derived in some way from \a src1 and \a src2.  When memory
	 * profiling is enabled, if either \a src1 or \a src2 points
	 * to an RObject with the memoryTraced() property set, this
	 * property will be propagated to <tt>this</tt>.  Also the
	 * creation of this object will be reported, along with the
	 * current context stack.
	 *
	 * @param src1 Non-null pointer to an RObject.
	 *
	 * @param src2 Non-null pointer to an RObject.
	 */
	void maybeTraceMemory(const RObject* src1,
			      const RObject* src2)
	{
#ifdef R_MEMORY_PROFILING
	    if (src1->memoryTraced() || src2->memoryTraced())
		traceMemory(src1, src2, 0);
#endif
	}

	/** @brief Carry out memory tracing.
	 *
	 * This function is a no-op unless rho is built with
	 * R_MEMORY_PROFILING defined (as will happen if it is
	 * configured with --enable-memory-profiling).
	 *
	 * This function should be called if <tt>this</tt> has been
	 * derived in some way from \a src1, \a src2 and \a src 3.  When
	 * memory profiling is enabled, if any of \a src1, \a src2 or
	 * \a src3 points to an RObject with the memoryTraced()
	 * property set, this property will be propagated to
	 * <tt>this</tt>.  Also the creation of this object will be
	 * reported, along with the current context stack.
	 *
	 * @param src1 Non-null pointer to an RObject.
	 *
	 * @param src2 Non-null pointer to an RObject.
	 *
	 * @param src3 Non-null pointer to an RObject.
	 */
	void maybeTraceMemory(const RObject* src1,
			      const RObject* src2,
			      const RObject* src3)
	{
#ifdef R_MEMORY_PROFILING
	    if (src1->memoryTraced()
		|| src2->memoryTraced()
		|| src3->memoryTraced())
		traceMemory(src1, src2, src3);
#endif
	}

	/** @brief Is copying etc. of this object being traced?
	 *
	 * The property reported by this function is used by R
	 * functions such as <tt>tracemem</tt>, and has effect only if
	 * rho is built with R_MEMORY_PROFILING defined (as will
	 * happen if it is configured with --enable-memory-profiling). 
	 *
	 * @return A return value of true signifies that when a copy
	 * is made of this object, or - more generally - some
	 * comparably sized object is derived from this object, this
	 * fact should be reported, and the 'memory traced' property
	 * propagated to the new object.
	 */
	bool memoryTraced() const
	{
	    return m_memory_traced;
	}

	/** @brief Reproduce the \c gp bits field used in CR.
	 *
	 * This function is used to reproduce the
	 * <tt>sxpinfo_struct.gp</tt> field used in CR.  It should be
	 * used exclusively for serialization.  Refer to the 'R
	 * Internals' document for details of this field.
	 *
	 * @return the reconstructed \c gp bits field (within the
	 * least significant 16 bits).
	 *
	 * @note If this function is overridden in a derived class,
	 * the overriding function should call packGPBits() for its
	 * immediate base class, and then 'or' further bits into the
	 * result.
	 */
	virtual unsigned int packGPBits() const;

	/** @brief Set or remove an attribute.
	 *
	 * @param name Pointer to the Symbol naming the attribute to
	 *          be set or removed.
	 *
	 * @param value Pointer to the value to be ascribed to the
	 *          attribute, or a null pointer if the attribute is
	 *          to be removed.  The object whose attribute is set
	 *          (i.e. <tt>this</tt>) should be considered to
	 *          assume ownership of \a value, which should
	 *          therefore not be subsequently altered externally.
	 */
	virtual void setAttribute(const Symbol* name, RObject* value);

	/** @brief Replace the attributes of an object.
	 *
	 * @param new_attributes Pointer to the start of the new list
	 *          of attributes.  May be a null pointer, in which
	 *          case all attributes are removed.  The object whose
	 *          attributes are set (i.e. <tt>this</tt>) should be
	 *          considered to assume ownership of the 'car' values
	 *          in \a new_attributes ; they should therefore not
	 *          be subsequently altered externally.
	 *
	 * @note The \a new_attributes list should conform to the
	 * class invariants.  However, attributes with null values are
	 * silently discarded, and if duplicate attributes are
	 * present, only the last one is heeded (and if the last
	 * setting has a null value, the attribute is removed altogether).
	 */
	void setAttributes(const PairList* new_attributes);

	/** @brief Enable/disable tracing of copying etc.
	 *
	 * The property set by this function is used by R functions
	 * such as <tt>tracemem</tt>, and has effect only if rho is
	 * built with R_MEMORY_PROFILING defined (as will happen if it
	 * is configured with --enable-memory-profiling).
	 *
	 * @param on A value of true signifies that when a copy
	 *          is made of this object, or - more generally - some
	 *          comparably sized object is derived from this
	 *          object, this fact should be reported, and the
	 *          'memory traced' property propagated to the new
	 *          object.
	 */	 
	void setMemoryTracing(bool on)
	{
	    m_memory_traced = on;
	}

	/** @brief Set the status of this RObject as an S4 object.
	 *
	 * @param on true iff this is to be considered an S4 object.
	 *          rho raises an error if an attempt is made to
	 *          unset the S4 object status of an S4Object
	 *          (::S4SXP), whereas CR (at least as of 2.7.2) permits
	 *          this.
	 */
	void setS4Object(bool on);

	/** @brief Get an object's ::SEXPTYPE.
	 *
	 * @return ::SEXPTYPE of this object.
	 */
	SEXPTYPE sexptype() const
	{
	    return SEXPTYPE(m_type & s_sexptype_mask);
	}

	/** @brief Name within R of this type of object.
	 *
	 * @return the name by which this type of object is known
	 *         within R.
	 */
	virtual const char* typeName() const;

	/** @brief Interpret the \c gp bits field used in CR.
	 *
	 * This function is used to interpret the
	 * <tt>sxpinfo_struct.gp</tt> field used in CR in a way
	 * appropriate to a particular node class.  It should be
	 * used exclusively for deserialization.  Refer to the 'R
	 * Internals' document for details of this field.
	 *
	 * @param gpbits the \c gp bits field (within the
	 *          least significant 16 bits).
	 *
	 * @note If this function is overridden in a derived class,
	 * the overriding function should also pass its argument to
	 * unpackGPBits() for its immediate base class.
	 */
	virtual void unpackGPBits(unsigned int gpbits);

	// Virtual functions of GCNode:
	void detachReferents() override
	{
	    m_attrib.detach();
	}

	void visitReferents(const_visitor* v) const override;
    protected:
	/**
	 * @param stype Required type of the RObject.
	 */
	explicit RObject(SEXPTYPE stype = CXXSXP)
	    : m_type(stype & s_sexptype_mask), m_named(0),
	      m_memory_traced(false), m_missing(0), m_argused(0),
	      m_active_binding(false), m_binding_locked(false)
	{}

	/** @brief Copy constructor.
	 *
	 * @param pattern Object to be copied.
	 */
	RObject(const RObject& pattern);

	virtual ~RObject() {}
    private:
	static const unsigned char s_sexptype_mask = 0x3f;
	static const unsigned char s_S4_mask = 0x40;
	static const unsigned char s_class_mask = 0x80;
	unsigned char m_type;  // The least-significant six bits hold
	  // the SEXPTYPE.  Bit 7 is set if the object has a
	  // class attribute.  Bit 6 is set to denote an S4 object.
    public:
	// To be private in future:
	unsigned char m_named;
    private:
	// The following field is used in connection with R functions
	// such as tracemem, and has effect only if rho is built with
	// R_MEMORY_PROFILING defined.  When set, it signifies that
	// when a copy is made of this object, or - more generally -
	// some comparably sized object is derived from this object,
	// this fact should be reported, and the m_memory_traced
	// property propagated to the new object.
	bool m_memory_traced : 1;
    public:
	// The following field is used only in connection with objects
	// inheriting from class ConsCell (and fairly rarely then), so
	// it would more logically be placed in that class (and
	// formerly was within rho).  It is placed here so that the
	// ubiquitous PairList objects can be squeezed into 32 bytes
	// (on 32-bit architecture), for improved cache efficiency.
	// This field is obsolescent in any case, and should be got
	// rid of entirely in due course:

	// 'Scratchpad' field used in handling argument lists,
	// formerly hosted in the 'gp' field of sxpinfo_struct.
	unsigned m_missing     : 2;
	
	// Similarly the following three obsolescent fields squeezed
	// in here are used only in connection with objects of class
	// PairList (and only rarely then), so they would more
	// logically be placed in that class (and formerly were within
	// rho).
	// 'Scratchpad' field used in handling argument lists,
	// formerly hosted in the 'gp' field of sxpinfo_struct.
	unsigned m_argused    : 2;

	// Used when the contents of an Environment are represented as
	// a PairList, for example during serialization and
	// deserialization, and formerly hosted in the gp field of
	// sxpinfo_struct.
	bool m_active_binding : 1;
	bool m_binding_locked : 1;
    private:
	GCEdge<PairList> m_attrib;

#ifdef R_MEMORY_PROFILING
	// This function implements maybeTraceMemory() (qv.) when
	// memory profiling is enabled.
	void traceMemory(const RObject* src1, const RObject* src2,
			 const RObject* src3);
#endif	
    };

    namespace ElementTraits {
	template<typename T>
	struct Duplicate<T*> {
	    T* operator()(T* value) const {
		return RObject::clone(value);
	    }
	};
    }  // namespace ElementTraits
}  // namespace rho

extern "C" {
    /** @brief Get the attributes of a rho::RObject.
     *
     * @param x Pointer to the rho::RObject whose attributes are required.
     *
     * @return Pointer to the attributes object of \a x , or 0 if \a x is
     * a null pointer.
     */
    SEXP ATTRIB(SEXP x);

    /** @brief Replace the attributes of \a to by those of \a from.
     *
     * The status of \a to as an S4 Object is also copied from \a from .
     * 
     * @param to Pointer to rho::RObject.
     *
     * @param from Pointer to another rho::RObject.
     */
    void DUPLICATE_ATTRIB(SEXP to, SEXP from);

    /** @brief Is this an S4 object?
     *
     * @param x Pointer to rho::RObject.
     *
     * @return true iff \a x is an S4 object.  Returns false if \a x
     * is 0.
     */
    inline Rboolean IS_S4_OBJECT(SEXP x)
    {
	return Rboolean(x && x->isS4Object());
    }

    /** @brief (For use only in serialization.)
     */
    inline int LEVELS(SEXP x) {return int(x->packGPBits());}

    /** @brief Get object copying status.
     *
     * @param x Pointer to rho::RObject.
     *
     * @return Refer to 'R Internals' document.  Returns 0 if \a x is a
     * null pointer.
     */
    inline int NAMED(SEXP x) {return x ? x->m_named : 0;}

    /** @brief Does an object have a class attribute?
     *
     * @param x Pointer to a rho::RObject.
     *
     * @return true iff \a x has a class attribute.  Returns false if \a x
     * is 0.
     */
    inline Rboolean OBJECT(SEXP x)
    {
	return Rboolean(x && x->hasClass());
    }

    /** @brief (For use only in deserialization.)
     */
    inline int SETLEVELS(SEXP x, int v)
    {
	x->unpackGPBits(static_cast<unsigned int>(v));
	return v;
    }

    /** @brief Replace an object's attributes.
     *
     * @param x Pointer to a rho::RObject.
     *
     * @param v Pointer to a PairList giving the new attributes of \a
     *          x.  \a x should be considered to assume ownership of
     *          the 'car' values in \a v ; they should therefore not
     *          be subsequently altered externally.
     *
     * @note Unlike CR, \a v isn't simply plugged into the attributes
     * field of \a x : refer to the documentation for \c
     * RObject::setAttributes() .  In particular, do not attempt to
     * modify the attributes by changing \a v \e after SET_ATTRIB
     * has been called.
     *
     * @note For compatibility with CR, garbage collection is
     * inhibited within this function.
     */
    void SET_ATTRIB(SEXP x, SEXP v);

    /** @brief Set object copying status.
     *
     * @param x Pointer to rho::RObject.  The function does nothing
     *          if \a x is a null pointer.
     *
     * @param v Refer to 'R Internals' document.
     *
     * @deprecated Ought to be private.
     */
    inline void SET_NAMED(SEXP x, int v)
    {
	if (!x) return;
	x->m_named = static_cast<unsigned char>(v);
    }

    /**
     * @deprecated Ought to be private.
     */
    inline void SET_S4_OBJECT(SEXP x)  {x->setS4Object(true);}

    /** @brief Get object's ::SEXPTYPE.
     *
     * @param x Pointer to rho::RObject.
     *
     * @return ::SEXPTYPE of \a x, or ::NILSXP if x is a null pointer.
     */
    inline SEXPTYPE TYPEOF(SEXP x)  {return x ? x->sexptype() : NILSXP;}

    /**
     * @deprecated Ought to be private.
     */
    inline void UNSET_S4_OBJECT(SEXP x)  {x->setS4Object(false);}

    /** @brief Copy attributes, with some exceptions.
     *
     * This is called in the case of binary operations to copy most
     * attributes from one of the input arguments to the output.
     * Note that the Dim, Dimnames and Names attributes are not
     * copied: these should have been assigned elsewhere.  The
     * function also copies the S4 object status.
     *
     * @param inp Pointer to the rho::RObject from which attributes are to
     *          be copied.
     *
     * @param ans Pointer to the rho::RObject to which attributes are to be
     *          copied.
     *
     * @note The above documentation is probably incomplete: refer to the
     *       source code for further details.
     */
    void Rf_copyMostAttrib(SEXP inp, SEXP ans);

    /** @brief Access a named attribute.
     *
     * @param vec Pointer to the rho::RObject whose attributes are to be
     *          accessed.
     *
     * @param name Either a pointer to the symbol representing the
     *          required attribute, or a pointer to a rho::StringVector
     *          containing the required symbol name as element 0; in
     *          the latter case, as a side effect, the corresponding
     *          symbol is installed if necessary.
     *
     * @return Pointer to the requested attribute, or a null pointer
     *         if there is no such attribute.
     *
     * @note The above documentation is incomplete: refer to the
     *       source code for further details.
     */
    SEXP Rf_getAttrib(SEXP vec, SEXP name);

    /** @brief Is this the null object pointer?
     *
     * @param s Pointer to a rho::RObject.
     *
     * @return TRUE iff the rho::RObject pointed to by \a s is either a null
     * pointer (i.e. <tt>== R_NilValue</tt> in rho), or is a rho::RObject
     * with ::SEXPTYPE ::NILSXP (should not happen in rho).
     */
    inline Rboolean Rf_isNull(SEXP s)
    {
	return Rboolean(!s || TYPEOF(s) == NILSXP);
    }

    /** @brief Does an object have a class attribute?
     *
     * @param s Pointer to a rho::RObject.
     *
     * @return TRUE iff the rho::RObject pointed to by \a s has a
     * class attribute.
     */
    inline Rboolean Rf_isObject(SEXP s)
    {
	return OBJECT(s);
    }

    /** @brief Set or remove a named attribute.
     *
     * @param vec Pointer to the rho::RObject whose attributes are to be
     *          modified.
     *
     * @param name Either a pointer to the symbol representing the
     *          required attribute, or a pointer to a rho::StringVector
     *          containing the required symbol name as element 0; in
     *          the latter case, as a side effect, the corresponding
     *          symbol is installed if necessary.
     *
     * @param val Either the value to which the attribute is to be
     *          set, or a null pointer.  In the latter case the
     *          attribute (if present) is removed.
     *
     * @return Refer to source code.  (Sometimes \a vec, sometimes \a
     * val, sometime a null pointer ...)
     *
     * @note The above documentation is probably incomplete: refer to the
     *       source code for further details.
     */
    SEXP Rf_setAttrib(SEXP vec, SEXP name, SEXP val);

    /** @brief C interface to RObject::traceMemory().
     *
     * This function provides a C language interface to
     * <tt>dest->maybeTraceMemory(src)</tt>: see the documentation of
     * that method for details.
     *
     * @param dest Non-null pointer to an RObject.
     *
     * @param src Non-null pointer to an RObject.
     */
    void maybeTraceMemory1(SEXP dest, SEXP src);

    /** @brief C interface to RObject::traceMemory().
     *
     * This function provides a C language interface to
     * <tt>dest->maybeTraceMemory(src1, src2)</tt>: see the documentation
     * of that method for details.
     *
     * @param dest Non-null pointer to an RObject.
     *
     * @param src1 Non-null pointer to an RObject.
     *
     * @param src2 Non-null pointer to an RObject.
     */
    void maybeTraceMemory2(SEXP dest, SEXP src1, SEXP src2);

    /** @brief Name of type within R.
     *
     * Translate a ::SEXPTYPE to the name by which it is known within R.
     *
     * @param st The ::SEXPTYPE whose name is required.
     *
     * @return The ::SEXPTYPE's name within R.
     */
    const char* Rf_type2char(SEXPTYPE st);
}

#endif /* ROBJECT_H */
