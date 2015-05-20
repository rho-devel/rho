/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file ExternalPointer.h
 * @brief Class CXXR::ExternalPointer and associated C interface.
 */

#ifndef EXTERNALPOINTER_H
#define EXTERNALPOINTER_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/SEXP_downcast.hpp"

namespace CXXR {
    /** @brief External pointer.
     *
     * RObject encapsulating a pointer to some entity that is not an
     * RObject.  An ExternalPointer also comprises two pointers to
     * objects of a type derived from RObject, here designated the
     * 'tag' and the 'protege', each of which is protected from
     * garbage collection for the lifetime of the ExternalPointer
     * object.  The tag and the protege are treated identically by the
     * ExternalPointer class, but the 'Writing R Extensions' document
     * (in recent revisions) suggests that the tag be used for some
     * sort of type identification, and that the protege be used for
     * protecting memory (or other resources) used by the entity
     * pointed to by the ExternalPointer.
     *
     * @note Writers of C++ packages are recommended to derive their
     * own classes from RObject rather than use this class, which is
     * provided primarily to support the established C interface.
     */
    class ExternalPointer : public RObject {
    public:
	/**
	 * @param ptr The pointer that the ExternalPointer object is
	 *          to encapsulate.
	 * @param tag Pointer to the tag object.  May be null (and
	 *          often is). 
	 * @param prot Pointer to the protege object.  May be null
	 *          (and often is).
	 */
	explicit ExternalPointer(void* ptr = nullptr, RObject* tag = nullptr,
				 RObject* prot = nullptr)
	    : RObject(EXTPTRSXP), m_ptr(ptr)
	{
	    m_tag = tag;
	    m_protege = prot;
	}

	/** @brief Get const pointer to protege object.
	 *
	 * @return a const pointer to the protege object of this
	 * ExternalPointer.
	 */
	const RObject* protege() const
	{
	    return m_protege;
	}

	/** @brief Get pointer to protege object.
	 *
	 * @return a pointer to the protege object of this
	 * ExternalPointer.
	 */
	RObject* protege()
	{
	    return m_protege;
	}

	/** @brief Get the encapsulated pointer, qualified by const.
	 *
	 * @return the encapsulated pointer, qualified by const.
	 */
	const void* ptr() const
	{
	    return m_ptr;
	}

	/** @brief Get the encapsulated pointer.
	 *
	 * @return the encapsulated pointer.
	 */
	void* ptr()
	{
	    return m_ptr;
	}

	/** @brief Designate the protege object.
	 *
	 * @param prot Pointer to the new protege object (or a null
	 *          pointer).
	 */
	void setProtege(RObject* prot)
	{
	    m_protege = prot;
	}

	/** @brief Set the value of the encapsulated pointer
	 *
	 * @param ptr New pointer value (may be null).
	 */
	void setPtr(void* ptr)
	{
	    m_ptr = ptr;
	}

	/** @brief Set the 'tag' value.
	 *
	 * @param tag Pointer to the new tag object (or a null
	 *           pointer).
	 */
	void setTag(RObject* tag)
	{
	    m_tag = tag;
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return the name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "externalptr";
	}

	/** @brief Get const pointer to tag object.
	 *
	 * @return a const pointer to the 'tag' of this ExternalPointer.
	 */
	const RObject* tag() const
	{
	    return m_tag;
	}

	/** @brief Get pointer to tag object.
	 *
	 * @return a pointer to the 'tag' of this ExternalPointer.
	 */
	RObject* tag()
	{
	    return m_tag;
	}

	// Virtual function of RObject:
	const char* typeName() const override;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const override;
    protected:
	// Declared protected to ensure that ExternalPointer objects are
	// allocated only using 'new':
	~ExternalPointer() {}

	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	void* m_ptr;
	GCEdge<> m_tag;
	GCEdge<> m_protege;

	// Not implemented yet.  Declared to prevent
	// compiler-generated versions:
	ExternalPointer(const ExternalPointer&);
	ExternalPointer& operator=(const ExternalPointer&);
    };
} // namespace CXXR

extern "C" {
#endif  /* __cplusplus */

    /** @brief Create a CXXR::ExternalPointer object.
     *
     * @param p The pointer that the CXXR::ExternalPointer object is
     *          to encapsulate.
     * @param tag Pointer to the tag object.  May be null (and
     *          often is). 
     * @param prot Pointer to the protege object.  May be null
     *          (and often is).
     *
     * @return Pointer to the created CXXR::ExternalPointer object.
     */
    SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot);

    /** @brief Get the encapsulated external pointer.
     *
     * @param s Pointer to a CXXR::ExternalPointer (checked).
     *
     * @return the external pointer encapsulated by \a s.
     */
#ifndef __cplusplus
    void *R_ExternalPtrAddr(SEXP s);
#else
    inline void *R_ExternalPtrAddr(SEXP s)
    {
	CXXR::ExternalPointer& ep
	    = *CXXR::SEXP_downcast<CXXR::ExternalPointer*>(s);
	return ep.ptr();
    }
#endif

    /** @brief Get pointer to tag object.
     *
     * @param s Pointer to a CXXR::ExternalPointer (checked).
     *
     * @return a pointer to the tag object of \a s.
     */
#ifndef __cplusplus  
SEXP R_ExternalPtrTag(SEXP s);
#else
inline SEXP R_ExternalPtrTag(SEXP s)
    {
	CXXR::ExternalPointer& ep
	    = *CXXR::SEXP_downcast<CXXR::ExternalPointer*>(s);
	return ep.tag();
    }
#endif

    /** @brief Get pointer to protege object.
     *
     * @param s Pointer to a CXXR::ExternalPointer (checked).
     *
     * @return a pointer to the protege object of \a s.
     */
#ifndef __cplusplus
    SEXP R_ExternalPtrProtected(SEXP s);
#else
    inline SEXP R_ExternalPtrProtected(SEXP s)
    {
	CXXR::ExternalPointer& ep
	    = *CXXR::SEXP_downcast<CXXR::ExternalPointer*>(s);
	return ep.protege();
    }
#endif

    /** @brief Set the value of the encapsulated pointer
     *
     * @param s Pointer to a CXXR::ExternalPointer (checked).
     *
     * @param p New pointer value (may be null).
     */
    void R_SetExternalPtrAddr(SEXP s, void *p);

    /** @brief Reset the encapsulated pointer to a null pointer.
     *
     * @param s Pointer to a CXXR::ExternalPointer (checked).
     */
#ifndef __cplusplus
    void R_ClearExternalPtr(SEXP s);
#else
    inline void R_ClearExternalPtr(SEXP s)
    {
	R_SetExternalPtrAddr(s, nullptr);
    }
#endif

    /** @brief Designate the tag object.
     *
     * @param s Pointer to a CXXR::ExternalPointer (checked).
     *
     * @param tag Pointer to the new tag object (or a null
     *          pointer).
     */
    void R_SetExternalPtrTag(SEXP s, SEXP tag);

    /** @brief Designate the protege object.
     *
     * @param s Pointer to a CXXR::ExternalPointer (checked).
     *
     * @param p Pointer to the new protege object (or a null
     *          pointer).
     */
    void R_SetExternalPtrProtected(SEXP s, SEXP p);

#ifdef __cplusplus
}
#endif

#endif /* EXTERNALPOINTER_H */
