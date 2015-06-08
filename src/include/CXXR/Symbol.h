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

/** @file Symbol.h
 * @brief Class CXXR::Symbol and associated C interface.
 */

#ifndef RSYMBOL_H
#define RSYMBOL_H

#include "CXXR/RObject.h"

#ifdef __cplusplus

#include "CXXR/GCRoot.h"
#include "CXXR/SEXP_downcast.hpp"
#include "CXXR/String.h"

namespace CXXR {
    /** @brief Class used to represent R symbols.
     *
     * A Symbol is an R identifier.  Each Symbol (except for
     * pseudo-symbols, see below) has a name, namely a String giving the
     * textual representation of the identifier.  Generally speaking,
     * however, a Symbol object is identified by its address rather
     * than by its name.  Consequently, the class enforces the
     * invariant that there is a most one Symbol object with a given
     * name (but this does not apply to pseudo-symbols).
     *
     * Symbols come in two varieties, standard symbols and pseudo-symbols,
     * both implemented by this class.  Dot-dot symbols are a
     * subvariety of standard symbols.
     *
     * Standard symbols are generated using the static member function
     * obtain(), and (as explained above) have the property that there
     * is at most one standard symbol with a given name.  This is
     * enforced by an internal table mapping names to standard
     * symbols.
     *
     * Dot-dot symbols have names of the form '<tt>..</tt><i>n</i>',
     * where <i>n</i> is a positive integer.  These are preferably
     * generated using the static member function obtainDotDotSymbol()
     * (though they can also be generated using obtain() ), and are
     * used internally by the interpreter to refer to elements of a
     * '<tt>...</tt>' argument list.  (Note that CR does not
     * consistently enforce the 'at most one Symbol per name' rule for
     * dot-dot symbols; CXXR does.)
     *
     * Pseudo-symbols are used to implement certain pseudo-objects
     * (::R_MissingArg and ::R_UnboundValue) that CR expects to have
     * ::SEXPTYPE SYMSXP.  Each psuedo-symbol has a blank string as
     * its name, but despite this each of them is a distinct symbol.
     *
     * @note Following the practice with CR's symbol table, Symbol
     * objects, once created, are permanently preserved against
     * garbage collection.  There is no inherent reason for this in
     * CXXR, but some packages may rely on it.  Consequently there is
     * no need to use smart pointers such as GCStackRoot<Symbol> or
     * GCEdge<Symbol>: plain pointers will do fine.
     */
    class Symbol : public RObject {
    private:
	typedef std::vector<GCRoot<Symbol> > Table;
    public:
	/** @brief const_iterator for iterating over all standard Symbols.
	 *
	 * This is used in BuiltInSize() and BuiltInNames().
	 */
        typedef Table::const_iterator const_iterator;

	static const_iterator begin()
	{
            return getTable()->begin();
	}

	static const_iterator end()
	{
            return getTable()->end();
	}

	/** @brief Index of a double-dot symbol.
	 *
	 * @return If this is a Symbol whose name is of the form
	 * <tt>..<em>n</em></tt>, where <em>n</em> is a positive integer,
	 * returns <em>n</em>.  Otherwise returns <em>0</em>.
	 *
	 * @note This function returns 0 in the (pathological)
	 * case of a Symbol called <tt>..0</tt>.
	 */
	unsigned int dotDotIndex() const
	{
	    return m_dd_index;
	}

	/** @brief Is this a double-dot symbol?
	 *
	 * @return true iff this symbol relates to an element of a
	 *         <tt>...</tt> argument list.
	 */
	bool isDotDotSymbol() const
	{
	    return m_dd_index != 0;
	}

	/** @brief Is this a special symbol?
	 *
         * Special symbols are symbols that get special lookup optimizations.
         * Environments on the function call stack that
         * have never contained such a symbol are marked as such, so they can
         * be quickly skipped when searching for a function named by such a
         * special symbol.
         *
         * In principle, any symbol could be marked as special, but ones that
         * contain special characters, or are reserved words, are the ones
         * unlikely to be defined in any environment other than base, and hence
         * the ones where this is most likely to help.
         *
	 * @return true iff this symbol is marked as a special symbol.
	 */
        bool isSpecialSymbol() const
        {
          return m_is_special_symbol;
        }

	/** @brief Maximum length of symbol names.
	 *
	 * @return The maximum permitted length of symbol names.
	 */
	static size_t maxLength()
	{
	    return s_max_length;
	}

	/** @brief Missing argument.
	 *
	 * @return a pointer to the 'missing argument' pseudo-object.
	 */
	static Symbol* missingArgument();

	/** @brief Access name.
	 *
	 * @return const reference to the name of this Symbol.
	 */
	const String* name() const
	{
	    if (m_name)
		return m_name;
	    return String::blank();
	}

	/** @brief Get a pointer to a regular Symbol object.
	 *
	 * If no Symbol with the specified name currently exists, one
	 * will be created, and a pointer to it returned.  Otherwise a
	 * pointer to the existing Symbol will be returned.
	 *
	 * @param name The name of the required Symbol.
	 *
	 * @return Pointer to a Symbol (preexisting or newly
	 * created) with the required name.
	 */
	static Symbol* obtain(const String* name)
	{
	    return (name->m_symbol ? name->m_symbol : make(name));
	}

	/** @brief Get a pointer to a regular Symbol object.
	 *
	 * If no Symbol with the specified name currently exists, one
	 * will be created, and a pointer to it returned.  Otherwise a
	 * pointer to the existing Symbol will be returned.
	 *
	 * @param name The name of the required Symbol (CE_NATIVE
	 *          encoding is assumed).  At present no check is made
	 *          that the supplied string is a valid symbol name.
	 *
	 * @return Pointer to a Symbol (preexisting or newly
	 * created) with the required name.
	 */
	static Symbol* obtain(const std::string& name);

	/** @brief Get a pointer to a regular Symbol object.
	 *
	 * If no Symbol with the specified name currently exists, one
	 * will be created, and a pointer to it returned.  Otherwise a
	 * pointer to the existing Symbol will be returned.
	 *
	 * @param name The name of the required Symbol (CE_NATIVE
	 *          encoding is assumed).  At present no check is made
	 *          that the supplied string is a valid symbol name.
	 *
	 * @return Pointer to a Symbol (preexisting or newly
	 * created) with the required name.
	 */
	static Symbol* obtain(const String& name);

	/** @brief Create a double-dot symbol.
	 *
	 * @param n Index number of the required symbol; must be
	 *          strictly positive.
	 *
	 * @return a pointer to the created symbol, whose name will be
	 * <tt>..</tt><i>n</i>.
	 */
	static Symbol* obtainDotDotSymbol(unsigned int n);

	/** @brief Get a pointer to a Symbol for an S3 method.
	 *
	 * If no Symbol with the specified signature currently exists, one
	 * will be created, and a pointer to it returned.  Otherwise a
	 * pointer to the existing Symbol will be returned.
	 *
	 * @param className The name of the class that the method is for.
         * @param methodName The name of the function that the method is for.
	 *
	 * @return Pointer to a Symbol (preexisting or newly
	 * created) with the required signature.
	 */
        static Symbol* obtainS3Signature(const char* className,
                                         const char* methodName);

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "symbol";
	}

	/** @brief Unbound value.
	 *
	 * This is used as the 'value' of a Symbol that has not been
	 * assigned any actual value.
	 *
	 * @return a pointer to the 'unbound value' pseudo-object.
	 */
	static Symbol* unboundValue();

	// Virtual functions of RObject:
	RObject* evaluate(Environment* env) override;
	const char* typeName() const override;

	// Virtual function of GCNode:
	void visitReferents(const_visitor* v) const override;

        /** @brief Return a symbol object that has no name.
         *
         *  An unnamed is useful in places where an illegal symbol is
         *  useful.  Examples are missing arguments and unbound values.
         *
         *  Note that unlike other symbols, unnamed symbols are not
         *  automatically protected from garbage collection.
         */
        static Symbol* createUnnamedSymbol() {
          return new Symbol();
        }
    protected:
	// Virtual function of GCNode:
	void detachReferents() override;
    private:
	static const size_t s_max_length = 256;
	static Table* getTable();  // Vector of
	  // pointers to all Symbol objects in existence, other than
	  // psuedo-symbols and deserialization temporaries, used to
	  // protect them against garbage collection.

	GCEdge<const String> m_name;

	unsigned int m_dd_index : 31;
        bool m_is_special_symbol : 1;
	enum S11nType {NORMAL = 0, MISSINGARG, UNBOUNDVALUE};

	/**
	 * @param name Pointer to String object representing the name
	 *          of the symbol.  Names of the form
	 *          <tt>..<em>n</em></tt>, where n is a (non-negative)
	 *          decimal integer signify that the Symbol to be
	 *          constructed relates to an element of a
	 *          <tt>...</tt> argument list.  A null pointer
	 *          signifies a psuedo-symbol, which is not entered
         *          into the table.
	 */
	explicit Symbol(const String* name = nullptr);

	// Declared private to ensure that Symbol objects are
	// allocated only using 'new':
	~Symbol()
	{}

	// Not (yet) implemented.  Declared to prevent
	// compiler-generated versions:
	Symbol(const Symbol&);
	Symbol& operator=(const Symbol&);

	// Initialize the static data members:
	static void initialize();
        friend void ::Rf_InitNames();

        static const char* s_special_symbol_names[];

	// Precondition: there is not already a Symbol identified by
	// 'name'.
	//
	// Creates a new Symbol identified by 'name', enters it into
	// the table of standard Symbols, and returns a pointer to it.
	static Symbol* make(const String* name);
    };

    /** @brief Does Symbol's name start with '.'?
     *
     * @param symbol pointer to Symbol to be tested, or a null pointer
     *          in which case the function returns false.
     *
     * @return true if the Symbol's name starts with '.'.
     */
    inline bool isDotSymbol(const Symbol* symbol)
    {
	return symbol && symbol->name()->c_str()[0] == '.';
    }

    /** @brief Does Symbol's name start with '..'?
     *
     * @param symbol pointer to Symbol to be tested, or a null pointer
     *          in which case the function returns false.
     *
     * @return true if the Symbol's name starts with '..'.
     */
    inline bool isDotDotSymbol(const Symbol* symbol)
    {
	return symbol && symbol->isDotDotSymbol();
    }

    // Predefined Symbols visible in 'namespace CXXR':
#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    extern Symbol* CXXR_NAME;
#include "CXXR/PredefinedSymbols.h"
#undef PREDEFINED_SYMBOL

}  // namespace CXXR


extern "C" {
#endif /* __cplusplus */

    /* Pseudo-objects */
    extern SEXP R_MissingArg;
    extern SEXP R_UnboundValue;

    /* Symbol Table Shortcuts */
#define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
    extern SEXP C_NAME;
#include "CXXR/PredefinedSymbols.h"
#undef PREDEFINED_SYMBOL

    /** @brief Does symbol relate to a <tt>...</tt> expression?
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return \c TRUE iff this symbol denotes an element of a
     *         <tt>...</tt> expression.
     */
#ifndef __cplusplus
    Rboolean DDVAL(SEXP x);
#else
    inline Rboolean DDVAL(SEXP x)
    {
	using namespace CXXR;
	const Symbol& sym = *SEXP_downcast<Symbol*>(x);
	return Rboolean(sym.isDotDotSymbol());
    }
#endif

    /** Find value of a <tt>..<em>n</em></tt> Symbol.
     *
     * @param symbol Pointer to a Symbol (checked) whose name is of
     *          the form <tt>..<em>n</em></tt>, where <em>n</em> is a
     *          positive integer.
     *
     * @param rho Pointer to an Environment, which must bind the
     *          symbol <tt>...</tt> to a PairList comprising at least
     *          <em>n</em> elements.  (All checked.)
     *
     * @return The 'car' of the <em>n</em>th element of the PairList to
     * which <tt>...</tt> is bound.
     */
    SEXP Rf_ddfindVar(SEXP symbol, SEXP rho);

    /** @brief Get a pointer to a regular Symbol object.
     *
     * If no Symbol with the specified name currently exists, one will
     * be created, and a pointer to it returned.  Otherwise a pointer
     * to the existing Symbol will be returned.
     *
     * @param name The name of the required Symbol (CE_NATIVE encoding
     *          is assumed).
     *
     * @return Pointer to a Symbol (preexisting or newly created) with
     * the required name.
     */
#ifndef __cplusplus
    SEXP Rf_install(const char *name);
#else
    inline SEXP Rf_install(const char *name)
    {
	return CXXR::Symbol::obtain(name);
    }
#endif

    /** @brief Test if SYMSXP.
     *
     * @param s Pointer to a CXXR::RObject.
     *
     * @return TRUE iff s points to a CXXR::RObject with ::SEXPTYPE
     *         SYMSXP. 
     */
#ifndef __cplusplus
    Rboolean Rf_isSymbol(SEXP s);
#else
    inline Rboolean Rf_isSymbol(SEXP s)
    {
	return Rboolean(s && TYPEOF(s) == SYMSXP);
    }
#endif

    /** @brief Symbol name.
     *
     * @param x Pointer to a CXXR::Symbol (checked).
     *
     * @return Pointer to a CXXR::String representing \a x's name.
     */
#ifndef __cplusplus
    SEXP PRINTNAME(SEXP x);
#else
    inline SEXP PRINTNAME(SEXP x)
    {
	using namespace CXXR;
	const Symbol& sym = *SEXP_downcast<Symbol*>(x);
	return const_cast<String*>(sym.name());
    }
#endif

#ifdef __cplusplus
}
#endif

#endif /* RSYMBOL_H */
