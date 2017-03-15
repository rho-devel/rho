/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *  Copyright (C) 2007-2014  Andrew R. Runnalls.
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

/** @file BuiltInFunction.h
 * @brief Class rho::BuiltInFunction and associated C interface.
 */

#ifndef BUILTINFUNCTION_H
#define BUILTINFUNCTION_H

#include <vector>

#include "rho/ArgList.hpp"
#include "rho/Environment.hpp"
#include "rho/Expression.hpp"
#include "rho/FunctionBase.hpp"
#include "sparsehash/dense_hash_map"

extern "C" {
    /** @brief The type of the do_xxxx functions.
     *
     * These are the built-in R functions.
     */
    typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);
}  // extern "C"

namespace rho {
    /** @brief R function implemented within the interpreter.
     *
     * A BuiltInFunction object represents an R function that is
     * implemented within the interpreter by a function in C++ or C.
     * These objects are of two kinds, according to whether the
     * arguments passed to BuiltInFunction::apply() are evaluated
     * before being passed on to the encapsulated C/C++ function (CR's
     * BUILTINSXP), or are passed on unevaluated (SPECIALSXP).
     */
    class BuiltInFunction : public FunctionBase {
    public:
	/** @brief Kind of function, used mainly in deparsing.
	 */
	enum Kind {
	    PP_INVALID  =  0,
	    PP_ASSIGN   =  1,
	    PP_ASSIGN2  =  2,
	    PP_BINARY   =  3,
	    PP_BINARY2  =  4,
	    PP_BREAK    =  5,
	    PP_CURLY    =  6,
	    PP_FOR      =  7,
	    PP_FUNCALL  =  8,
	    PP_FUNCTION =  9,
	    PP_IF 	= 10,
	    PP_NEXT 	= 11,
	    PP_PAREN    = 12,
	    PP_RETURN   = 13,
	    PP_SUBASS   = 14,
	    PP_SUBSET   = 15,
	    PP_WHILE 	= 16,
	    PP_UNARY 	= 17,
	    PP_DOLLAR 	= 18,
	    PP_FOREIGN 	= 19,
	    PP_REPEAT 	= 20
	};

	/** @brief Precedence level of function.
	 */
	enum Precedence {
	    PREC_FN	 = 0,
	    PREC_EQ	 = 1,
	    PREC_LEFT    = 2,
	    PREC_RIGHT	 = 3,
	    PREC_TILDE	 = 4,
	    PREC_OR	 = 5,
	    PREC_AND	 = 6,
	    PREC_NOT	 = 7,
	    PREC_COMPARE = 8,
	    PREC_SUM	 = 9,
	    PREC_PROD	 = 10,
	    PREC_PERCENT = 11,
	    PREC_COLON	 = 12,
	    PREC_SIGN	 = 13,
	    PREC_POWER	 = 14,
	    PREC_SUBSET	 = 15,
	    PREC_DOLLAR  = 16,
	    PREC_NS	 = 17
	};

	/** @brief 'Arity' of the function.
	 *
	 * @return The number of arguments expected by the function.
	 * A return value of -1 signifies that any number of arguments
	 * is acceptable.
	 */
	int arity() const
	{
            return m_arity;
	}

	/** @brief Report error if argument list is wrong length.
	 *
	 * This function raises an error if \a num_args is not a
	 * permissible length for all call to this BuiltInFunction.
	 *
	 * @param num_args The number of arguments that were passed.
	 *
	 * @param call The call being processed (for error reporting).
	 */
	void checkNumArgs(int num_args, const Expression* call) const {
	    checkNumArgs(num_args, arity(), call);
        }

	/** @brief Report error if argument list is wrong length.
	 *
	 * This function raises an error if \a num_args is not a
	 * permissible length for all call to this BuiltInFunction.
	 *
	 * @param num_args The number of arguments that were passed.
	 *
	 * @param arity The expected number of arguments.
	 *
	 * @param call The call being processed (for error reporting).
	 */
	void checkNumArgs(int num_args, int arity, const Expression* call)
	    const {
          if (num_args == arity || arity < 0) {
            return;
          }
          badArgumentCountError(num_args, arity, call);
        }

	/** @brief Kind of built-in function.
	 *
	 * (Used mainly in deparsing.)
	 *
	 * @return The Kind of the function.
	 */
	Kind kind() const
	{
	    return m_gram.kind;
	}

	/** @brief Name of function.
	 *
	 * @return The textual name of this function.
	 */
	const char* name() const
	{
	    return m_name.c_str();
	}

	/** @brief Get a pointer to a BuiltInFunction object.
	 *
	 * If \a symbol does not point to a built-in primitive this
	 * function will raise a warning and return a null pointer.
	 * Otherwise the function will return a pointer to the
	 * (unique) BuiltInFunction object embodying that function.
	 * If no such object already exists, one will be created.
	 *
	 * @param symbol Symbol pointing to the built-in function.
	 *
	 * @return a pointer to the BuiltInFunction object
	 * representing the function with the specified \a name, or a
	 * null pointer if \a name is not the name of a built-in
	 * primitive function.
	 *
	 * @note The reason this function raises a warning and not an
	 * error if passed an inappropriate \a name is so that loading
	 * an archive will not fail completely simply because it
	 * refers to an obsolete built-in function.
	 */
	static BuiltInFunction* obtainPrimitive(const Symbol* symbol);
	static BuiltInFunction* obtainPrimitive(const std::string& name);

        static void addPrimitivesToEnvironment(Environment* environment);

	/** @brief Get function accessed via <tt>.Internal()</tt>.
	 *
	 * @param sym Pointer to a Symbol.
	 *
	 * @return If \a x is associated with a function invoked in R \e
	 * via <tt>.Internal()</tt>, then a pointer to the appropriate
	 * rho::BuiltInFunction, otherwise a null pointer.
	 */
	static BuiltInFunction* obtainInternal(const Symbol* name);
	static BuiltInFunction* obtainInternal(const std::string& name) {
          return obtainInternal(Symbol::obtain(name));
        }

	/** @brief Get table offset.
	 *
	 * @return The offset into the table of functions.
	 */
	unsigned int offset() const
	{
	    return m_offset;
	}

	/** @brief Precedence of built-in function.
	 *
	 * @return The Precedence of the function.
	 */
	Precedence precedence() const
	{
	    return m_gram.precedence;
	}

	// SOFT_ON signifies that result printing should be enabled
	// before calling m_function, but that if m_function disables
	// result printing, this should not be overridden.
	enum ResultPrintingMode {FORCE_ON = 0, FORCE_OFF, SOFT_ON};

	/** @brief (Not for general use.)
	 *
	 * This function is used to implement PRIMPRINT, and is likely
	 * to be removed in future refactorisation.
	 *
	 * @return Code for handling \c R_Visible within the function,
	 *         as documented in names.cpp for the eval field of
	 *         the function table.
	 */
	ResultPrintingMode printHandling() const
	{
	    return m_result_printing_mode;
	}

	/** @brief Is a built-in function right-associative?
	 *
	 * @return true iff the function is right-associative.
	 */
	bool rightAssociative() const
	{
	    return m_gram.rightassoc;
	}

	/** @brief The names by which this type is known in R.
	 *
	 * @return The names by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "(builtin or special)";
	}

	/** @brief Index of variant behaviour.
	 *
	 * Where a single C/C++ function implements more than one
	 * built-in R function, this function provides the C/C++ code
	 * with an index indicating which R function is to be
	 * implemented.
	 *
	 * @return index of the required behaviour; interpretation is
	 * up to the C/C++ function called.
	 */
	unsigned int variant() const
	{
	    return m_variant;
	}

	/** @brief Must this function be called via .Internal()?
	 *
	 * @return true iff this function must be called via
	 * .Internal()?
	 */
	bool viaDotInternal() const
	{
	    return m_via_dot_internal;
	}

	/** @brief Does this function create a frame visible in traceback()?
	 */
	bool createsStackFrame() const
	{
	    return !m_transparent;
	}

	// Virtual function of RObject:
	const char* typeName() const override;

        // The different ways that the underlying function can take arguments.
        enum class CallingConvention {
          PairList, ArgumentArray, FixedNative
        };
        CallingConvention getCallingConvention() const {
          return m_calling_convention;
        }

	RObject* invoke(const Expression* call, Environment* env,
                        ArgList&& args) const
	{
	    // Handle internal generic functions.
            if (functionNeedsInternalDispatch()
                && argsHaveClassToDispatchOn(args))
	    {
                auto dispatched = RealInternalDispatch(call, env,
                                                       std::move(args));
		if (dispatched.first)
		    return dispatched.second;
	    }
            return invokeWithoutDispatch(call, env, args);
        }
      
	RObject* invokeWithoutDispatch(const Expression* call, Environment* env,
                                       const ArgList& args) const
        {
	    assert(m_calling_convention == CallingConvention::PairList);
            return callBuiltInWithCApi(m_function.pairlist, call, this, args,
                                       env);
        }

        RObject* invoke(const Expression* call, Environment* env,
                        RObject* const* args, int num_args,
                        const PairList* tags) const {
	    // Handle internal generic functions.
            if (functionNeedsInternalDispatch()
                && argsHaveClassToDispatchOn(args, num_args))
	    {
		auto dispatched = RealInternalDispatch(call, num_args, args,
						       tags, env);
		if (dispatched.first)
		    return dispatched.second;
	    }
            return invokeWithoutDispatch(call, env, args, num_args, tags);
        }
      
        RObject* invokeWithoutDispatch(const Expression* call, Environment* env,
                                       RObject* const* args, int num_args,
                                       const PairList* tags) const
        {
            assert(m_calling_convention == CallingConvention::ArgumentArray);
            return m_function.arg_array(const_cast<Expression*>(call),
                                        this, env, args, num_args, tags);
        }

	template<typename... Args>
        RObject* invokeFixedArity(const Expression* call,
				  Environment* env,
				  const PairList* tags,
				  Args... args) const {
            assert(m_calling_convention == CallingConvention::FixedNative);
            assert(sizeof...(Args) == arity());
            if (functionNeedsInternalDispatch()
                && argsHaveClassToDispatchOn(args...))
	    {
		std::initializer_list<RObject*> args_array = { args... };
		auto dispatched = RealInternalDispatch(
		    call, args_array.size(), args_array.begin(), tags, env);
		if (dispatched.first)
		    return dispatched.second;
	    }
            return invokeFixedArityWithoutDispatch(call, env, tags, args...);
        }
      
	template<typename... Args>
        RObject* invokeFixedArityWithoutDispatch(const Expression* call,
                                                 Environment* env,
                                                 const PairList* tags,
                                                 Args... args) const
        {
            assert(m_calling_convention == CallingConvention::FixedNative);
            assert(sizeof...(Args) == arity());
	    auto fn = reinterpret_cast<
		RObject*(*)(Expression*, const BuiltInFunction*,
			    Args...)>(m_function.fixed_native);
	    return (*fn)(const_cast<Expression*>(call), this, args...);
	}

	RObject* invokeSpecial(const Expression* call, Environment* env,
                               const PairList* args) const {
          assert(sexptype() == SPECIALSXP);
          assert(getCallingConvention() == CallingConvention::PairList);
          assert(!functionNeedsInternalDispatch());
          return (*m_function.pairlist)(const_cast<Expression*>(call),
                                        const_cast<BuiltInFunction*>(this),
                                        const_cast<PairList*>(args),
                                        env);
        }

            const char* getFirstArgName() const {
	    return m_first_arg_name;
	}

        static SEXP callBuiltInWithCApi(CCODE builtin,
                                        const Expression* call,
                                        const FunctionBase* op,
                                        const ArgList& args,
                                        Environment* env);
    private:
	// The type of internal dispatch (if any) that the function does..
	enum class DispatchType {
	    NONE, INTERNAL,
	    GROUP_MATH, GROUP_OPS, GROUP_COMPLEX, GROUP_SUMMARY };

    public:
        bool functionNeedsInternalDispatch() const {
            static BuiltInFunction* length_fn
                = BuiltInFunction::obtainPrimitive("length");
            if (isInternalGeneric()
                // Specials, the summary group and length handle dispatch
                // themselves.
                && !isSummaryGroupGeneric()
                && sexptype() == BUILTINSXP
                && this != length_fn)
                return true;
            return false;
	}

	/* @brief Attempt to dispatch this call to a method.
         *
	 * @param call The call being dispatched.
	 * @param env The environment to call the method from.
	 * @param evaluated_args The arguments to pass to the method.
	 *
	 * @return The first value is whether or not the dispatch succeeded.
	 *    The second value is the return value if it did.
         *
         * @note If the dispatch fails, then evaluated_args is unchanged.
	 */
        std::pair<bool, RObject*>
        InternalDispatch(const Expression* call,
			 Environment* env,
			 ArgList&& evaluated_args) const
        {
            assert(evaluated_args.status() == ArgList::EVALUATED);
  
            // NB: functionNeedsInternalDispatch() might not be true, since
            //   we could be doing dispatch for a special etc.
            assert(m_dispatch_type != DispatchType::NONE);
            if (argsHaveClassToDispatchOn(evaluated_args)) {
                return RealInternalDispatch(call, env,
                                            std::move(evaluated_args));
            }
            return std::make_pair(false, nullptr);
        }

    private:
	const char* GetInternalGroupDispatchName() const;

	// Alternative C function.  This differs from CCODE primarily in
	// that the arguments are passed in an array instead of a linked
	// list.
        typedef RObject*(*ArgumentArrayFn)(/*const*/ Expression* call,
                                           const BuiltInFunction* op,
                                           Environment* env,
                                           RObject* const* args,
                                           int num_args,
                                           const PairList* tags);

	// Placeholder for fuctions that takes the arguments as normal C
	// function arguments.
	// This must be cast to the correct type before calling.
	typedef RObject* (*FixedNativeFnStorage)(Expression*,
                                                 const BuiltInFunction*,
                                                 class SequenceOfRObject*);

	// 'Pretty-print' information:
	struct PPinfo {
	    Kind kind;
	    Precedence precedence;
	    unsigned int rightassoc;
	};

	BuiltInFunction(const char*,
			CCODE,
			unsigned int,
			unsigned int,
			int,
			PPinfo,
			const char* first_arg_name = nullptr,
			DispatchType dispatch = DispatchType::NONE);
	BuiltInFunction(const char*,
			ArgumentArrayFn,
			unsigned int,
			unsigned int,
			int,
			PPinfo,
			const char* first_arg_name = nullptr,
			DispatchType dispatch = DispatchType::NONE);

	template<typename... Args>
	BuiltInFunction(const char* name,
			RObject* (*cfun)(Expression*, const BuiltInFunction*,
					 Args...),
			unsigned int variant,
			unsigned int flags,
			int arity,
			PPinfo ppinfo,
			const char* first_arg_name = nullptr,
			DispatchType dispatch = DispatchType::NONE)
	    : BuiltInFunction(name, reinterpret_cast<FixedNativeFnStorage>(cfun),
			      variant, flags, arity, ppinfo,
			      first_arg_name, dispatch) {
	    assert(arity == sizeof...(Args));
	};

	BuiltInFunction(const char* name,
			FixedNativeFnStorage cfun,
			unsigned int variant,
			unsigned int flags,
			int arity,
			PPinfo ppinfo,
			const char* first_arg_name = nullptr,
			DispatchType dispatch = DispatchType::NONE);
	BuiltInFunction(const char*,
			unsigned int,
			unsigned int,
			int,
			PPinfo,
			const char* first_arg_name = nullptr,
			DispatchType dispatch = DispatchType::NONE);

	static unsigned int s_next_offset;

	static const std::vector<BuiltInFunction*>& getFunctionTable();

	typedef google::dense_hash_map<const Symbol*, GCRoot<BuiltInFunction>> map;
        static std::pair<map*, map*> getLookupTables();
        static std::pair<map*, map*> createLookupTables();
	// Mapping from function names to pointers to BuiltInFunction
	// objects for functions called via .Primitive()
	static map* getPrimitiveFunctionLookupTable();
        // And for functions called via .Internal()
        static map* getInternalFunctionLookupTable();

	unsigned int m_offset;
        CallingConvention m_calling_convention;
        union {
          CCODE pairlist;
          ArgumentArrayFn arg_array;
          FixedNativeFnStorage fixed_native;
        } m_function;

	std::string m_name;  // name of function
	unsigned int m_variant;  // used to select alternative
	                         // behaviours within the do_xxx
	    			 // function
	ResultPrintingMode m_result_printing_mode;
	bool m_transparent;  // if true, do not create a
			     // FunctionContext when this function is
			     // applied.
	bool m_via_dot_internal; // Must this function be called via .Internal?
	int m_arity;         // function arity; -1 means 'any'
	const char* m_first_arg_name;
	DispatchType m_dispatch_type;  // Type of internal dispatch to use.
	PPinfo m_gram;       // 'pretty-print' information

	// Declared private to ensure that BuiltInFunction objects are
	// allocated only using 'new'.
	~BuiltInFunction();

	bool isInternalGeneric() const {
	    return m_dispatch_type != DispatchType::NONE;
	}

	bool isSummaryGroupGeneric() const {
	    return m_dispatch_type == DispatchType::GROUP_SUMMARY;
	}

        bool argsHaveClassToDispatchOn(RObject* const* evaluated_args,
                                       int num_args) const
        {
	    switch(num_args) {
	    case 0:
		return false;
	    case 1:
		return argsHaveClassToDispatchOn(evaluated_args[0]);
	    default:
		return argsHaveClassToDispatchOn(evaluated_args[0],
						 evaluated_args[1]);
	    }
	}

	bool argsHaveClassToDispatchOn(const ArgList& evaluated_args) const
	{
	    switch(evaluated_args.size()) {
	    case 0:
		return false;
	    case 1:
                return argsHaveClassToDispatchOn(evaluated_args.get(0));
	    default:
                return argsHaveClassToDispatchOn(evaluated_args.get(0),
                                                 evaluated_args.get(1));
	    }
	}

	// Internal dispatch is used a lot, but there most of the time
	// no dispatch is needed because no objects are involved.
	// This quickly detects those cases.
	bool argsHaveClassToDispatchOn(const RObject* arg1 = nullptr,
                                       const RObject* arg2 = nullptr,
                                       ...) const
	{
	    assert(m_dispatch_type != DispatchType::NONE);
	    if (arg1 && arg1->hasClass())
		return true;
	    // 'Ops' dispatch on the second argument as well.
	    if (m_dispatch_type == DispatchType::GROUP_OPS
		&& arg2 && arg2->hasClass())
		return true;
	    return false;
	}

        std::pair<bool, RObject*>
        RealInternalDispatch(const Expression* call,
                             Environment* env,
                             ArgList&& args) const;

        std::pair<bool, RObject*>
        RealInternalDispatch(const Expression* call,
                             int num_args, RObject* const* evaluated_args,
                             const PairList* tags,
                             Environment* env) const;

	/** @brief Raise error because of missing argument.
	 *
	 * @param func Pointer, possibly null, to the BuiltInFunction
	 *          being evaluated.
	 *
	 * @param args The list of arguments.
	 *
	 * @param index Position (counting from one) of the first
	 * missing argument.
	 */
	static void missingArgumentError(const BuiltInFunction* func,
					 const PairList* args,
					 unsigned int index);

	/** @brief Raise error because a bad number of arguments.
	 *
	 * @param num_args The number of args passed.
	 *
	 * @param arity The expected number of args passed.

	 * @param call The call.
	 */
        void badArgumentCountError(int num_args, int arity,
				   const Expression* call) const;
    };
}  // namespace rho

// Old-style accessor functions.  Get rid of these in due course.

extern "C" {
inline const char* PRIMNAME(SEXP x)
{
    using namespace rho;
    BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
    return bif.name();
}
  
    /** @brief Get offset of a rho::BuiltInFunction.
     *
     * @param x Pointer to a rho::BuiltInFunction.
     *
     * @return The offset of this function within the function table.
     */
    inline int PRIMOFFSET(SEXP x)
    {
	using namespace rho;
	BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
	return int(bif.offset());
    }

inline unsigned int PRIMVAL(SEXP x)
{
    using namespace rho;
    BuiltInFunction& bif = *SEXP_downcast<BuiltInFunction*>(x);
    return bif.variant();
}
  
}

#endif /* BUILTINFUNCTION_H */
