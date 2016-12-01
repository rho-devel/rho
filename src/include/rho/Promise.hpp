/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
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

/** @file Promise.h
 * @brief Class rho::Promise and associated C interface.
 */

#ifndef RPROMISE_H
#define RPROMISE_H

#include "rho/RObject.hpp"
#include "rho/RObject.hpp"
#include "rho/Symbol.hpp"

extern "C"
void SET_PRVALUE(SEXP x, SEXP v);

namespace rho {
    class Environment;
    class Promise;

    /** @brief Mechanism for deferred evaluation.
     *
     * This class is used to handle function arguments within R's lazy
     * evaluation scheme.  A Promise object encapsulates a pointer to
     * an arbitrary RObject (typically a Symbol or an Expression), and
     * a pointer to an Environment.  When the Promise is first
     * evaluated, the RObject is evaluated within the Environment, and
     * the result of evaluation returned as the value of the Promise.
     *
     * After the first evaluation, the result of evaluation is cached
     * within the Promise object, and the Environment pointer is set
     * null (thus possibly allowing the Environment to be
     * garbage-collected).  Subsequent evaluations of the Promise
     * object simply return the cached value.
     */
    class PromiseData {
    public:
	/**
	 * @param valgen pointer to RObject to be evaluated to provide
	 *          the value of the Promise.  Can be null.
	 *
	 * @param env pointer to the Environment in which \a valgen is
	 *          to be evaluated.  If this pointer is null, the
	 *          value of the Promise is immediately set to be \a
	 *          valgen itself.
	 */
	PromiseData(const RObject* valgen, Environment* env);

        PromiseData(Promise* value);

        ~PromiseData();

        //* @brief Move constructor.
        PromiseData(PromiseData&&);
        // @brief Move assignment operator.
        PromiseData& operator=(PromiseData&& other);

        // PromiseData objects cannot be copied, only moved.  This helps
        // enforce that the promise is only evaluated once.
        PromiseData(const PromiseData&) = delete;
        PromiseData& operator=(const PromiseData&) = delete;

	static PromiseData createEvaluatedPromise(const RObject* expression,
						  RObject* evaluated_value) {
	    PromiseData result(expression, nullptr);
	    result.m_value = evaluated_value;
	    return result;
	}

        /** @brief Convert this object into a full Promise object.
         *
         * Moves this object's state into a heap-allocated Promise object and
         * return that object.
         */
        Promise* asPromise();

	/** @brief Force the Promise.
	 *
	 * i.e. evaluate the value generator of the Promise within the
	 * Environment of the Promise.  Following this, the
	 * environment pointer is set null, thus possibly allowing the
	 * Environment to be garbage-collected.
	 *
	 * If this function is used on a Promise that has already been
	 * forced, it simply returns the previously computed value.
	 *
	 * @return The value of the Promise, i.e. the result of
	 * evaluating the value generator.
	 */
	RObject* evaluate();

	//* @brief Has this promise been evaluated yet?
	bool evaluated() const {
          return getThis()->m_environment == nullptr;
	}

	/** @brief Not for general use.
	 *
	 * This function is used by ::isMissingArgument().  It
	 * implements some logic from CR's R_isMissing() which I don't
	 * fully understand.
	 *
	 * @return true iff ... well, read the code!
	 */
	bool isMissingSymbol() const;

	void visitReferents(GCNode::const_visitor* v) const;
	void detachReferents();
    private:
        // A PromiseData object has two possible representations.  It can either
        // store the data itself, or it might hold a pointer to a Promise
        // object.  The former representation is more efficient and should be
        // prefered where possible.  Once a promise has been evaluated, the
        // PromiseData object may switch back to the first representation.

        // To save space, m_value is overloaded.  In the first representation,
        // it stores the evaluated value of the promise.  In the second, it
        // stores the pointer to the Promise.
	GCEdge<> m_value;
	GCEdge<const RObject> m_valgen;
	GCEdge<Environment> m_environment;
	mutable bool m_under_evaluation;
	mutable bool m_interrupted;
        bool m_is_pointer_to_promise;

	/** @brief Set value of the Promise.
	 *
	 * Once the value is set to something other than
	 * Symbol::unboundValue(), the environment pointer is set
	 * null.
	 *
	 * @param val Value to be associated with the Promise.
	 */
	void setValue(RObject* val);

        PromiseData* getThis();
        const PromiseData* getThis() const;

	friend class Promise;
	friend int ::PRSEEN(SEXP x);
    };

    /** @brief Mechanism for deferred evaluation.
     *
     * This class is used to handle function arguments within R's lazy
     * evaluation scheme.  A Promise object encapsulates a pointer to
     * an arbitrary RObject (typically a Symbol or an Expression), and
     * a pointer to an Environment.  When the Promise is first
     * evaluated, the RObject is evaluated within the Environment, and
     * the result of evaluation returned as the value of the Promise.
     *
     * After the first evaluation, the result of evaluation is cached
     * within the Promise object, and the Environment pointer is set
     * null (thus possibly allowing the Environment to be
     * garbage-collected).  Subsequent evaluations of the Promise
     * object simply return the cached value.
     *
     * @note When a Promise is evaluated \e via a call to evaluate()
     * (the virtual function defined in class RObject), the \a env
     * parameter to evaluate() is ignored: evaluation uses only the
     * Environment encapsulated within the Promise object.  When an
     * RObject is known to be a Promise, it is suggested that
     * evaluation be carried out using the function Promise::force(),
     * which lacks the redundant parameter and is consequently clearer
     * to readers of the code.
     */
    class Promise : public RObject {
    public:
	Promise(const RObject* valgen, Environment* env)
	    : RObject(PROMSXP),
              m_data(&m_storage),
	      m_storage(valgen, env) {}

	Promise(PromiseData&& value)
	    : RObject(PROMSXP),
              m_data(&m_storage),
              m_storage(std::forward<PromiseData>(value))
	{}

        Promise(PromiseData* value, const GCNode* protect)
	    : RObject(PROMSXP),
              m_data(value),
              m_storage(nullptr)
      {
          m_protect = protect;
      }

	static Promise* createEvaluatedPromise(const RObject* expression,
					       RObject* evaluated_value) {
	    return new Promise(
		PromiseData::createEvaluatedPromise(expression,
						    evaluated_value));
	}

	/** @brief Force the Promise.
	 *
	 * i.e. evaluate the value generator of the Promise within the
	 * Environment of the Promise.  Following this, the
	 * environment pointer is set null, thus possibly allowing the
	 * Environment to be garbage-collected.
	 *
	 * If this function is used on a Promise that has already been
	 * forced, it simply returns the previously computed value.
	 *
	 * @return The value of the Promise, i.e. the result of
	 * evaluating the value generator.
	 */
	RObject* force() {
	    return m_data->evaluate();
	}

	/** @brief Not for general use.
	 *
	 * This function is used by ::isMissingArgument().  It
	 * implements some logic from CR's R_isMissing() which I don't
	 * fully understand.
	 *
	 * @return true iff ... well, read the code!
	 */
	bool isMissingSymbol() const {
	    return m_data->isMissingSymbol();
	}

	/** @brief The name by which this type is known in R.
	 *
	 * @return The name by which this type is known in R.
	 */
	static const char* staticTypeName()
	{
	    return "promise";
	}

	RObject* evaluate(Environment*) override {
	    return m_data->evaluate();
	}

	//* @brief Has this promise been evaluated yet?
	bool evaluated() const {
	    return m_data->evaluated();
	}

	const char* typeName() const override;

        // Virtual function of GCNode:
	void visitReferents(GCNode::const_visitor* v) const override {
	    m_data->visitReferents(v);
	    RObject::visitReferents(v);
            if (m_protect) {
                (*v)(m_protect);
            }
	}
    protected:
        // Virtual function of GCNode:
	void detachReferents() override {
	    m_data->detachReferents();
            m_protect = nullptr;
	    RObject::detachReferents();
	}
    private:
	friend class PromiseData;

	PromiseData* m_data;
        PromiseData m_storage;
        GCEdge<const GCNode> m_protect;

	friend RObject* ::PRCODE(RObject*);
	friend RObject* ::PRENV(RObject*);
	friend RObject* ::PRVALUE(RObject*);

	/** @brief Access the environment of the Promise.
	 *
	 * @return Pointer to the environment of the Promise.  This
	 * will be a null pointer after the promise has been
	 * evaluated.
	 */
	Environment* environment() const {
	    return m_data->m_environment;
	}

	/** @brief Access the value of a Promise.
	 *
	 * @return pointer to the value of the Promise, or to
	 * Symbol::unboundValue() if it has not yet been evaluated.
	 */
	RObject* value() {
	    return m_data->m_value;
	}

	/** @brief RObject to be evaluated by the Promise.
	 *
	 * @return const pointer to the RObject to be evaluated by
	 * the Promise.
	 */
	const RObject* valueGenerator() const {
	    return m_data->m_valgen;
	}

	/** @brief Set value of the Promise.
	 *
	 * Once the value is set to something other than
	 * Symbol::unboundValue(), the environment pointer is set
	 * null.
	 *
	 * @param val Value to be associated with the Promise.
	 */
	void setValue(RObject* val) {
	    m_data->setValue(val);
	}

	friend void ::SET_PRVALUE(SEXP x, SEXP v);  // Needs setValue().
	friend int ::PRSEEN(SEXP x);

	// Declared private to ensure that Promise objects are
	// created only using 'new':
	~Promise() {}
	Promise(const Promise&) = delete;
	Promise& operator=(const Promise&) = delete;
    };
    
    /** @brief Use forced value if RObject is a Promise.
     *
     * @param object Pointer, possibly null, to an RObject.
     *
     * @return If \a object points to a Promise, the Promise is forced
     * and the value of the Promise returned.  Otherwise \a object
     * itself is returned.
     */
    inline RObject* forceIfPromise(RObject* object)
    {
	if (object && object->sexptype() == PROMSXP)
	    object = static_cast<Promise*>(object)->force();
	return object;
    }

    inline PromiseData* PromiseData::getThis() {
        return m_is_pointer_to_promise
            ? static_cast<Promise*>(m_value.get())->m_data : this;
    }

    inline const PromiseData* PromiseData::getThis() const {
        return m_is_pointer_to_promise
            ? static_cast<const Promise*>(m_value.get())->m_data : this;
    }

}  // namespace rho

extern "C" {
    /** @brief Create a rho::Promise object.
     *
     * @param expr Expression to be evaluated to provide the value
     *          of the rho::Promise.
     *
     * @param env rho::Environment in which \a expr is to be evaluated.
     */
    SEXP Rf_mkPROMISE(SEXP expr, SEXP env);

    /** @brief Create a rho::Promise object which has already been evaluated.
     *
     * @param expr Expression to be evaluated to provide the value
     *          of the rho::Promise.
     *
     * @param value rho::RObject which is the value of the promise.
     */
    SEXP R_mkEVPROMISE(SEXP expr, SEXP value);

    /** @brief Access the expression of a rho::Promise.
     *
     * @param x Pointer to a rho::Promise (checked).
     *
     * @return Pointer to the expression to be evaluated by the
     *         rho::Promise. 
     */
    inline SEXP PRCODE(SEXP x)
    {
	using namespace rho;
	const Promise& prom = *SEXP_downcast<Promise*>(x);
	return const_cast<RObject*>(prom.valueGenerator());
    }

    /** @brief Access the value of a rho::Promise.
     *
     * @param x Pointer to a rho::Promise (checked).
     *
     * @return Pointer to the value of the rho::Promise, or to
     *         R_UnboundValue if it has not yet been evaluated..
     */
    inline SEXP PRVALUE(SEXP x)
    {
	using namespace rho;
	Promise& prom = *SEXP_downcast<Promise*>(x);
	return prom.value();
    }

    /** @brief Set the value of a rho::Promise.
     *
     * Once the value is set to something other than R_UnboundValue,
     * the environment pointer is set null.
     *
     * @param x Pointer to a rho::Promise (checked).
     *
     * @param v Pointer to the value to be assigned to the promise.
     *
     * @todo Replace this with a method call to evaluate the promise.
     */
    void SET_PRVALUE(SEXP x, SEXP v);
}

#endif /* RPROMISE_H */
