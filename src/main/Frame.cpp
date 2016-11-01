/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the Rho Project Authors.
 *
 *  Rho is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the Rho website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/** @file Frame.cpp
 *
 * @brief Implementation of class Frame and Frame::Binding.
 */

#include "rho/Frame.hpp"

#include "localization.h"
#include "R_ext/Error.h"
#include "rho/Evaluator.hpp"
#include "rho/FunctionBase.hpp"
#include "rho/GCStackRoot.hpp"
#include "rho/Promise.hpp"
#include <algorithm>

using namespace std;
using namespace rho;

Frame::monitor Frame::s_read_monitor = nullptr;
Frame::monitor Frame::s_write_monitor = nullptr;

// ***** Class Frame::Binding *****

PairList* Frame::Binding::asPairList(PairList* tail) const
{
    PairList* ans = new PairList(m_value, tail, symbol());
    SET_MISSING(ans, origin());
    if (isActive()) SET_ACTIVE_BINDING_BIT(ans);
    if (isLocked()) LOCK_BINDING(ans);
    return ans;
}

// Frame::Binding::assign() is defined in envir.cpp (for the time being).
	
RObject* Frame::Binding::forcedValue() const
{
    return forcedValue2().first;
}

pair<RObject*, bool>
Frame::Binding::forcedValue2() const
{
    bool promise_forced = false;
    RObject* val = m_value;
    if (val && val->sexptype() == PROMSXP) {
	Promise* prom = static_cast<Promise*>(val);
	if (!prom->evaluated()) {
	    frame()->monitorRead(*this);
	    promise_forced = true;
	}
	val = Evaluator::evaluate(prom, nullptr);
    }
    return make_pair(val, promise_forced);
}

void Frame::Binding::fromPairList(PairList* pl)
{
    const RObject* tag = pl->tag();
    if (tag && tag != m_symbol)
	Rf_error(_("internal error in %s"),
		 "Frame::Binding::fromPairList()");
    Origin pl_origin = Origin(pl->m_missing);
    if (pl->m_active_binding)
	setFunction(SEXP_downcast<FunctionBase*>(pl->car()),
		    pl_origin);
    else setValue(pl->car(), pl_origin);
    setLocking(pl->m_binding_locked);
}
    
void Frame::Binding::initialize(Frame* frame, const Symbol* sym)
{
    if (m_frame)
	Rf_error(_("internal error: binding already initialized"));
    if (!frame || !sym)
	Rf_error(_("internal error in %s"),
		 "Frame::Binding::initialize()");
    m_frame = frame;
    m_symbol = sym;
}

void Frame::Binding::setFunction(FunctionBase* function, Origin origin)
{
    // See if binding already has a non-default value:
    if (m_value != Symbol::missingArgument()) {
	if (!isActive())
	    Rf_error(_("symbol already has a regular binding"));
	if (isLocked())
	    Rf_error(_("cannot change active binding if binding is locked"));
    }
    m_value = function;
    m_active = true;
    m_frame->monitorWrite(*this);
}

void Frame::Binding::setValue(RObject* new_value, Origin origin, bool quiet)
{
    if (isLocked())
	Rf_error(_("cannot change value of locked binding for '%s'"),
		 symbol()->name()->c_str());
    if (isActive())
	Rf_error(_("internal error: use %s for active bindings"),
		 "setFunction()");
    m_value = new_value;
    m_origin = origin;
    if (!quiet)
	m_frame->monitorWrite(*this);
}

Frame::Frame(size_t size, bool check_list_size)
    : Frame(ArgList(nullptr, ArgList::PROMISED), size, check_list_size)
{}

Frame::Frame(const ArgList& promised_args, size_t size, bool check_list_size)
    : m_descriptor(), m_bindings_size(0), m_used_bindings_size(0),
      m_cache_count(0), m_locked(false), m_no_special_symbols(true),
      m_read_monitored(false), m_write_monitored(false), m_overflow(nullptr),
      m_promised_args(promised_args.list(), promised_args.status())
{
    if (check_list_size && size > kMaxListSize) {
	size_t overflow_size = size - kMaxListSize;
	m_overflow = new map(overflow_size);
	size = kMaxListSize;
    }
    if (size > std::numeric_limits<decltype(m_bindings_size)>::max()) {
	size = std::numeric_limits<decltype(m_bindings_size)>::max();
    }

    m_bindings = new Binding[size];
    m_bindings_size = size;
    m_promised_args_protect = m_promised_args.list();
}

Frame::Frame(const FrameDescriptor* descriptor, const ArgList& promised_args)
    : Frame(promised_args, descriptor->getNumberOfSymbols(), false)
{
    m_descriptor = descriptor;
}

Frame::Frame(const Frame& source)
    : m_descriptor(source.m_descriptor), m_bindings_size(source.m_bindings_size),
      m_used_bindings_size(0), m_cache_count(0), m_locked(source.m_locked),
      m_no_special_symbols(source.m_no_special_symbols),
      m_read_monitored(false), m_write_monitored(false), m_overflow(nullptr),
      m_promised_args(source.m_promised_args.list(),
		      source.m_promised_args.status())
{
    m_bindings = new Binding[m_bindings_size];
    importBindings(&source);
    if (source.isLocked())
	lock(false);
    m_promised_args_protect = m_promised_args.list();
}

Frame::~Frame() {
    statusChanged(nullptr);
    delete[] m_bindings;
    delete m_overflow;
}

Frame* Frame::clone() const {
    return new Frame(*this);
}

vector<const Symbol*> Frame::symbols(bool include_dotsymbols,
				     bool sorted) const
{
    vector<const Symbol*> ans;
    visitBindings([&](const Binding* binding) {
	    const Symbol* symbol = binding->symbol();
	    if (include_dotsymbols || !isDotSymbol(symbol))
		ans.push_back(symbol);
	});
    if (sorted) {
	std::sort(ans.begin(), ans.end(),
		  [](const Symbol* x, const Symbol* y) {
		      return String::Comparator()(x->name(), y->name()); });
    }

    return ans;
}

// Frame::Binding::value() is defined in envir.cpp (for the time being).

void Frame::Binding::visitReferents(const_visitor* v) const
{
    // We assume the visitor has just come from m_frame, so we don't
    // visit that.
    if (m_value)
	(*v)(m_value);
#ifdef PROVENANCE_TRACKING
    if (m_provenance)
	(*v)(m_provenance);
#endif
}

std::size_t Frame::size() const
{
  std::size_t result = 0;
  for (int i = 0; i < m_used_bindings_size; ++i) {
      if (m_bindings[i].isSet()) {
	  result++;
      }
  }
  if (m_overflow) {
    result += m_overflow->size();
  }
  return result;
}

Frame::Binding* Frame::v_binding(const Symbol* symbol)
{
    for (size_t i = 0; i < m_used_bindings_size; i++) {
	if (m_bindings[i].symbol() == symbol && m_bindings[i].isSet())
	    return &m_bindings[i];
    }
    if (m_overflow) {
	auto location = m_overflow->find(symbol);
	if (location != m_overflow->end()) {
	    return &location->second;
	}
    }
    return nullptr;
}


PairList* Frame::asPairList() const
{
    GCStackRoot<PairList> ans(nullptr);
    visitBindings([&](const Binding* binding) {
	    ans = binding->asPairList(ans);
	});
    return ans;
}

void Frame::clear()
{
    statusChanged(nullptr);

    for (size_t i = 0; i < m_used_bindings_size; i++) {
	m_bindings[i].unset();
    }
    if (m_overflow) {
	delete m_overflow;
	m_overflow = nullptr;
    }

    m_no_special_symbols = true;
}

void Frame::detachReferents()
{
    clear();
    m_descriptor.detach();
}

bool Frame::erase(const Symbol* symbol)
{
    if (isLocked())
	Rf_error(_("cannot remove bindings from a locked frame"));

    for (size_t i = 0; i < m_used_bindings_size; i++) {
	if (m_bindings[i].symbol() == symbol && m_bindings[i].isSet())
	{
	    m_bindings[i].unset();
	    statusChanged(symbol);
	    return true;
	}
    }

    if (m_overflow) {
	if (m_overflow->erase(symbol)) {
	    statusChanged(symbol);
	    return true;
	}
    }
    return false;
}

void Frame::enableReadMonitoring(bool on) const
{
    if (on && !s_read_monitor)
	Rf_error("Internal error: Frame::s_read_monitor not set");
    m_read_monitored = on;
}

void Frame::enableWriteMonitoring(bool on) const
{
    if (on && !s_write_monitor)
	Rf_error("Internal error: Frame::s_write_monitor not set");
    m_write_monitored = on;
}

void Frame::flush(const Symbol* sym)
{
    Environment::flushFromSearchPathCache(sym);
}

void Frame::initializeBinding(Frame::Binding* binding,
			      const Symbol* symbol)
{
    assert(!isLocked());
    binding->initialize(this, symbol);
    statusChanged(symbol);
    if (symbol->isSpecialSymbol()) {
	m_no_special_symbols = false;
    }
}

void Frame::initializeBindingIfUnlocked(Frame::Binding* binding,
					const Symbol* symbol)
{
    if (isLocked()) {
	Rf_error(_("cannot add bindings to a locked frame"));
    }
    initializeBinding(binding, symbol);
}

Frame::Binding* Frame::obtainBinding(const Symbol* symbol)
{
    Frame::Binding* binding = nullptr;
    if (isLocked()) {
	// If the frame is locked, we can only return pre-existing bindings.
	Frame::Binding* binding = Frame::binding(symbol);
	if (!binding) {
	    Rf_error(_("cannot add bindings to a locked frame"));
	}
	return binding;
    }
    
    if (m_descriptor)
    {
	// Use the pressigned location.
	int location = m_descriptor->getLocation(symbol);
	if (location != -1) {
	    binding = m_bindings + location;
	    m_used_bindings_size = std::max(m_used_bindings_size,
					    (unsigned char)(location + 1));
	}
    } else
    {
	// If the binding exists, return that.
	binding = Frame::binding(symbol);
	if (binding)
	    return binding;
	
        // Otherwise return the first unused space in the array if any.
	for (unsigned char i = 0; i < m_bindings_size; ++i) {
	    if (!m_bindings[i].isSet()) {
		// Found an unused spot.
		m_used_bindings_size = std::max(m_used_bindings_size,
						(unsigned char)(i + 1));
		binding = &m_bindings[i];
		break;
	    }
	}
    }

    // If all else fails, go to the overflow.
    if (!binding) {
	if (!m_overflow) {
	    m_overflow = new map;
	}
	binding = &((*m_overflow)[symbol]);
    }

    if (!binding->frame()) {
	initializeBinding(binding, symbol);
    }
    return binding;
}

void Frame::importBinding(const Binding* binding_to_import, bool quiet) {
    if (!binding_to_import)
	return;
    Binding *new_binding = obtainBinding(binding_to_import->symbol());
    *new_binding = *binding_to_import;
    new_binding->m_frame = this;
    if (!quiet)
	monitorWrite(*new_binding);
}

void Frame::importBindings(const Frame* frame, bool quiet) {
    frame->visitBindings([=](const Binding* binding) {
	    importBinding(binding, quiet);
	});
}

void Frame::visitBindings(std::function<void(const Binding*)> f) const
{
    for (size_t i = 0; i < m_used_bindings_size; i++) {
	if (m_bindings[i].isSet())
	    f(&m_bindings[i]);
    }
    if (m_overflow) {
	for (const auto& entry : *m_overflow) {
	    f(&entry.second);
	}
    }
}

void Frame::modifyBindings(std::function<void(Binding*)> f)
{
    for (size_t i = 0; i < m_used_bindings_size; i++) {
	if (m_bindings[i].isSet())
	    f(&m_bindings[i]);
    }
    if (m_overflow) {
	for (auto& entry : *m_overflow) {
	    f(&entry.second);
	}
    }
}

void Frame::lockBindings() {
    modifyBindings([](Binding* binding) { binding->setLocking(true); });
}


void Frame::visitReferents(const_visitor* v) const
{
    visitBindings([=](const Binding* binding) {
	    binding->visitReferents(v);
	});
    if (m_descriptor)
	(*v)(m_descriptor);
    if (m_promised_args_protect)
	(*v)(m_promised_args_protect);
}

namespace rho {
    void frameReadPairList(Frame* frame, PairList* bindings)
    {
	for (PairList* pl = bindings; pl != nullptr; pl = pl->tail()) {
	    const RObject* tag = pl->tag();
	    const Symbol* symbol = dynamic_cast<const Symbol*>(tag);
	    if (!symbol) Rf_error(_("list used to set frame bindings"
				    " must have symbols as tags throughout"));
	    Frame::Binding* bdg = frame->obtainBinding(symbol);
	    bdg->fromPairList(pl);
	}
    }

    bool isMissingArgument(const Symbol* sym, Frame* frame)
    {
	RObject* rawval;
	if (sym->isDotDotSymbol()) {
	    unsigned int ddv = sym->dotDotIndex();
	    Frame::Binding* bdg = frame->binding(DotsSymbol);
	    if (!bdg)
		return false;  // This is what CR does.  Is it really right?
	    ConsCell* cc = SEXP_downcast<ConsCell*>(bdg->rawValue());
	    while (cc && ddv > 1) {
		cc = cc->tail();
		--ddv;
	    }
	    if (!cc)
		return true;
	    rawval = cc->car();
	} else {
	    // Not a ..n symbol:
	    if (sym == Symbol::missingArgument())
		return true;
	    Frame::Binding* bdg = frame->binding(sym);
	    if (!bdg)
		return false;
	    rawval = bdg->rawValue();
	    if (bdg->origin() == Frame::Binding::MISSING
		|| rawval == Symbol::missingArgument())
		return  true;
	    if (bdg->isActive())
		return false;
	}
	if (rawval && rawval->sexptype() == PROMSXP) {
	    Promise* prom = static_cast<Promise*>(rawval);
	    return prom->isMissingSymbol();
	}
	return false;
    }
}
