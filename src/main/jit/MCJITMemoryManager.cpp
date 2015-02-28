/*
 *  R : A Computer Language for Statistical Data Analysis
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
#include "CXXR/jit/llvm.hpp"

#define R_NO_REMAP
#include "CXXR/jit/MCJITMemoryManager.hpp"

#include "CXXR/jit/TypeBuilder.hpp"
#include "CXXR/BuiltInFunction.h"
#include "CXXR/Symbol.h"

using namespace llvm;

namespace CXXR {
namespace JIT {

static inline bool startsWith(const std::string& text,
			      const std::string& prefix) {
    return (text.compare(0, prefix.size(), prefix) == 0);
}

static const std::string symbol_prefix = "cxxr.symbol.";
static const std::string primitive_prefix = "cxxr.primitive.";
static const std::string internal_prefix = "cxxr.internal.";

MCJITMemoryManager::MCJITMemoryManager(Module* module)
    : m_module(module) { }

uint64_t MCJITMemoryManager::getSymbolAddress(const std::string& name)
{
    if (startsWith(name, symbol_prefix)) {
	std::string symbol_name = name.substr(symbol_prefix.length());
	return reinterpret_cast<uint64_t>(Symbol::obtain(symbol_name));
    }

    if (startsWith(name, primitive_prefix)) {
	// This returns the location of the BuiltInFunction.
	std::string builtin_name = name.substr(primitive_prefix.length());
	return reinterpret_cast<uint64_t>(
	    BuiltInFunction::obtainPrimitive(builtin_name));
    }
    if (startsWith(name, internal_prefix)) {
	// This returns the location of the BuiltInFunction.
	std::string builtin_name = name.substr(internal_prefix.length());
	return reinterpret_cast<uint64_t>(
	    BuiltInFunction::obtainInternal(builtin_name));
    }

    auto mapping = m_mappings.find(name);
    if (mapping != m_mappings.end()) {
	return reinterpret_cast<uint64_t>(mapping->second.first);
    }

    return RTDyldMemoryManager::getSymbolAddress(name);
}

GlobalVariable* MCJITMemoryManager::getSymbol(const Symbol* symbol)
{
    Type* type = TypeBuilder<Symbol, false>::get(m_module->getContext());
    std::string name = symbol_prefix + symbol->name()->stdstring();

    GlobalVariable* result = m_module->getNamedGlobal(name);
    if (result) {
	return result;
    }
    return new GlobalVariable(*m_module, type, true,
			      GlobalValue::ExternalLinkage, nullptr,
			      name);
}

GlobalVariable* MCJITMemoryManager::getBuiltIn(const BuiltInFunction* function)
{
    Type* type = TypeBuilder<BuiltInFunction, false>::get(
	m_module->getContext());
    std::string name = 
	(function->viaDotInternal() ? internal_prefix : primitive_prefix)
	+ function->name();

    GlobalVariable* result = m_module->getNamedGlobal(name);
    if (result) {
	return result;
    }
    return new GlobalVariable(*m_module, type, true,
     			      GlobalValue::ExternalLinkage, nullptr,
			      name);
}

static std::string addCounter(const std::string& prefix) {
	// Create our own, unique name.
	static int counter = 0;
	return prefix + "." + std::to_string(++counter);
}

GlobalVariable* MCJITMemoryManager::addGlobal(Type* type, void* address,
					      bool is_constant,
					      std::string prefix)
{
    std::string name = prefix;
    if (prefix.empty()) {
	prefix = "cxxr.global";
	name = addCounter(prefix);
    }
    // Find a unique name in the global table.
    std::pair<void*, GlobalVariable*>* item;
    while(1) {
	item = &m_mappings[name];
	if (item->second && item->first != address) {
	    // This name was already assigned to a different object.  Make a new
	    // name and try again.
	    name = addCounter(prefix);
	    continue;
	}
	break;
    }

    if (!item->second) {
	// New symbol in the global table.
	item->first = address;
	item->second = new GlobalVariable(*m_module, type, is_constant,
					  GlobalVariable::ExternalLinkage,
					  nullptr, name);
    }
    return item->second;
}

}  // namespace JIT
}  // namespace CXXR
