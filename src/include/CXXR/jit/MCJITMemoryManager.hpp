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

#ifndef CXXR_JIT_MCJIT_MEMORY_MANAGER_HPP
#define CXXR_JIT_MCJIT_MEMORY_MANAGER_HPP

#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/TypeBuilder.h"

#include <string>
#include <unordered_map>

namespace CXXR {

class BuiltInFunction;
class Symbol;

namespace JIT {

/*
 * Memory manager that additionally knows the locations of:
 * - All Symbol objects.
 * - All builtin function objects.
 * - Any objects that have been added with 'addGlobal()'.
 */
class MCJITMemoryManager : public llvm::SectionMemoryManager {
public:
    explicit MCJITMemoryManager(llvm::Module* module);

    uint64_t getSymbolAddress(const std::string& name) override;

    template<class T>
    llvm::GlobalVariable* addGlobal(T* object,
				    bool isConstant,
				    std::string name = "")
    {
	llvm::Type* type = llvm::TypeBuilder<T, false>::get(
	    m_module->getContext());
	return addGlobal(type, (void*)object, isConstant, name);
    }

    llvm::GlobalVariable* getSymbol(const Symbol* symbol);
    llvm::GlobalVariable* getBuiltIn(const BuiltInFunction* function);
private:
    llvm::Module* m_module;
    std::unordered_map<std::string,
		       std::pair<void*, llvm::GlobalVariable*>> m_mappings;

    llvm::GlobalVariable* addGlobal(llvm::Type* type, void* address,
				    bool is_constant, std::string name);

    MCJITMemoryManager(const MCJITMemoryManager&) = delete;
    MCJITMemoryManager& operator=(const MCJITMemoryManager&) = delete;
};

}  // namespace JIT
}  // namespace CXXR

#endif  // CXXR_JIT_MCJIT_MEMORY_MANAGER_HPP
