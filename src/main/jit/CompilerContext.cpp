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

#include <typeinfo>

#include "CXXR/jit/llvm.hpp"

#define R_NO_REMAP
#include "CXXR/jit/CompilerContext.hpp"

#include "CXXR/jit/Compiler.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"
#include "CXXR/jit/OptimizationOptions.hpp"
#include "CXXR/BuiltInFunction.h"
#include "CXXR/Closure.h"
#include "CXXR/Frame.hpp"
#include "CXXR/LoopException.hpp"

using llvm::BasicBlock;
using llvm::Function;
using llvm::LLVMContext;
using llvm::Module;
using llvm::PHINode;
using llvm::Value;

namespace CXXR {
namespace JIT {

CompilerContext::CompilerContext(const Closure* closure,
				 llvm::Value* environment,
				 llvm::Function* function,
				 MCJITMemoryManager* memory_manager)
{
    m_closure = closure;
    m_environment = environment;
    m_function = function;
    m_memory_manager = memory_manager;
    m_frame_descriptor = new FrameDescriptor(closure);
}

CompilerContext::~CompilerContext() {
  // nothing needed.
}

Module* CompilerContext::getModule()
{
    return getFunction()->getParent();
}

LLVMContext& CompilerContext::getLLVMContext()
{
    return getFunction()->getContext();
}

const Environment* CompilerContext::getEnclosingEnvironment()
{
    return getClosure()->environment();
}

FunctionBase* CompilerContext::staticallyResolveFunction(const Symbol* symbol)
{
    // A symbol can be statically resolved if:
    // - it is a control flow operator and AssumeSaneControlFlowOperators is set.
    // - OR it is an assignment and AssumeSaneAssignementOperators is set.
    // In both cases we still need to check for shadowing.
    bool isSaneControlFlowOp
	= (getOptimizationOptions().AssumeSaneControlFlowOperators
	   && controlFlowOperatorNames().count(symbol));
    bool isSaneAssignmentOp
	= (getOptimizationOptions().AssumeSaneAssignmentOperators
	   && assignmentOperatorNames().count(symbol));

    if (!isSaneControlFlowOp && !isSaneAssignmentOp) {
	return nullptr;
    }

    if (m_frame_descriptor->getLocation(symbol) != -1) {
	// The symbol is shadowed in the local frame.
	return nullptr;
    }

    const Frame::Binding* binding
	= getEnclosingEnvironment()->findBinding(symbol);
    if (binding->frame() != Environment::base()->frame()
	&& binding->frame() != Environment::baseNamespace()->frame()) {
	// Lookup returned a binding that isn't in base, so the symbol is
	// shadowed somewhere else.
	return nullptr;
    }

    FunctionBase* builtin_definition = BuiltInFunction::obtainPrimitive(
	symbol->name()->stdstring());
    if (binding->rawValue() != builtin_definition) {
	// The value in base has been changed.
	return nullptr;
    }

    return builtin_definition;
}

const OptimizationOptions& CompilerContext::getOptimizationOptions()
{
    // TODO(kmillar): make this settable.
    static OptimizationOptions options;
    return options;
}

bool CompilerContext::canInlineControlFlow()
{
    // Control flow can be inlined if all the control flow operators can be
    // statically resolved.
    for (const Symbol* symbol : controlFlowOperatorNames())
    {
	if (!staticallyResolveFunction(symbol))
	{
	    return false;
	}
    }
    return true;
}

template<class T>
static T* topOrNull(const std::stack<T*>& items) {
    return items.empty() ? nullptr : items.top();
}

BasicBlock* CompilerContext::getBreakDestination() {
    return topOrNull(m_break_destinations);
}

BasicBlock* CompilerContext::getNextDestination() {
    return topOrNull(m_next_destinations);
}

BasicBlock* CompilerContext::getExceptionLandingPad() {
    return topOrNull(m_exception_landing_pads);
}

void CompilerContext::pushLoopContext(BasicBlock* continue_block,
				      BasicBlock* loop_header,
				      Compiler* compiler)
{
    // TODO(kmillar): this needs to set and reset m_environment->m_in_loop.
    m_break_destinations.push(continue_block);
    m_next_destinations.push(loop_header);

    // The interpreter handles 'next' and 'break' by throwing a LoopException.
    // Setup an exception handler to handle the exception correctly.
    PHINode* handler = compiler->emitLoopExceptionHandler(
	continue_block, loop_header);

    pushExceptionHandlerContext(&typeid(LoopException), handler, compiler);
}

void CompilerContext::popLoopContext() {
    assert(!m_break_destinations.empty());
    assert(!m_next_destinations.empty());

    m_break_destinations.pop();
    m_next_destinations.pop();
    popExceptionHandlerContext();
}

void CompilerContext::pushExceptionHandlerContext(const std::type_info* type,
						  PHINode* handler,
						  Compiler* compiler) {
    PHINode* fallthrough = nullptr;
    if (!m_exception_handlers.empty()) {
	fallthrough = m_exception_handlers.top();
    } else {
	fallthrough = compiler->emitRethrowException();
    }

    PHINode* dispatch = compiler->emitDispatchToExceptionHandler(type,
								 handler,
								 fallthrough);
    BasicBlock* landing_pad = compiler->emitLandingPad(dispatch);
    m_exception_handlers.push(dispatch);
    m_exception_landing_pads.push(landing_pad);
}

void CompilerContext::popExceptionHandlerContext() {
    assert(!m_exception_handlers.empty());
    assert(!m_exception_landing_pads.empty());
    m_exception_handlers.pop();
    m_exception_landing_pads.pop();
}

// Helper function for the two functions below.
static std::set<const Symbol*> obtain_symbols(
    std::initializer_list<const char*> names)
{
    std::set<const Symbol*> result;
    for (const char* name : names) {
	result.insert(Symbol::obtain(name));
    }
    return result;
}

const std::set<const Symbol*>& CompilerContext::controlFlowOperatorNames()
{
    static std::set<const Symbol*> symbols = 
	obtain_symbols({ "{", "(", "if",
		    "for", "while", "repeat", "next", "break", "return",
		    "||", "&&" });
    return symbols;
}

const std::set<const Symbol*>& CompilerContext::assignmentOperatorNames()
{
    static std::set<const Symbol*> symbols = obtain_symbols({ "<-", "=" });
    return symbols;
}

} // namespace JIT
} // namespace CXXR
