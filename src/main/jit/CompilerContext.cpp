/*CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR
 *CXXR CXXR is Copyright (C) 2008-13 Andrew R. Runnalls, subject to such other
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

#define R_NO_REMAP

#include "CXXR/jit/CompilerContext.hpp"

#include "CXXR/LoopException.hpp"
#include "CXXR/jit/Compiler.hpp"
#include "CXXR/jit/FrameDescriptor.hpp"

#include "llvm/IR/Function.h"

#include <typeinfo>

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
				 llvm::Function* function)
{
    m_closure = closure;
    m_environment = environment;
    m_function = function;
    m_frame_descriptor = new FrameDescriptor(closure);
}

CompilerContext::~CompilerContext() {
    delete m_frame_descriptor;
}

Module* CompilerContext::getModule()
{
    return getFunction()->getParent();
}

LLVMContext& CompilerContext::getLLVMContext()
{
    return getFunction()->getContext();
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

} // namespace JIT
} // namespace CXXR
