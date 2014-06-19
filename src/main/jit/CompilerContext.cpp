#include "CXXR/jit/CompilerContext.hpp"

#include "llvm/IR/Function.h"

using llvm::LLVMContext;
using llvm::Module;

namespace CXXR {
namespace JIT {

CompilerContext::CompilerContext(const Closure* closure,
				 llvm::Value* environment,
				 llvm::Function* function)
{
    m_closure = closure;
    m_environment = environment;
    m_function = function;
}

Module* CompilerContext::getModule()
{
    return getFunction()->getParent();
}

LLVMContext& CompilerContext::getLLVMContext()
{
    return getFunction()->getContext();
}

} // namespace JIT
} // namespace CXXR
