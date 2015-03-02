#ifndef CXXR_JIT_LLVM_HPP
#define CXXR_JIT_LLVM_HPP

#include <llvm/Config/llvm-config.h>

#define LLVM_VERSION ((100 * LLVM_VERSION_MAJOR) + (LLVM_VERSION_MINOR))

#include "llvm/LinkAllIR.h"
#include "llvm/LinkAllPasses.h"

#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/IRReader/IRReader.h"

#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"

#include "llvm/Transforms/Utils/Cloning.h"

#include "llvm/ADT/ArrayRef.h"

#include "llvm/Support/Casting.h"
#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"

#include "llvm/Target/TargetOptions.h"

#if (LLVM_VERSION >= 305)
#include "llvm/Linker/Linker.h"
#else
#include "llvm/Linker.h"
#endif

#if (LLVM_VERSION >= 305)
#else
#include "llvm/Support/system_error.h"
#endif

#if (LLVM_VERSION >= 305)
#include "llvm/IR/Verifier.h"
#else
#include "llvm/Analysis/Verifier.h"
#endif

#endif //CXXR_JIT_LLVM_HPP
