#include "CXXR/jit/Runtime.hpp"

#include "CXXR/jit/EmitIR.hpp"
#include "CXXR/jit/Globals.hpp"
#include "CXXR/jit/TypeBuilder.hpp"
#include "CXXR/RObject.h"
#include "Rinternals.h"

#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/TypeBuilder.h"
#include "llvm/Linker.h"
#include "llvm/Support/Casting.h"

using llvm::Constant;
using llvm::ConstantExpr;
using llvm::ConstantInt;
using llvm::Function;
using llvm::FunctionType;
using llvm::IRBuilder;
using llvm::LLVMContext;
using llvm::Linker;
using llvm::Module;
using llvm::Type;
using llvm::TypeBuilder;
using llvm::Value;
using llvm::dyn_cast;

namespace CXXR {

namespace JIT {

llvm::Module* getModule(IRBuilder<>* builder)
{
    return builder->GetInsertBlock()->getParent()->getParent();
}

namespace Runtime {

    llvm::Function* getDeclaration(FunctionId function,
				   llvm::Module* module) {
	return module->getFunction(getName(function));
    }

    static llvm::Function* getDeclaration(FunctionId fun,
					  IRBuilder<>* builder) {
	return getDeclaration(fun, getModule(builder));
}

extern "C" RObject* evaluate(RObject* value, Environment* environment)
{
    return value->evaluate(environment);
}

Value* emitEvaluate(Value* value, Value* environment,
		    IRBuilder<>* builder) {
    Function* f = getDeclaration(EVALUATE, builder);
    return builder->CreateCall2(f, value, environment);
}


extern "C" RObject* lookupSymbol(Symbol* value, Environment* environment)
{
    return value->evaluate(environment);
}

Value* emitLookupSymbol(Value* value, Value* environment,
			IRBuilder<>* builder)
{
    Function* f = getDeclaration(LOOKUP_SYMBOL, builder);
    return builder->CreateCall2(f, value, environment);
}

Value* emitLookupSymbol(const Symbol* value, Value* environment,
			IRBuilder<>* builder) 
{
    Value* symbol = emitSymbol(value);
    return emitLookupSymbol(symbol, environment, builder);
}


extern "C" FunctionBase* lookupFunction(const Symbol* symbol,
					const Environment* environment)
{
    return SEXP_downcast<FunctionBase*>(Rf_findFun(
        const_cast<Symbol*>(symbol),
        const_cast<Environment*>(environment)));
}

Value* emitLookupFunction(Value* value, Value* environment,
			  IRBuilder<>* builder)
{
    Function* f = getDeclaration(LOOKUP_FUNCTION, builder);
    return builder->CreateCall2(f, value, environment);
}

Value* emitLookupFunction(const Symbol* value, Value* environment,
			  IRBuilder<>* builder) 
{
    Value* symbol = emitSymbol(value);
    return emitLookupFunction(symbol, environment, builder);
}

extern "C"
RObject* callFunction(const FunctionBase* function, const PairList* args,
		      const Expression* call, Environment* environment)
{
    ArgList arglist(args, ArgList::RAW);
    return function->apply(&arglist, environment, call);
}

Value* emitCallFunction(llvm::Value* function_base,
			llvm::Value* pairlist_args,
			llvm::Value* call,
			llvm::Value* environment,
			IRBuilder<>* builder)
{
    Function* f = getDeclaration(CALL_FUNCTION, builder);
    return builder->CreateCall4(f, function_base, pairlist_args, call,
				environment);
}


static Module* runtime_module = nullptr;

template <class FUNCTION>
static void createRuntimeFunction(FunctionId functionId, FUNCTION* fun)
{
    FunctionType* type = TypeBuilder<FUNCTION, false>::get(
	runtime_module->getContext());
    Function* function = Function::Create(type,
					  Function::ExternalLinkage,
					  getName(functionId),
					  runtime_module);
    engine->addGlobalMapping(function, reinterpret_cast<void*>(fun));
}

// TODO: read the module in from a bytecode file compiled from C++ by clang,
// where all the functions have static linkage, so llvm can ignore them if
// unused.
static void setupRuntimeModule(LLVMContext &context) {
    if (runtime_module != nullptr) {
	return;
    }

    runtime_module = new Module("cxxr.module", context);
    assert(engine != nullptr);
    engine->addModule(runtime_module);

    createRuntimeFunction(EVALUATE, evaluate);
    createRuntimeFunction(LOOKUP_SYMBOL, lookupSymbol);
    createRuntimeFunction(LOOKUP_FUNCTION, lookupFunction);
    createRuntimeFunction(CALL_FUNCTION, callFunction);
}

void mergeInRuntimeModule(llvm::Module* module) {
    setupRuntimeModule(module->getContext());
    // TODO: error handling
    Linker::LinkModules(module, runtime_module, Linker::PreserveSource,
			nullptr);
}

    std::string getName(FunctionId function) {
	switch(function) {
	case NOT_A_RUNTIME_FUNCTION:
	    assert(0 && "Invalid FunctionId value passed.");
	    return nullptr; // TODO: throw an exception.
	case EVALUATE:
	    return "evaluate";
	case LOOKUP_SYMBOL:
	    return "lookupSymbol";
	case LOOKUP_FUNCTION:
	    return "lookupFunction";
	case CALL_FUNCTION:
	    return "callFunction";
	};
    }

static const FunctionId allFunctionIds[]
= { EVALUATE, LOOKUP_SYMBOL, LOOKUP_FUNCTION, CALL_FUNCTION };

    FunctionId getFunctionId(llvm::Function* function) {
	// TODO: implement more efficiently.
	std::string name = function->getName();
	for (FunctionId id : allFunctionIds) {
	    if (name == getName(id)) {
		return id;
	    }
	}
	return NOT_A_RUNTIME_FUNCTION;
    }

} // namespace Runtime
} // namespace JIT
} // namespace CXXR
