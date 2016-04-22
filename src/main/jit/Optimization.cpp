/*
 *  R : A Computer Language for Statistical Data Analysis
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

#include "rho/jit/Optimization.hpp"

#include <string>
#include <vector>

#include "rho/jit/llvm.hpp"

namespace rho {
namespace JIT {
namespace {
const std::string kSetVisibilityFuncName("rho_runtime_setVisibility");
}  // namespace

void RemoveRedundantCallsToSetVisibility(llvm::Module* module,
                                         llvm::Function* function) {
  std::vector<llvm::CallInst*> calls_to_delete;  // Pointed insts not owned.
  for (auto& block : function->getBasicBlockList()) {
    bool seen_call_to_setVisibility = false;
    for (auto r_iter = block.rbegin(); r_iter != block.rend(); ++r_iter) {
      auto call_inst = dynamic_cast<llvm::CallInst*>(&(*r_iter));
      if (call_inst != nullptr &&
          call_inst->getCalledFunction()->getName() == kSetVisibilityFuncName) {
        if (seen_call_to_setVisibility) calls_to_delete.emplace_back(call_inst);
        seen_call_to_setVisibility = true;
      }
    }
  }
  for (llvm::CallInst* call_to_delete : calls_to_delete) {
    call_to_delete->eraseFromParent();
  }
}

void BasicIntraProceduralOptimizations(llvm::Module* module,
                                       llvm::Function* function) {
  // TODO(ArunChauhan): Use make_unique when we move to C++-14.
  std::unique_ptr<llvm::FunctionPassManager> pass_manager(
      new llvm::FunctionPassManager(module));
  // Provide basic AliasAnalysis support for GVN.
  pass_manager->add(llvm::createBasicAliasAnalysisPass());
  pass_manager->add(llvm::createInstructionCombiningPass());
  pass_manager->add(llvm::createReassociatePass());
  pass_manager->add(llvm::createGVNPass());
  pass_manager->add(llvm::createCFGSimplificationPass());
  pass_manager->run(*function);
  llvm::verifyFunction(*function);
}

}  // namespace JIT
}  // namespace rho
