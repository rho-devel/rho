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

//------------------------------------------------------------------------------
// Implementation of BasicFunctionPass.

bool BasicFunctionPass::runOnFunction(llvm::Function& function) {
  RemoveRedundantCallsToSetVisibility optimize_set_visibility;
  bool changed = false;
  for (auto& block : function.getBasicBlockList()) {
    bool block_changed = optimize_set_visibility.runOnBasicBlock(block);
    changed = changed || block_changed;
  }
  return changed;
}

// LLVM uses the address of the following variable, the value is unimportant.
char BasicFunctionPass::pass_id = 0;

//------------------------------------------------------------------------------
// Impelementation of RemoveRedundantCallsToSetVisibility.

bool RemoveRedundantCallsToSetVisibility::runOnBasicBlock(
    llvm::BasicBlock& block) {
  // Collect all calls to kSetVisibilityFuncName.
  std::vector<llvm::CallInst*> calls_to_delete;  // Pointed insts not owned.
  for (llvm::Instruction& instr : block) {
    llvm::CallInst* call_inst = llvm::dyn_cast<llvm::CallInst>(&instr);
    if (call_inst != nullptr &&
        call_inst->getCalledFunction()->getName() == kSetVisibilityFuncName) {
      calls_to_delete.emplace_back(call_inst);
    }
  }
  // Finally, remove all calls, except the last one.
  if (calls_to_delete.size() > 1) {
    calls_to_delete.pop_back();
    for (llvm::CallInst* call_to_delete : calls_to_delete) {
      call_to_delete->eraseFromParent();
    }
    return true;
  } else {
    return false;
  }
}

// LLVM uses the address of the following variable, the value is unimportant.
char RemoveRedundantCallsToSetVisibility::pass_id = 0;

}  // namespace JIT
}  // namespace rho
