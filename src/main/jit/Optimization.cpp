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

#include <deque>
#include <set>
#include <string>
#include <tuple>
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
  using TreeNode = llvm::DomTreeNodeBase<llvm::BasicBlock>;
  using QEntry = std::pair<TreeNode*, bool>;
  using NodeQ = std::deque<QEntry>;

  llvm::PostDominatorTree post_domtree;
  post_domtree.runOnFunction(function);
  std::set<llvm::BasicBlock*> covered_blocks;
  RemoveRedundantCallsToSetVisibility optimize_set_visibility;
  NodeQ nodes;
  for (const auto root : post_domtree.DT->getRoots()) {
    nodes.push_back(QEntry(post_domtree.DT->getNode(root),
                           /* leave_one= */ true));
  }

  // Traverse the postdom tree to remove all post-dominated calls.
  while (!nodes.empty()) {
    TreeNode* node;
    bool leave_one;
    std::tie(node, leave_one) = nodes.front();
    covered_blocks.insert(node->getBlock());
    nodes.pop_front();
    bool leave_one_in_children = optimize_set_visibility.RemoveRedundantCalls(
                                     *node->getBlock(), leave_one) == 0 &&
                                 leave_one;
    for (auto child : node->getChildren()) {
      nodes.push_back(QEntry(child, leave_one_in_children));
    }
  }

  // Remove reundancies in any remaining basic blocks, not in the postdom tree.
  bool changed = covered_blocks.size() > 0;
  for (auto& block : function.getBasicBlockList()) {
    if (covered_blocks.find(&block) != covered_blocks.end()) continue;
    changed = optimize_set_visibility.runOnBasicBlock(block) || changed;
  }
  return changed;
}

// LLVM uses the address of the following variable, the value is unimportant.
char BasicFunctionPass::pass_id = 0;

//------------------------------------------------------------------------------
// Impelementation of RemoveRedundantCallsToSetVisibility.

int RemoveRedundantCallsToSetVisibility::RemoveRedundantCalls(
    llvm::BasicBlock& block, bool leave_one) {
  // Collect all calls to kSetVisibilityFuncName.
  std::vector<llvm::CallInst*> calls_to_delete;  // Pointed insts not owned.
  for (llvm::Instruction& instr : block) {
    llvm::CallInst* call_inst = llvm::dyn_cast<llvm::CallInst>(&instr);
    if (call_inst != nullptr &&
        call_inst->getCalledFunction()->getName() == kSetVisibilityFuncName) {
      calls_to_delete.emplace_back(call_inst);
    }
  }
  int count = calls_to_delete.size();
  if (count > 0 && leave_one) {
    calls_to_delete.pop_back();  // Don't delete the last call.
  }
  for (llvm::CallInst* call_to_delete : calls_to_delete) {
    call_to_delete->eraseFromParent();
  }
  return count;
}

// LLVM uses the address of the following variable, the value is unimportant.
char RemoveRedundantCallsToSetVisibility::pass_id = 0;

}  // namespace JIT
}  // namespace rho
