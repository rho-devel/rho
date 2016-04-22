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

#ifndef RHO_JIT_OPTIMIZATION_HPP
#define RHO_JIT_OPTIMIZATION_HPP

#include "rho/jit/llvm.hpp"

namespace rho {
namespace JIT {
class BasicFunctionPass : public llvm::FunctionPass {
 public:
  BasicFunctionPass() : llvm::FunctionPass(pass_id) {}
  BasicFunctionPass(BasicFunctionPass& other) = delete;
  BasicFunctionPass& operator=(const BasicFunctionPass& other) = delete;
  BasicFunctionPass(BasicFunctionPass&& other) = default;
  BasicFunctionPass& operator=(BasicFunctionPass&& other) = default;

  // Eliminates redundant calls to rho_runtime_setVisibility.  Any call that is
  // postdominated by another is redundant.
  // TODO(arunchauhan): Add more intra-procedural optimizations.
  bool runOnFunction(llvm::Function& function);

 private:
  static char pass_id;  // LLVM uses the address of this variable as the ID.
};

}  // namespace JIT
}  // namespace rho

#endif  // RHO_JIT_OPTIMIZATIOIN_HPP
