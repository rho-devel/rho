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

#include "gtest/gtest.h"

#define R_NO_REMAP
#include "rho/jit/MCJITMemoryManager.hpp"
#include "rho/jit/TypeBuilder.hpp"

#include "rho/BuiltInFunction.hpp"
#include "rho/Symbol.hpp"

using namespace rho;
using namespace rho::JIT;
using namespace llvm;

class MCJITMemoryManagerTest : public ::testing::Test
{
protected:
    MCJITMemoryManagerTest()
	: m_context(getGlobalContext()),
	  m_module(new Module("mcjit_mm_test", m_context)),
	  m_manager(new MCJITMemoryManager(m_module.get())) { }

    LLVMContext& m_context;
    std::unique_ptr<Module> m_module;
    std::unique_ptr<MCJITMemoryManager> m_manager;
};


TEST_F(MCJITMemoryManagerTest, FindSymbol) {
    Symbol* test_symbol = Symbol::obtain("mcjit_mm_test_symbol");
    ASSERT_TRUE(test_symbol != nullptr);
    
    GlobalVariable* global = m_manager->getSymbol(test_symbol);
    ASSERT_TRUE(global != nullptr);

    uint64_t address = m_manager->getSymbolAddress(global->getName());
    EXPECT_EQ(test_symbol, reinterpret_cast<Symbol*>(address));
}

TEST_F(MCJITMemoryManagerTest, DuplicateSymbol) {
    Symbol* test_symbol = Symbol::obtain("mcjit_mm_test_symbol");
    ASSERT_TRUE(test_symbol != nullptr);
    
    GlobalVariable* global = m_manager->getSymbol(test_symbol);
    ASSERT_TRUE(global != nullptr);
    GlobalVariable* global2 = m_manager->getSymbol(test_symbol);
    ASSERT_TRUE(global == global2);

    uint64_t address = m_manager->getSymbolAddress(global->getName());
    EXPECT_EQ(test_symbol, reinterpret_cast<Symbol*>(address));
}

TEST_F(MCJITMemoryManagerTest, FindBuiltInInternal) {
    BuiltInFunction* function = BuiltInFunction::obtainInternal("all.names");
    ASSERT_TRUE(function != nullptr);
    
    GlobalVariable* global = m_manager->getBuiltIn(function);
    ASSERT_TRUE(global != nullptr);

    uint64_t address = m_manager->getSymbolAddress(global->getName());
    EXPECT_EQ(function, reinterpret_cast<BuiltInFunction*>(address));
}

TEST_F(MCJITMemoryManagerTest, FindBuiltInPrimitive) {
    BuiltInFunction* function = BuiltInFunction::obtainPrimitive("if");
    ASSERT_TRUE(function != nullptr);
    
    GlobalVariable* global = m_manager->getBuiltIn(function);
    ASSERT_TRUE(global != nullptr);

    uint64_t address = m_manager->getSymbolAddress(global->getName());
    EXPECT_EQ(function, reinterpret_cast<BuiltInFunction*>(address));
}

TEST_F(MCJITMemoryManagerTest, GetGlobal) {
    const std::string object_name = "mcjit_mm_test_obj_1";
    Environment* test_object = (Environment*)0x1112222333334444;
    GlobalVariable* global = m_manager->addGlobal(test_object, true,
						  object_name);
    ASSERT_TRUE(global != nullptr);

    uint64_t address = m_manager->getSymbolAddress(object_name);
    EXPECT_EQ(test_object, reinterpret_cast<Environment*>(address));
}

TEST_F(MCJITMemoryManagerTest, GetAnonymousGlobal) {
    Environment* test_object = (Environment*)0x2222333334444555;
    GlobalVariable* global = m_manager->addGlobal(test_object, true);
    ASSERT_TRUE(global != nullptr);

    uint64_t address = m_manager->getSymbolAddress(global->getName());
    EXPECT_EQ(test_object, reinterpret_cast<Environment*>(address));
}

TEST_F(MCJITMemoryManagerTest, MultipleAnonymousGlobals) {
    Environment* test_object_1 = (Environment*)0x1;
    GlobalVariable* global_1 = m_manager->addGlobal(test_object_1, true);

    Environment* test_object_2 = (Environment*)0x2;
    GlobalVariable* global_2 = m_manager->addGlobal(test_object_2, true);

    uint64_t address_1 = m_manager->getSymbolAddress(global_1->getName());
    EXPECT_EQ(test_object_1, reinterpret_cast<Environment*>(address_1));
    uint64_t address_2 = m_manager->getSymbolAddress(global_2->getName());
    EXPECT_EQ(test_object_2, reinterpret_cast<Environment*>(address_2));
}

TEST_F(MCJITMemoryManagerTest, GlobalNameCollision) {
    const std::string object_name = "mcjit_mm_test_obj_2";
    Environment* test_object_1 = (Environment*)0x1;
    GlobalVariable* global_1 = m_manager->addGlobal(test_object_1, true,
						    object_name);

    Environment* test_object_2 = (Environment*)0x2;
    GlobalVariable* global_2 = m_manager->addGlobal(test_object_2, true,
						    object_name);

    uint64_t address_1 = m_manager->getSymbolAddress(global_1->getName());
    EXPECT_EQ(test_object_1, reinterpret_cast<Environment*>(address_1));
    uint64_t address_2 = m_manager->getSymbolAddress(global_2->getName());
    EXPECT_EQ(test_object_2, reinterpret_cast<Environment*>(address_2));
}

TEST_F(MCJITMemoryManagerTest, DuplicateDefinition) {
    const std::string object_name = "mcjit_mm_test_obj_3";
    Environment* test_object = (Environment*)0x1;
    GlobalVariable* global_1 = m_manager->addGlobal(test_object, true,
						    object_name);
    GlobalVariable* global_2 = m_manager->addGlobal(test_object, true,
						    object_name);
    EXPECT_EQ(global_1, global_2);

    uint64_t address_1 = m_manager->getSymbolAddress(global_1->getName());
    EXPECT_EQ(test_object, reinterpret_cast<Environment*>(address_1));
}

TEST_F(MCJITMemoryManagerTest, UndefinedGlobal) {
    uint64_t address = m_manager->getSymbolAddress("my_undefined_symbol");
    EXPECT_EQ(0, address);
}
