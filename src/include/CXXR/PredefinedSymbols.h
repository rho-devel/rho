/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2008-2014  Andrew R. Runnalls.
 *  Copyright (C) 2014 and onwards the CXXR Project Authors.
 *
 *  CXXR is not part of the R project, and bugs and other issues should
 *  not be reported via r-bugs or other R project channels; instead refer
 *  to the CXXR website.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

// This is not a normal header, but rather a listing of all the predefined
// symbols in CXXR and is used to automatically generate boilerplate code
// required for those symbols.

// To use this file, define the preprocessor function
//   PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME)
// and then #include this file.
//
// e.g.
//   #define PREDEFINED_SYMBOL(C_NAME, CXXR_NAME, R_NAME) \
//   SEXP C_NAME = CXXR::CXXR_NAME;
//   #include "CXXR/PredefinedSymbols.h"
//   #undef PREDEFINED_SYMBOL
// generates:
//   SEXP R_Bracket2Symbol = CXXR::Bracket2Symbol;
//   SEXP R_BracketSymbol = CXXR::BracketSymbol;
//   ...

// TODO(kmillar): distinguish between public and private symbols.

// Helper macro for the common case where the C name is the same as the CXXR
// name with a 'R_' prefix.
#define PREDEFINED_SYMBOL2(CXXR_NAME, R_NAME) \
  PREDEFINED_SYMBOL(R_ ## CXXR_NAME, CXXR_NAME, R_NAME)

PREDEFINED_SYMBOL2(baseSymbol, "base");  // deprecated.
PREDEFINED_SYMBOL2(BaseSymbol, "base");
PREDEFINED_SYMBOL2(BraceSymbol, "{");
PREDEFINED_SYMBOL2(BracketSymbol, "[");
PREDEFINED_SYMBOL2(SubsetSym, "[");
PREDEFINED_SYMBOL2(SubassignSym, "[<-");
PREDEFINED_SYMBOL2(Bracket2Symbol, "[[");
PREDEFINED_SYMBOL2(Subset2Sym, "[[");
PREDEFINED_SYMBOL2(Subassign2Sym, "[[<-");
PREDEFINED_SYMBOL2(ColonSymbol, ":");
PREDEFINED_SYMBOL2(TmpvalSymbol, "*tmp*");
PREDEFINED_SYMBOL2(ClassSymbol, "class");
PREDEFINED_SYMBOL2(ConnIdSymbol, "conn_id");
PREDEFINED_SYMBOL2(DimNamesSymbol, "dimnames");
PREDEFINED_SYMBOL2(DimSymbol, "dim");
PREDEFINED_SYMBOL2(DollarSymbol, "$");
PREDEFINED_SYMBOL2(DollarGetsSymbol, "$<-");
PREDEFINED_SYMBOL2(DotClassSymbol, ".Class");
PREDEFINED_SYMBOL(R_DeviceSymbol, DotDeviceSymbol, ".Device");
PREDEFINED_SYMBOL(R_DevicesSymbol, DotDevicesSymbol, ".Devices");
PREDEFINED_SYMBOL(R_dot_Generic, DotGenericSymbol, ".Generic");
PREDEFINED_SYMBOL2(DotGenericCallEnvSymbol, ".GenericCallEnv");
PREDEFINED_SYMBOL2(DotGenericDefEnvSymbol, ".GenericDefEnv");
PREDEFINED_SYMBOL2(DotGroupSymbol, ".Group");
PREDEFINED_SYMBOL(R_dot_Method, DotMethodSymbol, ".Method");
PREDEFINED_SYMBOL2(DotMethodsSymbol, ".Methods");
PREDEFINED_SYMBOL(R_dot_defined, DotdefinedSymbol, ".defined");
PREDEFINED_SYMBOL(R_dot_packageName, DotPackageName, ".packageName");
PREDEFINED_SYMBOL2(DotsSymbol, "...");
PREDEFINED_SYMBOL(R_dot_target, DottargetSymbol, ".target");
PREDEFINED_SYMBOL2(DoubleColonSymbol, "::");
PREDEFINED_SYMBOL2(DropSymbol, "drop");
PREDEFINED_SYMBOL2(ExactSymbol, "exact");
PREDEFINED_SYMBOL2(LastvalueSymbol, ".Last.value");
PREDEFINED_SYMBOL2(LevelsSymbol, "levels");
PREDEFINED_SYMBOL2(ModeSymbol, "mode");
PREDEFINED_SYMBOL2(NameSymbol, "name");
PREDEFINED_SYMBOL2(NamesSymbol, "names");
PREDEFINED_SYMBOL2(NamespaceEnvSymbol, ".__NAMESPACE__.");
PREDEFINED_SYMBOL2(NaRmSymbol, "na.rm");
PREDEFINED_SYMBOL2(PackageSymbol, "package");
PREDEFINED_SYMBOL2(PreviousSymbol, "previous");
PREDEFINED_SYMBOL2(QuoteSymbol, "quote");
PREDEFINED_SYMBOL2(RowNamesSymbol, "row.names");
PREDEFINED_SYMBOL2(S3MethodsTableSymbol, ".__S3MethodsTable__.");
PREDEFINED_SYMBOL2(SeedsSymbol, ".Random.seed");
PREDEFINED_SYMBOL2(SourceSymbol, "source");
PREDEFINED_SYMBOL2(SortListSymbol, "sort.list");
PREDEFINED_SYMBOL2(SpecSymbol, "spec");
PREDEFINED_SYMBOL2(TripleColonSymbol, ":::");
PREDEFINED_SYMBOL2(TspSymbol, "tsp");
PREDEFINED_SYMBOL2(CommentSymbol, "comment");
PREDEFINED_SYMBOL2(DotEnvSymbol, ".Environment");
PREDEFINED_SYMBOL2(RecursiveSymbol, "recursive");
PREDEFINED_SYMBOL2(UseNamesSymbol, "use.names");
PREDEFINED_SYMBOL2(SrcfileSymbol, "srcfile");
PREDEFINED_SYMBOL2(SrcrefSymbol, "srcref");
PREDEFINED_SYMBOL2(valueSym, "value");
PREDEFINED_SYMBOL2(WholeSrcrefSymbol, "wholeSrcref");

#undef PREDEFINED_SYMBOL2
