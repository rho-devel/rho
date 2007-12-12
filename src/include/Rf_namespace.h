/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/** \file Rf_namespace.h
 * 
 * This header file causes functions within the C interface to be
 * allocated symbol names starting with Rf_ , to avoid name clashes
 * with function names in client code.  It corresponds in C++ terms to
 * placing the functions in a namespace.  It should be included within
 * any file that uses or defines the functions concerned.
 */

#ifndef RF_NAMESPACE_H
#define RF_NAMESPACE_H

#ifndef R_NO_REMAP
/*#define allocString		Rf_allocString*/
  /*see comment in Rinlinedfuns.h
// #define isS4                     Rf_isS4
// #define asS4                    Rf_asS4
*/

// Please keep sorted!

#define CheckFormals		Rf_CheckFormals
#define CleanEd		        Rf_CleanEd
#define CoercionWarning       	Rf_CoercionWarning
#define ComplexFromInteger	Rf_ComplexFromInteger
#define ComplexFromLogical	Rf_ComplexFromLogical
#define ComplexFromReal	        Rf_ComplexFromReal
#define ComplexFromString	Rf_ComplexFromString
#define CreateTag		Rf_CreateTag
#define CustomPrintValue	Rf_CustomPrintValue
#define DataFrameClass		Rf_DataFrameClass
#define DispatchGroup		Rf_DispatchGroup
#define DispatchOrEval		Rf_DispatchOrEval
#define DropDims                Rf_DropDims
#define EncodeRaw               Rf_EncodeRaw
#define EncodeString            Rf_EncodeString
#define EnsureString 		Rf_EnsureString
#define ErrorMessage		Rf_ErrorMessage
#define FrameClassFix		Rf_FrameClassFix
#define GetArrayDimnames	Rf_GetArrayDimnames
#define GetColNames		Rf_GetColNames
#define GetMatrixDimnames	Rf_GetMatrixDimnames
#define GetOption		Rf_GetOption
#define GetOptionDigits		Rf_GetOptionDigits
#define GetOptionWidth		Rf_GetOptionWidth
#define GetRowNames		Rf_GetRowNames
#define InheritsClass		Rf_InheritsClass
#define InitArithmetic		Rf_InitArithmetic
#define InitBaseEnv		Rf_InitBaseEnv
#define InitColors		Rf_InitColors
#define InitConnections	        Rf_InitConnections
#define InitEd			Rf_InitEd
#define InitFunctionHashing	Rf_InitFunctionHashing
#define InitGlobalEnv		Rf_InitGlobalEnv
#define InitMemory		Rf_InitMemory
#define InitNames		Rf_InitNames
#define InitOptions		Rf_InitOptions
#define InitRand		Rf_InitRand
#define InitTempDir		Rf_InitTempDir
#define IntegerFromComplex	Rf_IntegerFromComplex
#define IntegerFromLogical	Rf_IntegerFromLogical
#define IntegerFromReal	        Rf_IntegerFromReal
#define IntegerFromString	Rf_IntegerFromString
#define ItemName		Rf_ItemName
#define LogicalFromComplex	Rf_LogicalFromComplex
#define LogicalFromInteger	Rf_LogicalFromInteger
#define LogicalFromReal	        Rf_LogicalFromReal
#define LogicalFromString	Rf_LogicalFromString
#define Mbrtowc		        Rf_mbrtowc
#define NewEnvironment		Rf_NewEnvironment
#define NonNullStringMatch	Rf_NonNullStringMatch
#define OneIndex		Rf_OneIndex
#define PairToVectorList	Rf_PairToVectorList
#define PrintDefaults		Rf_PrintDefaults
#define PrintGreeting		Rf_PrintGreeting
#define PrintValue		Rf_PrintValue
#define PrintValueEnv		Rf_PrintValueEnv
#define PrintValueRec		Rf_PrintValueRec
#define PrintVersion		Rf_PrintVersion
#define PrintVersionString    	Rf_PrintVersionString
#define PrintWarnings		Rf_PrintWarnings
#define RealFromComplex	        Rf_RealFromComplex
#define RealFromInteger	        Rf_RealFromInteger
#define RealFromLogical	        Rf_RealFromLogical
#define RealFromString		Rf_RealFromString
#define RemoveClass		Rf_RemoveClass
#define ScalarComplex		Rf_ScalarComplex
#define ScalarInteger		Rf_ScalarInteger
#define ScalarLogical		Rf_ScalarLogical
#define ScalarRaw		Rf_ScalarRaw
#define ScalarReal		Rf_ScalarReal
#define ScalarString		Rf_ScalarString
#define StrToInternal		Rf_StrToInternal
#define StringBlank		Rf_StringBlank
#define StringFromComplex	Rf_StringFromComplex
#define StringFromInteger	Rf_StringFromInteger
#define StringFromLogical	Rf_StringFromLogical
#define StringFromReal		Rf_StringFromReal
#define VectorToPairList	Rf_VectorToPairList
#define WarningMessage		Rf_WarningMessage
#define allocArray		Rf_allocArray
#define allocList		Rf_allocList
#define allocMatrix		Rf_allocMatrix
#define allocS4Object		Rf_allocS4Object
#define allocSExp		Rf_allocSExp
#define allocVector		Rf_allocVector
#define applyClosure		Rf_applyClosure
#define arraySubscript		Rf_arraySubscript
#define asChar			Rf_asChar
#define asComplex		Rf_asComplex
#define asInteger		Rf_asInteger
#define asLogical		Rf_asLogical
#define asReal			Rf_asReal
#define begincontext		Rf_begincontext
#define checkArity		Rf_checkArity
#define check_stack_balance	Rf_check_stack_balance
#define classgets		Rf_classgets
#define coerceVector		Rf_coerceVector
#define conformable		Rf_conformable
#define cons			Rf_cons
#define copyListMatrix		Rf_copyListMatrix
#define copyMatrix		Rf_copyMatrix
#define copyMostAttrib		Rf_copyMostAttrib
#define copyMostAttribNoTs	Rf_copyMostAttribNoTs
#define copyVector		Rf_copyVector
#define ddfindVar		Rf_ddfindVar
#define defineVar		Rf_defineVar
#define deparse1		Rf_deparse1
#define deparse1line		Rf_deparse1line
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
#define duplicate		Rf_duplicate
#define duplicated		Rf_duplicated
#define dynamicfindVar		Rf_dynamicfindVar
#define elt			Rf_elt
#define endcontext		Rf_endcontext
#define envlength		Rf_envlength
#define errorcall		Rf_errorcall
#define eval			Rf_eval
#define evalList		Rf_evalList
#define evalListKeepMissing	Rf_evalListKeepMissing
#define factorsConform		Rf_factorsConform
#define findFun			Rf_findFun
#define findVar			Rf_findVar
#define findVar1		Rf_findVar1
#define findVarInFrame		Rf_findVarInFrame
#define findVarInFrame3		Rf_findVarInFrame3
#define findcontext		Rf_findcontext
#define frameSubscript		Rf_frameSubscript
#define framedepth		Rf_framedepth
#define get1index		Rf_get1index
#define getAttrib		Rf_getAttrib
#define getVar			Rf_getVar
#define getVarInFrame		Rf_getVarInFrame
#define gsetVar			Rf_gsetVar
#define hashpjw		        Rf_hashpjw
#define inherits		Rf_inherits
#define initStack		Rf_initStack
#define install			Rf_install
#define internalTypeCheck	Rf_internalTypeCheck
#define isArray			Rf_isArray
#define isComplex		Rf_isComplex
#define isEnvironment		Rf_isEnvironment
#define isExpression		Rf_isExpression
#define isFactor		Rf_isFactor
#define isFrame			Rf_isFrame
#define isFree			Rf_isFree
#define isFunction		Rf_isFunction
#define isInteger		Rf_isInteger
#define isLanguage		Rf_isLanguage
#define isList			Rf_isList
#define isLogical		Rf_isLogical
#define isMatrix		Rf_isMatrix
#define isNewList		Rf_isNewList
#define isNull			Rf_isNull
#define isNumeric		Rf_isNumeric
#define isObject		Rf_isObject
#define isOrdered		Rf_isOrdered
#define isPairList		Rf_isPairList
#define isPrimitive		Rf_isPrimitive
#define isReal			Rf_isReal
#define isString		Rf_isString
#define isSymbol		Rf_isSymbol
#define isTs			Rf_isTs
#define isUnordered		Rf_isUnordered
#define isUnsorted		Rf_isUnsorted
#define isUserBinop		Rf_isUserBinop
#define isValidName		Rf_isValidName
#define isValidString		Rf_isValidString
#define isValidStringF		Rf_isValidStringF
#define isVector		Rf_isVector
#define isVectorAtomic		Rf_isVectorAtomic
#define isVectorList		Rf_isVectorList
#define isVectorizable		Rf_isVectorizable
#define jump_to_toplevel	Rf_jump_to_toplevel
#define lang1			Rf_lang1
#define lang2			Rf_lang2
#define lang3			Rf_lang3
#define lang4			Rf_lang4
#define lastElt			Rf_lastElt
#define lcons			Rf_lcons
#define length(x)		Rf_length(x)
#define lengthgets		Rf_lengthgets
#define levelsgets		Rf_levelsgets
#define list1			Rf_list1
#define list2			Rf_list2
#define list3			Rf_list3
#define list4			Rf_list4
#define listAppend		Rf_listAppend
#define mainloop		Rf_mainloop
#define makeSubscript		Rf_makeSubscript
#define markKnown		Rf_markKnown
#define mat2indsub		Rf_mat2indsub
#define match			Rf_match
#define matchArg		Rf_matchArg
#define matchArgExact		Rf_matchArgExact
#define matchArgs		Rf_matchArgs
#define matchPar		Rf_matchPar
#define mkCLOSXP		Rf_mkCLOSXP
#define mkChar			Rf_mkChar
#define mkComplex               Rf_mkComplex
#define mkFalse		        Rf_mkFalse
#define mkFloat		        Rf_mkFloat
#define mkNA			Rf_mkNA
#define mkPROMISE		Rf_mkPROMISE
#define mkQUOTE		        Rf_mkQUOTE
#define mkSYMSXP		Rf_mkSYMSXP
#define mkString		Rf_mkString
#define mkTrue			Rf_mkTrue
#define namesgets		Rf_namesgets
#define ncols			Rf_ncols
#define nlevels			Rf_nlevels
#define nrows			Rf_nrows
#define nthcdr			Rf_nthcdr
#define onintr			Rf_onintr
#define onsigusr1               Rf_onsigusr1
#define onsigusr2               Rf_onsigusr2
#define parse			Rf_parse
#define pmatch			Rf_pmatch
#define promiseArgs		Rf_promiseArgs
#define protect			Rf_protect
#define psmatch			Rf_psmatch
#define rownamesgets		Rf_rownamesgets
#define setAttrib		Rf_setAttrib
#define setSVector		Rf_setSVector
#define setVar			Rf_setVar
#define sortVector		Rf_sortVector
#define ssort			Rf_ssort
#define str2type		Rf_str2type
#define substitute		Rf_substitute
#define substituteList		Rf_substituteList
#define translateChar		Rf_translateChar
#define tsConform		Rf_tsConform
#define tspgets		        Rf_tspgets
#define type2char		Rf_type2char
#define type2str		Rf_type2str
#define type2symbol		Rf_type2symbol
#define unbindVar		Rf_unbindVar
#define unprotect		Rf_unprotect
#define unprotect_ptr		Rf_unprotect_ptr
#define usemethod		Rf_usemethod
#define vectorSubscript	        Rf_vectorSubscript
#define warningcall		Rf_warningcall
#define warningcall		Rf_warningcall
#define warningcall_immediate	Rf_warningcall_immediate
#define yychar			Rf_yychar
#define yylval			Rf_yylval
#define yynerrs		        Rf_yynerrs
#define yyparse		        Rf_yyparse

#endif

#endif /* RF_NAMESPACE_H */
