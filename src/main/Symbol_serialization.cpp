#include "CXXR/Symbol_serialization.hpp"
#include "CXXR/Symbol.h"

namespace CXXR {
    SymbolSerializationType symbolSerializationType(const Symbol* sym) {
	if (sym==R_MissingArg) return MISSINGARGSYM;
	if (sym==R_UnboundValue) return UNBOUNDVALUESYM;
	return OTHERSYM;
    }


    const char* decomposeSymbol(const Symbol* sym) {
	return sym->name()->c_str();
    }

    Symbol* composeSymbol(const SymbolSerializationType type,
    			  const std::string& str) {
	switch(type) {
	case MISSINGARGSYM:
	    return Symbol::missingArgument();
	case UNBOUNDVALUESYM:
	    return Symbol::unboundValue();
	default:
	    return Symbol::obtain(str);
	}
    }
}
