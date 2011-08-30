#ifndef SYMBOL_SERIALIZATION_HPP
#define SYMBOL_SERIALIZATION_HPP

#include <string>
#include "CXXR/BSerializer.hpp"

namespace CXXR {
    class GCNode;
    class RObject;
    class Symbol;

    // For distinguishing 'Special' symbols
    enum SymbolSerializationType {OTHERSYM=0, MISSINGARGSYM,
                                  RESTARTTOKENSYM, UNBOUNDVALUESYM};

    // Forward declarations, defined in .cpp
    GCNode* composeSymbol(const SymbolSerializationType, const std::string&);
    const char* decomposeSymbol(const Symbol*);
    SymbolSerializationType symbolSerializationType(const Symbol*);

    template<class Archive>
    void saveSymbol(Archive & ar, const GCNode* pcs) {
	BSerializer::Frame frame("Symbol");
	const Symbol* sym=static_cast<const Symbol*>(pcs);
	SymbolSerializationType type=symbolSerializationType(sym);
	std::string strSym(decomposeSymbol(sym));

	ar << type;
	ar << strSym;
    }

    template<class Archive>
    GCNode* loadSymbol(Archive & ar) {
	BSerializer::Frame frame("Symbol");
	SymbolSerializationType type;
	std::string tmp;

	ar >> type;
	ar >> tmp;
	return composeSymbol(type, tmp);
    }
}

#endif
