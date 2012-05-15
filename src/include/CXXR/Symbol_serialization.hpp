#ifndef SYMBOL_SERIALIZATION_HPP
#define SYMBOL_SERIALIZATION_HPP

#include <string>
#include <boost/serialization/nvp.hpp>

#include "CXXR/BSerializer.hpp"

namespace CXXR {
    class GCNode;
    class RObject;
    class Symbol;

    // For distinguishing 'Special' symbols.
    enum SymbolSerializationType {OTHERSYM=0, MISSINGARGSYM, UNBOUNDVALUESYM};

    // Forward declarations, defined in .cpp
    Symbol* composeSymbol(const SymbolSerializationType, const std::string&);
    const char* decomposeSymbol(const Symbol*);
    SymbolSerializationType symbolSerializationType(const Symbol*);

    template<class Archive>
    void saveSymbol(Archive & ar, const GCNode* pcs) {
	BSerializer::Frame frame("Symbol");
	const Symbol* sym=static_cast<const Symbol*>(pcs);
	SymbolSerializationType type=symbolSerializationType(sym);
	std::string strSym(decomposeSymbol(sym));

	ar << BOOST_SERIALIZATION_NVP(type);
	ar << BOOST_SERIALIZATION_NVP(strSym);
    }

    template<class Archive>
    Symbol* loadSymbol(Archive & ar) {
	BSerializer::Frame frame("Symbol");
	SymbolSerializationType type;
	std::string strSym;

	ar >> BOOST_SERIALIZATION_NVP(type);
	ar >> BOOST_SERIALIZATION_NVP(strSym);
	return composeSymbol(type, strSym);
    }
}

#endif
