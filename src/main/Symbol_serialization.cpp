#include "CXXR/Symbol_serialization.hpp"
#include "CXXR/Symbol.h"

namespace CXXR {
    const char* decomposeSymbol(Symbol* sym) {
	return sym->name()->c_str();
    }

    GCNode* composeSymbol(std::string& str) {
	return Symbol::obtain(str);
    }
}
