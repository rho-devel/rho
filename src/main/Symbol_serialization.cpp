#include "CXXR/Symbol_serialization.hpp"
#include "CXXR/Symbol.h"

namespace CXXR {
    const char* decomposeSymbol(const Symbol* sym) {
	return sym->name()->c_str();
    }

    GCNode* composeSymbol(const std::string& str) {
	return Symbol::obtain(str);
    }
}
