#ifndef CACHEDSTRING_SERIALIZATION_HPP
#define CACHEDSTRING_SERIALIZATION_HPP

#include <string>
#include <utility>
#include "CXXR/BSerializer.hpp"
#include "CXXR/StringEncodingType.h"

namespace CXXR {
    class GCNode;
    class RObject;
    class CachedString;

    // Prototypes of functions defined in CachedString_serialization.cpp
    GCNode* composeCachedString(const std::string, const cetype_t);
    std::pair<std::string, cetype_t> decomposeCachedString(const CachedString*);

    template<class Archive>
    void saveCachedString(Archive & ar, const GCNode* pcs) {
	BSerializer::Frame frame("CachedString");
	std::pair<std::string, cetype_t> pr
	    =decomposeCachedString(static_cast<const CachedString*>(pcs));
	ar << pr.first;
	ar << pr.second;
    }

    template<class Archive>
    GCNode* loadCachedString(Archive & ar) {
	BSerializer::Frame frame("CachedString");
	std::string str;
	cetype_t encoding;
	ar >> str;
	ar >> encoding;
	
	return composeCachedString(str, encoding);
    }
}

#endif
