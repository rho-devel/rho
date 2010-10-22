#include <string>
#include <utility>
#include "CXXR/CachedString_serialization.hpp"
#include "CXXR/CachedString.h"
#include "CXXR/String.h"

namespace CXXR {
    std::pair<std::string, cetype_t> decomposeCachedString(const CachedString* pcs) {
	std::pair<std::string, cetype_t> pr;

	pr.first=pcs->stdstring();
	pr.second=pcs->encoding();

	return pr;
    }

    GCNode* composeCachedString(const std::string str, const cetype_t encoding) {
	return CachedString::obtain(str, encoding);
    }
}
