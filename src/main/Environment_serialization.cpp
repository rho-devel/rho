#include "CXXR/Environment_serialization.hpp"
#include "CXXR/Environment.h"
#include "CXXR/Frame.hpp"

namespace CXXR {
    EnvironmentSerializationType environmentSerializationType(const Environment* env) {
    	if (env==R_GlobalEnv) return GLOBALENV;
	if (env==R_BaseEnv) return BASEENV;
	return OTHERENV;
    }

    const GCNode* composeEnvironment(const EnvironmentSerializationType type,
    			             const Environment* env) {
	switch(type) {
	case GLOBALENV: return R_GlobalEnv;
	case BASEENV: return R_BaseEnv;
	default: return env;
	}
    }
}
