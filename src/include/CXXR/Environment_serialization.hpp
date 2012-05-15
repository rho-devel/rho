#ifndef ENVIRONMENT_SERIALIZATION_HPP
#define ENVIRONMENT_SERIALIZATION_HPP

#include <string>
#include <boost/serialization/nvp.hpp>

#include "CXXR/BSerializer.hpp"
#include "CXXR/GCNode.hpp"

namespace CXXR {
    class GCNode;
    class RObject;
    class Environment;

    // For distinguishing 'Special' environments
    enum EnvironmentSerializationType {OTHERENV=0, GLOBALENV, BASEENV};

    // Forward declarations, defined in .cpp
    const GCNode* composeEnvironment(const EnvironmentSerializationType,
                                     const Environment*);
    EnvironmentSerializationType environmentSerializationType(const Environment*);

    template<class Archive>
    void saveEnvironment(Archive & ar, const GCNode* pce) {
	BSerializer::Frame frame("Environment(Wrapper)");
    	Environment* env=const_cast<Environment*>(
	    static_cast<const Environment*>(pce)
	);
	EnvironmentSerializationType type=environmentSerializationType(env);

	ar << BOOST_SERIALIZATION_NVP(type);
	if (type==OTHERENV)
	    ar << BOOST_SERIALIZATION_NVP(env);
    }

    template<class Archive>
    GCNode* loadEnvironment(Archive & ar) {
	BSerializer::Frame frame("Environment(Wrapper)");
	EnvironmentSerializationType type;
	Environment* env=NULL;

	ar >> BOOST_SERIALIZATION_NVP(type);
	if (type==OTHERENV)
	    ar >> BOOST_SERIALIZATION_NVP(env);
	return const_cast<GCNode*>(composeEnvironment(type, env));
    }
}

#endif
