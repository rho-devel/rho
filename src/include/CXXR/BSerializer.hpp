#ifndef BSERIALIZER_HPP
#define BSERIALIZER_HPP

#ifdef __cplusplus

#include <string>
#include "CXXR/SchwarzCounter.hpp"

namespace CXXR {
    class BSerializer {
    public:
	class Frame {
	public:
	    Frame(const char* cls) : s_class(cls)
	    {
		if (debugging())
		    BSerializer::start(s_class);
		++s_level;
	    }

	    ~Frame()
	    {
		--s_level;
		if (debugging())
		    BSerializer::stop(s_class);
	    }
	private:
	    std::string s_class;
	};
	
	static void attrib(const std::string&);
	static void debug(const std::string&);
	static bool debugging() { return s_debug_output; }
	static void indent();
	static int level() { return s_level; }
	static void start(const std::string&);
	static void stop(const std::string&);

    private:
	BSerializer() {}
	
	// Switch on/off debug output
	static bool s_debug_output;

	// Indenting level (stack count)
	static int s_level;

	// Required for SchwarzCounter
	static void cleanup();
	static void initialize();
	friend class SchwarzCounter<BSerializer>;
    };
} // namespace CXXR

namespace {
    CXXR::SchwarzCounter<CXXR::BSerializer> bserializer_schwarz_ctr;
}

#endif // CPP

#endif // BSERIALIZER_HPP
