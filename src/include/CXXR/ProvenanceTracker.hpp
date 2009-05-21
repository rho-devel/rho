#ifndef PROVENANCETRACKER_HPP
#define PROVENANCETRACKER_HPP

#ifdef __cplusplus

#include "CXXR/SchwarzCounter.hpp"
#include "CXXR/GCEdge.hpp"
#include "CXXR/Parentage.hpp"
#include "CXXR/Expression.h"
#include "CXXR/Frame.hpp"

namespace CXXR {
	class ProvenanceTracker {
	public:
	static Parentage* parentage();
	static void resetParentage();

	static Expression* expression();
	static void setExpression(Expression*);

	static void readMonitor(const Frame::Binding&);
	static void writeMonitor(const Frame::Binding&);

	private:
	ProvenanceTracker();
	static GCRoot<Parentage>* p_current;
	static GCRoot<Expression>* e_current;

	// Required for SchwarzCounter
	static void cleanup();
	static void initialize();
	friend class SchwarzCounter<ProvenanceTracker>;
	};
} // namespace CXXR

namespace {
	CXXR::SchwarzCounter<CXXR::ProvenanceTracker> provtrack_schwarz_ctr;
}

#endif // CPP

#endif // PROVENANCETRACKER_HPP
