#include "CXXR/StackChecker.hpp"

namespace CXXR {

namespace {
    const unsigned int R_MIN_EXPRESSIONS_OPT = 25;
    const unsigned int R_MAX_EXPRESSIONS_OPT = 500000;
}

unsigned int StackChecker::s_depth = 0;
unsigned int StackChecker::s_depth_limit = 5000;

void StackChecker::checkAvailableStackSpace(size_t required_bytes)
{
    if (R_CStackLimit == -1)
	return;

    char dummy;
    intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);
    if (usage + required_bytes > R_CStackLimit - R_CStackLimit / 16)
    {
	handleStackSpaceExceeded();
    }
}

void StackChecker::setDepthLimit(unsigned int depth)
{
    if (depth < R_MIN_EXPRESSIONS_OPT || depth > R_MAX_EXPRESSIONS_OPT)
    {
	Rf_error(_("'expressions' parameter invalid, allowed %d...%d"),
		 R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
    }
    s_depth_limit = depth;
}

void StackChecker::handleStackDepthExceeded()
{
    DisableStackCheckingScope no_stack_checking;
    Rf_errorcall(0, _("evaluation nested too deeply: "
		      "infinite recursion / options(expressions=)?"));
}	

void StackChecker::handleStackSpaceExceeded()
{
    // We'll need to use the remaining stack space for error recovery,
    // so temporarily disable stack checking.
    DisableStackCheckingScope no_stack_checking;
    
    // Do not translate this, to save stack space.
    Rf_errorcall(R_NilValue, "C stack usage is too close to the limit");
}

DisableStackCheckingScope::DisableStackCheckingScope()
{
    m_previous_limit = StackChecker::depthLimit();
    StackChecker::setDepthLimit(R_MAX_EXPRESSIONS_OPT);
    
    m_previous_stack_limit = R_CStackLimit;
    R_CStackLimit = -1;
}

DisableStackCheckingScope::~DisableStackCheckingScope()
{
    StackChecker::setDepthLimit(m_previous_limit);
    R_CStackLimit = m_previous_stack_limit;
}

}  // namespace CXXR
