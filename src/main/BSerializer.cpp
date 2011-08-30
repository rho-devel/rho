#include <iostream>
#include <string>
#include "CXXR/BSerializer.hpp"

using namespace std;
using namespace CXXR;

bool BSerializer::s_debug_output;
int BSerializer::s_level;

void BSerializer::attrib(const std::string& a) {
    if (!debugging()) return;

    indent();
    cout << "Attribute " << a << ":" << std::endl;
}

void BSerializer::debug(const std::string& op) {
    if (!debugging()) return;

    indent();
    cout << op << std::endl;
}

void BSerializer::indent() {
    for (int i=0; i<level(); i++)
	cout << "  ";
}

void BSerializer::start(const std::string& c) {
    indent();
    cout << "<" << c << ">" << std::endl;
}

void BSerializer::stop(const std::string& c) {
    indent();
    cout << "</" << c << ">" << std::endl;
}

void BSerializer::cleanup() {
}

void BSerializer::initialize() {
    s_level=0;
#ifdef BSERIALIZEDEBUG
    debugOutput=true;
#endif
}
