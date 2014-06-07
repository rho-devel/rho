/*CXXR $Id$
 *CXXR
 *CXXR This file is part of CXXR, a project to refactor the R interpreter
 *CXXR into C++.  It may consist in whole or in part of program code and
 *CXXR documentation taken from the R project itself, incorporated into
 *CXXR CXXR (and possibly MODIFIED) under the terms of the GNU General Public
 *CXXR Licence.
 *CXXR 
 *CXXR CXXR is Copyright (C) 2008-9 Andrew R. Runnalls, subject to such other
 *CXXR copyrights and copyright restrictions as may be stated below.
 *CXXR 
 *CXXR CXXR is not part of the R project, and bugs and other issues should
 *CXXR not be reported via r-bugs or other R project channels; instead refer
 *CXXR to the CXXR website.
 *CXXR */

/** @file Test of templated class HeterogeneousList.
 */

#include "CXXR/HeterogeneousList.hpp"

#include <algorithm>
#include <iostream>

using namespace std;
using namespace CXXR;

class Node : public HeterogeneousListBase::Link {
public:
    explicit Node(int i)
	: m_i(i)
    {}

    int get() const
    {
	return m_i;
    }
private:
    int m_i;

    ~Node()
    {}
};

void printnode(const Node* node)
{
    cout << node->get() << '\n';
}

int main() {
    HeterogeneousList<Node> list1, list2;
    Node* node1 = new Node(1);
    list1.splice_back(node1);
    const Node* node2 = new Node(2);
    list1.splice_back(node2);
    Node* node3 = new Node(3);
    list1.splice_back(node3);
    cout << "list1.front()->get() = " << list1.front()->get() << endl;
    cout << "list1.back()->get() = " << list1.back()->get() << endl;
    cout << "list2 is" << (list2.empty() ? " " : " not ") << "empty.\n";
    cout << "list1:\n";
    for_each(list1.begin(), list1.end(), printnode);
    cout << "list2.splice_back(list1.front())\n";
    list2.splice_back(list1.front());
    cout << "list1.front()->get() = " << list1.front()->get() << endl;
    cout << "list1.back()->get() = " << list1.back()->get() << endl;
    cout << "list2.front()->get() = " << list2.front()->get() << endl;
    cout << "list2.back()->get() = " << list2.back()->get() << endl;
    cout << "list2 is" << (list2.empty() ? " " : " not ") << "empty.\n";
    cout << "list1:\n";
    for_each(list1.begin(), list1.end(), printnode);
    cout << "list2.splice_back(&list1)\n";
    list2.splice_back(&list1);
    cout << "list1 is" << (list1.empty() ? " " : " not ") << "empty.\n";
    cout << "list1:\n";
    for_each(list1.begin(), list1.end(), printnode);
    cout << "list2.front()->get() = " << list2.front()->get() << endl;
    cout << "list2.back()->get() = " << list2.back()->get() << endl;
    cout << "list2 is" << (list2.empty() ? " " : " not ") << "empty.\n";
    return 0;
}


    
