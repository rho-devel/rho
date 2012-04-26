/* SCCS @(#)free_tree.c	1.2 02/08/98 */
/*
** free up all of the memory associated with a tree
*/
#include "rpart.h"
#include "node.h"
#include "rpartS.h"
#include "rpartproto.h"

void free_tree(struct node *node,  int freenode)
    {
    struct split *s1, *s2;

    if (node->rightson !=0) free_tree(node->rightson, 1);
    if (node->leftson  !=0) free_tree(node->leftson,  1);

    for (s1=node->surrogate; s1!=0; ){
	s2 = s1;
	s1 = s1->nextsplit;
	Free(s2);
	}
    for (s1=node->primary; s1!=0; ){
	s2 = s1;
	s1 = s1->nextsplit;
	Free(s2);
	}
    if (freenode==1) Free(node);
    }
