/* SCCS @(#)fix_cp.c	1.2 02/08/98 */
/*
** When partition is done, each node is labeled with the complexity
**  appropriate if it were the top of the tree.  Actually, the complexity
**  should be min(me, any-node-above-me).  This routine fixes that.
*/
#include "rpart.h"
#include "node.h"
#include "rpartproto.h"

void fix_cp(struct node *me, double parent_cp)
    {
    if (me->complexity > parent_cp)  me->complexity = parent_cp;

    if (me->leftson != 0) {
	fix_cp(me->leftson, me->complexity);
	fix_cp(me->rightson,me->complexity);
	}
    }
