/*
**  SCCS  @(#)rpartproto.h	1.5 06/06/01
** prototypes for all of the rpart functions
**   This helps the ansi compiler do tight checking.
**
*/

struct node *branch(struct node *tree, int obs);

void bsplit(struct node *me, int nodenum);

void choose_surg(int nodenum,    int *y,            FLOAT *x,     Sint *order, 
		 int ncat,       double *agreement, FLOAT *split, int *csplit,
		 double ltot,    double rtot,       double *adj);

void fix_cp(struct node *me, double parent_cp);

void free_tree(struct node *node,  int freenode);

void graycode_init0( int maxcat);
void graycode_init1( int numcat, int *count);
void graycode_init2( int numcat, int *count, double *val);
int  graycode(void);

struct split *insert_split(struct split **listhead, int ncat, 
			   double improve,          int max);

void make_cp_list(struct node *me, double parent, 
		  struct cptable *cptable_head);

struct cptable *make_cp_table(struct node *me, double parent, int nsplit);

void mysort(int start, int stop, FLOAT *x, int *cvec);

void nodesplit(struct node *me, int nodenum);

int partition(int nodenum, struct node *splitnode, double *sumrisk);

void pred_rpart(Sint *dimx,	Sint *nnode, 	Sint *nsplit, 	Sint *dimc, 
		Sint *nnum,  	Sint *nodes2,   Sint *vnum,     double *split2,
		Sint *csplit2,  Sint *usesur,   double *xdata2, 
		Sint *xmiss2,   Sint *where);

int rpart(int n,         int nvarx,      Sint *ncat,     int method, 
          int  maxpri,   double *parms,  double *ymat,   FLOAT *xmat,
          Sint *missmat, struct cptable *cptable,
	  struct node **tree,            char **error,   int *which,
	  int xvals,     Sint *x_grp,    double *wt,     double *opt,
	  int ny,        double *cost) ;

void rpart_callback0(int *nr);
void rpart_callback1(int n, double *y[], double *wt, double *z);

void rpart_callback2(int n, int ncat, double *y[], double *wt, 
		     FLOAT *x, double *good);

void rpcountup(struct node *me, Sint *nnode, Sint *nsplit, int *ncat);

void rplabel(Sint *nsplit,   Sint *index,   double *splits, 
             Sint *ncat,     Sint *csplit,  char   **cutleft, char **cutright);

void rpmatrix(struct node *me,  Sint *nodecount,   Sint *splitcount, 
	      Sint *catcount,   Sint *numcat,      double **dsplit,
	      Sint **isplit,    Sint **csplit,     double **dnode, 
	      Sint **inode,     int id);

void rundown(struct node *tree,  int obs,     double *cp, 
	     double *xpred,      double *xtemp);

void rundown2(struct node *tree, int obs, double *cp, double *xpred);

void s_to_rp(Sint *n, 	  Sint *nvarx, 	 Sint *ncat, 	Sint *method, 
	     double *opt, double *parms, Sint *xvals,   Sint *x_grp,
	     double *y,   FLOAT  *xmat,  Sint *missmat, char **error,
	     double *wt,  Sint   *ny,    double *cost);

void s_to_rp2(Sint *n,         Sint *nsplit,    Sint *nnode,     Sint *ncat, 
	      Sint *numcat,    Sint *maxcat,    Sint *xvals,     Sint *which, 
	      double *cptable, double *dsplit,  Sint *isplit,    Sint *csplit,
	      double *dnode,   Sint *inode);

void s_xpred(Sint *sn, 	   Sint *nvarx,   Sint *ncat,    Sint *method, 
	     double *opt,  double *parms, Sint *xvals,   Sint *x_grp,
	     double *ymat, FLOAT  *xmat,  Sint *missmat, double *predict,
	     Sint *ncp,    double *cp,    char **error,  double *wt,
	     Sint *ny,     double *cost);

void surrogate(struct node *me, int nodenum);

void xval(int n_xval,  struct cptable *cptable_head,  Sint *x_grp, 
	  int maxcat,  char **error,                  double * parms);
