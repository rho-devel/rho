/* qp.h : this is the header file for the quadratic programming unit.
   Last Revision 4/3/00 */

matrix addconQT(matrix *Q,matrix T,matrix a,matrix *u);
void GivensAddconQT(matrix *Q,matrix *T,matrix *a,matrix *s,matrix *c);

void QPCLS(matrix *Z,matrix *X, matrix *p, matrix *y,matrix *Ain,matrix *b,matrix *Af,int *active);
void PCLS(matrix *X,matrix *p,matrix *y,matrix *w,matrix *Ain,matrix *b,
          matrix *Af,matrix *H,matrix *S,int *off,double *theta,int m, int *active);


/* A global data structure for exporting information from this module for
   output by other modules (possibly not to the console) */
typedef struct
{ long constraints;
  double obj, obj_change;
  int converged;
} qpoutdatatype;
