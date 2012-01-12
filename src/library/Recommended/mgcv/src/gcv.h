#ifndef MATRIX_HEADER_IN
#include "matrix.h"
#endif
typedef struct
{ double 
  conv_tol, /* convergence tolerance for GCV minimisation */
  min_edf,  /* minimum possible model edf - useful for bracketing global smoothing parameter search */
  target_edf; /* If positive then the overall smoothing parameter search returns the local minimum "closest" to this: -ve to ignore */
  int max_step_half; /* How many times to halve steepest descent steps, befor giving up. */
} msctrl_type;

typedef struct
{ double *g, /* gradient of score w.r.t. s.p. at convergence */
         *h, /* leading diagonal of Hessian at convergence */
	 *e, /* eigenvalues of Hessian at convergence */
         *edf, /* edf's tried for direct overall s.p. search */
         *score; /* ubre or gcv score at edf values */
  int m,    /* number of smooths */
	  inok, /* was the second guess at parameters ok (if autoinit) */
	  iter, /* number of iterations taken to converge */
	  step_fail; /* 1 if last step was a step failure, 0 if terminated by meeting convergence criteria */ 
} msrep_type;



double TrInf(matrix *X,matrix *Z,matrix *w,matrix *S,double rho);
double EScv(double *ldt,matrix *T,matrix *l0,matrix *l1,matrix *x,double nx,matrix *z,double r,long n,
            double *trace,double *ress,double *sig2);
double EasySmooth(matrix *T,matrix *z,double *v,double *df,long n,double *sig2,double tol,int mesh,double *edf,double *score,
                  double min_edf,double target_edf,double low_edf);
double SingleSmooth(matrix *y,matrix *X,matrix *Z,matrix *w,matrix *S,matrix *p,
                    double *df,double *sig2);
void init_msrep(msrep_type *msrrep,int m,int direct_mesh);
void free_msrep(msrep_type *msrrep);
double MultiSmooth(matrix *y,matrix *J,matrix *Z,matrix *w,matrix *S,matrix *p,
                 double *theta,long *off,int m,double *sig2,msctrl_type *msctrl,msrep_type *msrep,int direct_mesh,double *trA);
void MSmooth(double ft(int,int,int,double*,double*,int,int,int),
             matrix *y,matrix *J,matrix *Z,matrix *w,matrix *S,matrix *p,
             double *theta,long *off,int m,int mp,double *sig2,int transform);                 


