/* tprs.h: thin plate regression splines*/
#ifndef MATRIX_HEADER_IN
#include "matrix.h"
#endif

/*extern double eta(int m,int d,double r);*/
void tpsE(matrix *E,matrix *X,int m,int d);

void tpsT(matrix *T,matrix *X,int m,int d);
double tps_g(matrix *X,matrix *p,double *x,int d,int m,double *b,int constant);
void tprs_setup(double **x,double **knt,int m,int d,int n,int k,int constant,matrix *X,matrix *S,
                matrix *UZ,matrix *Xu,int n_knots);
int null_space_dimension(int d,int m);
int *Xd_strip(matrix *Xd);



