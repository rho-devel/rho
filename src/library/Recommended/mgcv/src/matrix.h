/* matrix.h : header file for matrix routines.*/

#ifndef MATRIX_HEADER_IN
#define MATRIX_HEADER_IN
/* The basic matrix structure */
#define TOL 1e-10
#define VEC M[0]

typedef struct
{ int vec;long r,c,mem,original_r,original_c;double **M,*V;} matrix;

extern matrix null_mat;

extern long matrallocd;

/* The user routines */

void mtest(void);
void mcopy(matrix *A,matrix *B);
void sort(matrix);
long rank(matrix a);
matrix initmat(long rows,long cols);
void freemat(matrix A);
long fsafewrite(double *ptr,size_t size,long n,FILE *stream);
long fsaferead(double *ptr,size_t size,long n,FILE *stream);
void dumpmat(matrix M,char *filename);
void readmat(matrix *M,char *filename);
matrix vecmult(matrix A,matrix x,int t);
void vmult(matrix *A,matrix *b,matrix *c,int t);
void matmult(matrix C,matrix A,matrix B,int tA,int tB);
void multi(int n,matrix C, ...);
double trace(matrix *A);
void mad(matrix C,matrix A,matrix B,double mA,double mB);
void invert(matrix *a);
void bicholeskisolve(matrix *A,matrix *B,matrix *l0,matrix *l1);
double triTrInvLL(matrix *l0,matrix *l1);
void tricholeski(matrix *T,matrix *l0,matrix *l1);
void choleski(matrix A,matrix L,int invert,int invout);
int chol(matrix A,matrix L,int invert,int invout);
matrix choleskiupdate(matrix L,matrix a);
void choleskir1ud(matrix L,matrix u,double alpha);
void choleskisolve(matrix L,matrix z,matrix y);
double dot(matrix a,matrix b);
double enorm(matrix d);
double matrixnorm(matrix M);
double m1norm(matrix M);
double condition(matrix a);
void householder(matrix *u,matrix a,matrix b,long t1);
void Hmult(matrix C,matrix u);
void HQmult(matrix C,matrix U,int p,int t);
void QT(matrix Q,matrix A,int Qfull);
void InvertTriangular(matrix *R);
void Rsolv(matrix *R,matrix *p,matrix *y, int transpose);
int QR(matrix *Q,matrix *R);
void UTU(matrix *T,matrix *U);
void bidiag(matrix *A,matrix *wl,matrix *ws,matrix *V);
void OrthoMult(matrix *Q,matrix *A,int off,int rows,int t,int pre,int o_pre);
void root(matrix *M,matrix *C,double tol);
void symproduct(matrix A,matrix B,matrix C,int trace,int chol);
matrix getmask(int *index,int smalln,int bign);
void gettextmatrix(matrix M,char *name);
void matrixintegritycheck(void);
double variance(matrix a);
double cov(matrix a,matrix b);
double corr(matrix a,matrix b);
double acf(matrix s,int lag);
double absdev(matrix a);
double mean(matrix a);
void leastsq(matrix A,matrix p,matrix y,matrix w);
long alias(matrix X,long *aliased,double tol);
void notinv(matrix J,matrix K,matrix B);
void printmat(matrix A,char *fmt);
double pythag(double a,double b);
void svd(matrix *a,matrix *w,matrix *v);
void svdLS(matrix A,matrix p,matrix y,double tol);
void suminvert(matrix A,matrix B,matrix U,matrix W);
void specd(matrix U,matrix W);
void updateLS(matrix T,matrix z,matrix x,double y,double w);
void fullLS(matrix A,matrix p,matrix y,matrix w,matrix T,matrix zo,int TZout);
void rtsolve(matrix T,matrix p,matrix z);
long pinv(matrix *A,double trunc);
matrix svdroot(matrix A,double reltol);
void fprintmat(matrix A,char *fname,char *fmt);
void old_svd(matrix *A, matrix *w, matrix *V);    /* NOTE: delete eventually */
void svd_bidiag(matrix *U, matrix *w, matrix *ws,matrix *V);
int lanczos_spd(matrix *A, matrix *V, matrix *va,int m,int lm);
void eigenvv_tri(double *d,double *g,double **v, int n);
void eigen_tri(double *d,double *g,double **v,int n,int getvec);
void lu_tri(double *d,double *g,double *u,int n);
void msort(matrix a);
void RArrayFromMatrix(double *a,long r,matrix *M);
matrix Rmatrix(double *A,long r,long c);
#endif
