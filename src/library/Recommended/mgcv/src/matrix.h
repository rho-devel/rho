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
matrix initmat(long rows,long cols);
void freemat(matrix A);
void vmult(matrix *A,matrix *b,matrix *c,int t);
void matmult(matrix C,matrix A,matrix B,int tA,int tB);
void multi(int n,matrix C, ...);
void invert(matrix *a);
void tricholeski(matrix *T,matrix *l0,matrix *l1);
double dot(matrix a,matrix b);
double enorm(matrix d);
void householder(matrix *u,matrix a,matrix b,long t1);
void Hmult(matrix C,matrix u);
void HQmult(matrix C,matrix U,int p,int t);
void QT(matrix Q,matrix A,int Qfull);
void Rsolv(matrix *R,matrix *p,matrix *y, int transpose);
int QR(matrix *Q,matrix *R);
void UTU(matrix *T,matrix *U);
void bidiag(matrix *A,matrix *wl,matrix *ws,matrix *V);
void OrthoMult(matrix *Q,matrix *A,int off,int rows,int t,int pre,int o_pre);
void root(matrix *M,matrix *C,double tol);
void matrixintegritycheck(void);
double mean(matrix a);
double pythag(double a,double b);
void svd(matrix *a,matrix *w,matrix *v);
matrix svdroot(matrix A,double reltol);
void svd_bidiag(matrix *U, matrix *w, matrix *ws,matrix *V);
void msort(matrix a);
void RArrayFromMatrix(double *a,long r,matrix *M);
matrix Rmatrix(double *A,long r,long c);
#endif
