/* This is a file used to develop and test interfacing routines for 
   Lapack and Linpack. In particular the aim is to check that I can reproduce
   R routines such as chol and La.svd by direct calls to Lapack and Linpack.

   They have been tested against R routines, (but R's qr() routine was buggy, so only
   unpivoted case was testable).

   See R-x/include/R_ext/Lapack.h...
*/
#include "mgcv.h"
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <R_ext/Linpack.h>
#include <R_ext/Lapack.h>
/*#include <dmalloc.h>*/

void mgcv_chol(double *a,int *pivot,int *n,int *rank)
/* a stored in column order, this routine finds the pivoted choleski decomposition of matrix a 
   library(mgcv)
   X<-matrix(rnorm(16),4,4);D<-X%*%t(X)
   rank<-0
   er<-.C("mgcv_chol",as.double(D),as.integer(rep(0,4)),as.integer(4),as.integer(rank))
   rD<-matrix(er[[1]],4,4);piv<-er[[2]]
   chol(D,pivot=TRUE);rD;piv
   n<-length(piv);ind<-1:n;
   ind[piv]<-1:n
   rD<-rD[,ind]
   L<-mroot(D)
   D;t(rD)%*%rD;L%*%t(L)
*/
{ double *work,*p1,*p2,*p;
  int piv=1;
  work=(double *)calloc((size_t) *n,sizeof(double));
  F77_NAME(dchdc)(a,n,n,work,pivot,&piv,rank);
  /* zero stuff below the leading diagonal */
  for (p2=a+ *n,p1=a+1;p2<a+ *n * *n;p1+= *n+1,p2+= *n) for (p=p1;p<p2;p++) *p=0.0;
  free(work);
}

void mroot(double *A,int *rank,int *n)
/* finds the minimum rank or supplied rank square root of n by n matrix  A by pivoted choleski 
   decomposition returned in A is B such that B'B = A (this is different from R routine mroot)

   R testing code....
   library(mgcv)
   X<-matrix(rnorm(12),4,3);D<-X%*%t(X)
   rank<-0
   er<-.C("mroot",as.double(D),as.integer(rank),as.integer(4))
   rD<-matrix(er[[1]],er[[2]],4)
   D;t(rD)%*%rD
   
*/
{ int *pivot,erank,i,j;
  double *pi,*pj,*p0,*p1,*B;
  pivot=(int *)calloc((size_t)*n,sizeof(int));
  mgcv_chol(A,pivot,n,&erank);
  if (*rank<=0) *rank = erank;
  /* unscramble the returned choleski factor */ 
  B=calloc((size_t)(*n * *n),sizeof(double));
  /* copy A to B and zero A */
  for (p0=A,p1=B,i=0;i< *n;i++,p0+= *n,p1+= *n)
  for (pi=p0,pj=p1;pi<=p0+i;pi++,pj++) { *pj = *pi; *pi=0.0;}
  /* now copy B into A undoing pivoting */
  for (p0=B,i=0;i< *n;p0+= *n,i++) 
  for (j=pivot[i],pj=p0,pi=A + (j-1)* *n;pj<=p0+i;pi++,pj++) *pi = *pj;
  /* remove trailing rows from choleski factor */
  for (pi=A,p0=A,i=0;i< *n;i++,p0+= *n)
  for (pj=p0;pj<p0+ *rank;pj++,pi++) *pi = *pj; 
  free(pivot);free(B);
}


void mgcv_svd(double *x,double *u,double *d,int *r,int *c)
/* call LA_PACK svd routine to form x=UDV'. Assumes that V' not wanted, but that full square U is required.
*/
{ const char jobu='A',jobvt='N';
  int lda,ldu,ldvt=1,lwork;
  int info;
  double *vt=NULL,work1,*work;
  ldu=lda= *r;
  lwork=-1;
  /* workspace query */
  F77_NAME(dgesvd)(&jobu,&jobvt, r, c, x, &lda, d, u, &ldu, vt,&ldvt,
  		   &work1, &lwork, &info);
  lwork=(int)floor(work1);
  if (work1-lwork>0.5) lwork++;
  work=(double *)calloc((size_t)lwork,sizeof(double));
  /* actual call */
  F77_NAME(dgesvd)(&jobu,&jobvt, r, c, x, &lda, d, u, &ldu, vt,&ldvt,
  		   work, &lwork, &info);
  free(work);
}

void mgcv_svd_full(double *x,double *vt,double *d,int *r,int *c)
/* call LA_PACK svd routine to form x=UDV'. U returned in x. V' returned in vt.
   assumed r > c. U is r by c. D is length c. V is c by c.

# Here is R test code.....
library(mgcv)
n<-4;q<-3
X<-matrix(rnorm(n*q),n,q)
um<-.C("mgcv_svd_full",as.double(X),double(q*q),double(q),as.integer(n),as.integer(q),
        PACKAGE="mgcv")
er<-La.svd(X)
matrix(um[[1]],n,q);er$u
um[[3]];er$d
matrix(um[[2]],q,q);er$v
*/
{ const char jobu='O',jobvt='A';
  int lda,ldu,ldvt,lwork;
  int info;
  double work1,*work,*u=NULL;
  ldu=lda= *r;ldvt = *c;
  lwork=-1;
  /* workspace query */
  F77_NAME(dgesvd)(&jobu,&jobvt, r, c, x, &lda, d, u, &ldu, vt,&ldvt,
  		   &work1, &lwork, &info);
  lwork=(int)floor(work1);
  if (work1-lwork>0.5) lwork++;
  work=(double *)calloc((size_t)lwork,sizeof(double));
  /* actual call */
  F77_NAME(dgesvd)(&jobu,&jobvt, r, c, x, &lda, d, u, &ldu, vt,&ldvt,
  		   work, &lwork, &info);
  free(work);
}


void mgcv_td_qy(double *S,double *tau,int *m,int *n, double *B,int *left,int *transpose)
/* Multiplies m by n matrix B by orthogonal matrix returned from mgcv_tri_diag and stored in 
   S, tau. B is overwritten with result. 
    
   Note that this is a bit inefficient if really only a few rotations matter!

   Calls LAPACK routine dormtr 
*/
{ char trans='N',side='R',uplo='U';
  int nq,lwork=-1,info;
  double *work,work1;
  if (*left) { side = 'L';nq = *m;} else nq = *n;
  if (*transpose) trans = 'T';
  /* workspace query ... */
  F77_NAME(dormtr)(&side,&uplo,&trans,m,n,S,&nq,tau,B,m,&work1,&lwork,&info);
  lwork=(int)floor(work1);if (work1-lwork>0.5) lwork++;
  work=(double *)calloc((size_t)lwork,sizeof(double));
  /* actual call ... */
  F77_NAME(dormtr)(&side,&uplo,&trans,m,n,S,&nq,tau,B,m,work,&lwork,&info);
  free(work);
}

void mgcv_tri_diag(double *S,int *n,double *tau)
/* Reduces symmetric n by n matrix S to tridiagonal form T 
   by similarity transformation. Q'SQ = T, where Q is an
   orthogonal matrix. Only the upper triangle of S is actually
   used.

   On exit the diagonal and superdiagonal of T are written in 
   the corresponding position in S. The elements above the first 
   superdiagonal, along with tau, store the householder reflections 
   making up Q. 

   Note that this is not optimally efficient if actually only a 
   few householder rotations are needed because S does not have 
   full rank.

   The routine calls dsytrd from LAPACK.
     
*/
{ int lwork=-1,info;
  double *work,work1,*e,*d;
  char uplo='U';
  d = (double *)calloc((size_t)*n,sizeof(double));
  e = (double *)calloc((size_t)*n-1,sizeof(double));
  /* work space query... */
  F77_NAME(dsytrd)(&uplo,n,S,n,d,e,tau,&work1,&lwork,&info);
  lwork=(int)floor(work1);if (work1-lwork>0.5) lwork++;
  work=(double *)calloc((size_t)lwork,sizeof(double));
  /* Actual call... */
  F77_NAME(dsytrd)(&uplo,n,S,n,d,e,tau,work,&lwork,&info);
  free(work);free(d);free(e);
}


void mgcv_backsolve(double *R,int *r,int *c,double *B,double *C, int *bc) 
/* Finds C = R^{-1} B where R is the c by c matrix stored in the upper triangle 
   of r by c argument R. B is c by bc. (Possibility of non square argument
   R facilitates use with output from mgcv_qr). This is just a standard back 
   substitution loop.
*/  
{ int i,j,k;
  double x,*pR,*pC;
  for (j=0;j<*bc;j++) { /* work across columns of B & C */
    for (i = *c-1;i>=0;i--) { /* work up each column of B & C */
      x = 0.0;
      /* for (k=i+1;k<*c;k++) x += R[i + *r * k] * C[k + j * *c]; ...following replaces...*/
      pR = R + i + (i+1) * *r;pC = C + j * *c + i + 1;
      for (k=i+1;k<*c;k++,pR+= *r,pC++) x += *pR * *pC;      
      C[i + j * *c] = (B[i + j * *c] - x)/R[i + *r * i];
    }
  }
}

void mgcv_forwardsolve(double *R,int *r,int *c,double *B,double *C, int *bc) 
/* Finds C = R^{-T} B where R is the c by c matrix stored in the upper triangle 
   of r by c argument R. B is c by bc. (Possibility of non square argument
   R facilitates use with output from mgcv_qr). This is just a standard forward 
   substitution loop.
*/  
{ int i,j,k;
  double x;
  for (j=0;j<*bc;j++) { /* work across columns of B & C */
    for (i = 0;i< *c;i++) { /* work down each column of B & C */
      x=0.0;
      for (k=0;k<i;k++) x+= C[k + j * *c] * R[k + i * *r]; 
      C[i + j * *c] = (B[i + j * *c] - x) / R[i + i * *r]; 
    }
  }
}


void mgcv_qr(double *x, int *r, int *c,int *pivot,double *tau)
/* call LA_PACK to get pivoted QR decomposition of x
   tau is an array of length min(r,c)
   pivot is array of length c, zeroed on entry, pivoting order on return.
   On exist upper triangle of x is R. Below upper triangle plus tau 
   represent reflectors making up Q.
   pivoting is always performed (not just when matrix is rank deficient), so
   leading diagonal of R is in descending order of magnitude.
   library(mgcv)
   r<-4;c<-3
   X<-matrix(rnorm(r*c),r,c)
   pivot<-rep(1,c);tau<-rep(0,c)
   um<-.C("mgcv_qr",as.double(X),as.integer(r),as.integer(c),as.integer(pivot),as.double(tau))
   qr.R(qr(X));matrix(um[[1]],r,c)[1:c,1:c]
*/
{ int info,lwork=-1,*ip;
  double work1,*work;
  /* workspace query */
  F77_NAME(dgeqp3)(r,c,x,r,pivot,tau,&work1,&lwork,&info);
  lwork=(int)floor(work1);if (work1-lwork>0.5) lwork++;
  work=(double *)calloc((size_t)lwork,sizeof(double));
   /* actual call */
  F77_NAME(dgeqp3)(r,c,x,r,pivot,tau,work,&lwork,&info); 
  free(work);
  /*if (*r<*c) lwork= *r; else lwork= *c;*/ 
  for (ip=pivot;ip < pivot + *c;ip++) (*ip)--;
  /* ... for 'tis C in which we work and not the 'cursed Fortran... */
  
}


void mgcv_qrqy(double *b,double *a,double *tau,int *r,int *c,int *k,int *left,int *tp)
/* applies k reflectors of Q of a QR decomposition to r by c matrix b.
   Apply Q from left if left!=0, right otherwise.
   Transpose Q only if tp!=0.
   Information about Q has been returned from mgcv_qr, and is stored in tau and 
   below the leading diagonal of a.
   library(mgcv)
   r<-4;c<-3
   X<-matrix(rnorm(r*c),r,c)
   qrx<-qr(X)
   pivot<-rep(1,c);tau<-rep(0,c)
   um<-.C("mgcv_qr",a=as.double(X),as.integer(r),as.integer(c),as.integer(pivot),tau=as.double(tau))
   y<-1:4;left<-1;tp<-0;cy<-1
   er<-.C("mgcv_qrqy",as.double(y),as.double(um$a),as.double(um$tau),as.integer(r),as.integer(cy),as.integer(c),
        as.integer(left),as.integer(tp),PACKAGE="mgcv")
   er[[1]];qr.qy(qrx,y)
   
*/

{ char side='L',trans='N';
  int lda,lwork=-1,info;
  double *work,work1;
 
  if (! *left) { side='R';lda = *c;} else lda= *r;
  if ( *tp) trans='T'; 
  /* workspace query */
  F77_NAME(dormqr)(&side,&trans,r,c,k,a,&lda,tau,b,r,&work1,&lwork,&info);
  lwork=(int)floor(work1);if (work1-lwork>0.5) lwork++;
  work=(double *)calloc((size_t)lwork,sizeof(double));
  /* actual call */
  F77_NAME(dormqr)(&side,&trans,r,c,k,a,&lda,tau,b,r,work,&lwork,&info); 
  free(work);
   
}


void update_qr(double *Q,double *R,int *n, int *q,double *lam, int *k)
/* Let X=QR where X is n by q and R is q by q upper triangular and Q is n by q.
   A single element extra row, x, is to be appended to X and Q and R updated 
   accordingly. x is zero except for kth element lam.

   Let Q* be the full orthogonal matrix of which Q is the upper left portion, then 
   
   [X] = [Q* 0][R]
   [x]   [0  1][0]
               [x]

   The rhs of the above can be bought into standard QR form by application of givens rotations 
   from the left to the augmented R matrix and the corresponding inverse rotations from the right 
   to the augmented Q* matrix. The rotations from the right applied to the augmented Q* have the 
   effect of rotating columns of Q into the final column of the augmented matrix and vice-versa.

   Since the columns between Q and the final column are not involved, only Q and R need to be updated 
   here, the rest of Q* being irrelevant. This routine does not augment the Q by an extra row, it is 
   assumed that the calling function only requires the update of the input rows.

   All matrices are assumed to be packed in column order. Some very minor further optimizations could 
   be added (e.g. using fact that working and most of x are zero at first iteration), but it's really 
   unlikely to yield a worthwhile saving.

   Some R code for testing the routine:

library(mgcv)
n<-4;q<-3
X<-matrix(rnorm(n*q),n,q)
#X[,q-1]<-X[,q]
qrx<-qr(X,tol=0)
Q<-qr.Q(qrx);R<-qr.R(qrx);lam<-1
um<-.C("update_qr",as.double(Q),as.double(R),as.integer(n),as.integer(q),
        as.double(lam),as.integer(q-1),PACKAGE="mgcv")
R1<-matrix(um[[2]],q,q);Q1<-matrix(um[[1]],n,q)
Xa<-matrix(0,n+1,q)
Xa[1:n,]<-X;Xa[n+1,q]<-lam
qr.R(qr(Xa,tol=0))   

*/

{ double *x,*work,c,s,r,x0,x1,m,*xip,*xjp,*riip,*rijp,*Qp,*wp;
  x=(double *)calloc((size_t)*q,sizeof(double)); 
  work=(double *)calloc((size_t)*n,sizeof(double)); /* working extra column of Q */
  x[*k] = *lam;
  /* conceptually i runs from k to q in the following loop */
  for (Qp=Q+ *k * *n,riip=R+ *k * *q + *k,xip=x+ *k ;xip< x+ *q;xip++,riip+= *q+1)
  { /* rotate x[i] into R[i,i], using over/underflow proof rotator */
    x0= *xip; /* x[i] */
    x1= *riip; /* R[1 * *q + i] */
    m = fabs(x0);s=fabs(x1); if (s>m) m=s;
    x1/=m;x0/=m;
    r=sqrt(x0*x0+x1*x1);
    c=x1/r;s=x0/r;
    *riip=m*r;/* *xip=0.0; but never neaded*/    
    /* conceptually j runs from i+1 to q in the following loop */
    for (rijp=riip + *q,xjp=xip+1;xjp<x+ *q;xjp++,rijp+= *q) /* apply rotation to x[j], R[i,j] */
    { x1= *rijp;    /* R[j* *q+i] */
      /* *xjp == x[j] */
      *rijp = c*x1 - s* *xjp;
      *xjp = s*x1 + c* *xjp;
    }
    /* apply transpose of rotation from right to Q */
    /* conceptually j runs from 0 to n in following loop */
    for (wp=work;wp<work+ *n;wp++,Qp++)
    { x1 = *Qp;   /*Q[i* *n+j]*/
      /* *wp == work[j] */
      *Qp = c*x1-s* *wp;
      *wp = s*x1+c* *wp;
    }
  }
  free(x);free(work);
}

void mgcv_symeig(double *A,double *ev,int *n,int *use_dsyevd,int *get_vectors,
                 int *descending)

/* gets eigenvalues and (optionally) vectors of symmetric matrix A. 
   
   Two alternative underlying LAPACK routines can be used, 
   either dsyevd (slower, robust) or dsyevr (faster, seems less robust). 
   Vectors returned in columns of A, values in ev (ascending).
   
   ******************************************************
   *** Eigenvalues are returned  in *ascending* order ***
   *** unless descending is set to be non-zero        ***
   ******************************************************

   Testing R code....
   library(mgcv)
   n<-4;A<-matrix(rnorm(n*n),n,n);A<-A%*%t(A);d<-array(0,n)
   er<-eigen(A)
   um<-.C("mgcv_symeig",as.double(A),as.double(d),as.integer(n),
           as.integer(1),as.integer(1),PACKAGE="mgcv")
   er$vectors;matrix(um[[1]],n,n)
   er$values;um[[2]]
*/  

{ char jobz='V',uplo='U',range='A'; 
  double work1,*work,dum1=0,abstol=0.0,*Z,*dum2,x,*p;
  int lwork = -1,liwork = -1,iwork1,info,*iwork,dumi=0,n_eval=0,*isupZ,i;
  if (*get_vectors) jobz='V'; else jobz='N';
  if (*use_dsyevd)
  { F77_NAME(dsyevd)(&jobz,&uplo,n,A,n,ev,&work1,&lwork,&iwork1,&liwork,&info);
    lwork=(int)floor(work1);if (work1-lwork>0.5) lwork++;
    work=(double *)calloc((size_t)lwork,sizeof(double));
    liwork = iwork1;iwork= (int *)calloc((size_t)liwork,sizeof(int));
    F77_NAME(dsyevd)(&jobz,&uplo,n,A,n,ev,work,&lwork,iwork,&liwork,&info);
    free(work);free(iwork);
  } else
  { Z=(double *)calloc((size_t)(*n * *n),sizeof(double)); /* eigen-vector storage */
    isupZ=(int *)calloc((size_t)(2 * *n),sizeof(int)); /* eigen-vector support */
    F77_NAME(dsyevr)(&jobz,&range,&uplo,
		   n,A,n,&dum1,&dum1,&dumi,&dumi,
		   &abstol,&n_eval,ev, 
    		     Z,n,isupZ, &work1,&lwork,&iwork1,&liwork,&info);
    lwork=(int)floor(work1);if (work1-lwork>0.5) lwork++;
    work=(double *)calloc((size_t)lwork,sizeof(double));
    liwork = iwork1;iwork= (int *)calloc((size_t)liwork,sizeof(int));
    F77_NAME(dsyevr)(&jobz,&range,&uplo,
		   n,A,n,&dum1,&dum1,&dumi,&dumi,
		   &abstol,&n_eval,ev, 
    		     Z,n,isupZ, work,&lwork,iwork,&liwork,&info);
    free(work);free(iwork);
    
    if (*descending) for (i=0;i<*n/2;i++) { /* reverse the eigenvalues */
      x = ev[i]; ev[i] = ev[*n-i-1];ev[*n-i-1] = x;
    }

    if (*get_vectors) {  /* copy vectors back into A */
      if (*descending) { /* need to reverse order */
        dum2 = Z + *n * (*n-1);
        for (work=dum2;work>=Z;work -= *n)
	  for (p=work;p<work + *n;p++,A++) *A = *p; 
      } else { /* simple copy */
        dum2 = Z + *n * *n;
        for (work=Z;work<dum2;work++,A++) *A = *work;
      }
    }
    free(Z);free(isupZ);
  }

 }

void mgcv_trisymeig(double *d,double *g,double *v,int *n,int getvec,int descending) 
/* Find eigen-values and vectors of n by n symmetric tridiagonal matrix 
   with leading diagonal d and sub/super diagonals g. 
   eigenvalues returned in d, and eigenvectors in columns of v, if
   getvec!=0. If *descending!=0 then eigenvalues returned in descending order,
   otherwise ascending. eigen-vector order corresponds.  

   Routine is divide and conquer followed by inverse iteration. 

   dstevd could be used instead, with just a name change.
   dstevx may be faster, but needs argument changes.
*/ 
		    
{ char compz;
  double *work,work1,x,*dum1,*dum2;
  int ldz=0,info,lwork=-1,liwork=-1,*iwork,iwork1,i,j;

  if (getvec) { compz='I';ldz = *n;} else { compz='N';ldz=0;}

  /* workspace query first .... */
  F77_NAME(dstedc)(&compz,n,
		   d, g, /* lead and su-diag */
		   v, /* eigenvectors on exit */  
                   &ldz, /* dimension of v */
		   &work1, &lwork,
		   &iwork1, &liwork, &info);

   lwork=(int)floor(work1);if (work1-lwork>0.5) lwork++;
   work=(double *)calloc((size_t)lwork,sizeof(double));
   liwork = iwork1;
   iwork= (int *)calloc((size_t)liwork,sizeof(int));

   /* and the actual call... */
   F77_NAME(dstedc)(&compz,n,
		   d, g, /* lead and su-diag */
		   v, /* eigenvectors on exit */  
                   &ldz, /* dimension of v */
		   work, &lwork,
		   iwork, &liwork, &info);

   if (descending) { /* need to reverse eigenvalues/vectors */
     for (i=0;i<*n/2;i++) { /* reverse the eigenvalues */
       x = d[i]; d[i] = d[*n-i-1];d[*n-i-1] = x;
       dum1 = v + *n * i;dum2 = v + *n * (*n-i-1); /* pointers to heads of cols to exchange */
       for (j=0;j<*n;j++,dum1++,dum2++) { /* work down columns */
         x = *dum1;*dum1 = *dum2;*dum2 = x;
       }
     }
   }

   free(work);free(iwork);
   *n=info; /* zero is success */
}
