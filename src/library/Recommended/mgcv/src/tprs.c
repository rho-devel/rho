/* Copyright (C) 2000-2012 Simon N. Wood  simon.wood@r-project.org

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License   
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
(www.gnu.org/copyleft/gpl.html)

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
USA.*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "mgcv.h"
#include "matrix.h"
#include "general.h"
#include <R_ext/BLAS.h>
/* Code for thin plate regression splines */

#define ROUND(a) ((a)-(int)floor(a)>0.5) ? ((int)floor(a)+1):((int)floor(a))

void ErrorMessage(char *msg, int fatal);

static inline double eta(int m,int d,double r)

/* The basis functions for a thin plate spline for d- dimensional data, with an mth order 
   wiggliness penalty. 
   Note that r is really r^2 (saves a sqrt when d even, and some flops )
*/

{ double pi=PI,Ghalf=1.772453850905515881919; /* Gamma function of 0.5 = sqrt(pi) */
  double f;
  int i,k,d2,m2;

  d2 = d/2;m2 = 2*m;
  if (m2 <= d) ErrorMessage(_("You must have 2m>d for a thin plate spline."),1);
  if (r<=0.0) return(0.0); /* this is safe: even if eta() gets inlined so that r comes in in an fp register! */
  if (d%2==0) /* then d even */
  { if ((m+1+d2)%2) f= -1.0; else f=1.0; /* finding (-1)^{m+1+d/2} */
    for (i=0;i<m2-1;i++) f/=2;  /* dividing by 2^{2m-1} */
    for (i=0;i<d2;i++) f/=pi;  /* dividing by pi^{d/2} */
    for (i=2;i<m;i++) f/=i; /* dividing by (m-1)! */
    for (i=2;i<=m-d2;i++) f/=i; /* dividing by (m-d/2)! */
    /*f*=log(r);*/
    f *= log(r) * .5; /* since r is really r^2 */
    /* for (i=0;i<2*m-d;i++) f*=r;*/
    for (i=0;i<m-d2;i++) f *= r; /* r^2m-d (noting r is really r^2) */
  } else /* d odd */
  { f=Ghalf;
    k=m-(d-1)/2; /* 1/2 - d = d/2 -m */
    for (i=0;i<k;i++) f/= -0.5-i; /* f = gamma function of d/2-m */
    for (i=0;i<m;i++) f/= 4; /* divide by 2^{2m} */
    for (i=0;i<d-1;i++) f/=pi;
    f /= Ghalf;                /* dividing by (pi^{d/2}) */
    for (i=2;i<m;i++) f/=i;  /* divide by (m-1)! */
    /* for (i=0;i<2*m-d;i++) f*=r;*/
    for (i=0;i<m-d2-1 ;i++) f *= r; /* note r really r^2 */
    f *= sqrt(r); 
  } 
  return(f);
}


double eta_const(int m,int d) {
  /* compute the irrelevant constant for TPS basis */
  double pi=PI,Ghalf; 
  double f;
  int i,k,d2,m2;
  Ghalf = sqrt(pi); /* Gamma function of 0.5 = sqrt(pi) */
  d2 = d/2;m2 = 2*m;
  if (m2 <= d) ErrorMessage(_("You must have 2m>d for a thin plate spline."),1);
  if (d%2==0) /* then d even */
  { if ((m+1+d2)%2) f= -1.0; else f=1.0; /* finding (-1)^{m+1+d/2} */
    for (i=0;i<m2-1;i++) f/=2;  /* dividing by 2^{2m-1} */
    for (i=0;i<d2;i++) f/=pi;  /* dividing by pi^{d/2} */
    for (i=2;i<m;i++) f/=i; /* dividing by (m-1)! */
    for (i=2;i<=m-d2;i++) f/=i; /* dividing by (m-d/2)! */
  } else /* d odd */
  { f=Ghalf;
    k=m-(d-1)/2; /* 1/2 - d = d/2 -m */
    for (i=0;i<k;i++) f/= -0.5-i; /* f = gamma function of d/2-m */
    for (i=0;i<m;i++) f/= 4; /* divide by 2^{2m} */
    for (i=0;i<d-1;i++) f/=pi;
    f /= Ghalf;                /* dividing by (pi^{d/2}) */
    for (i=2;i<m;i++) f/=i;  /* divide by (m-1)! */
  } 
  return(f);
}

static inline double fast_eta(int m,int d,double r,double f) {
  int d2,i;
  /* computation of eta given constant already in f and r^2 in r */
  d2 = d/2;
  if (r<=0.0) return(0.0); /* this is safe: even if eta() gets inlined so that r comes in in an fp register! */
  if (d%2==0) { /* then d even */
    f *= log(r) * .5; /* since r is really r^2 */
    for (i=0;i<m-d2;i++) f *= r; /* r^2m-d (noting r is really r^2) */
  } else { /* d odd */
    for (i=0;i<m-d2-1 ;i++) f *= r; /* note r really r^2 */
    f *= sqrt(r); 
  } 
  return(f);
}

void tpsE(matrix *E,matrix *X,int m,int d)

/* obtains E the tps penalty matrix (and all round weird object). It is assumed that the ith
   row of X contains the co-ordinates of the ith datum. */

{ int i,j,k,Xr,Xc;
  double r,x,eta0,**EM,**XMi,**XMj,*xi,*xj;
  (*E)=initmat(X->r,X->r);
  EM = E->M;
  eta0 = eta_const(m,d);
  XMi = X->M;Xr = X->r;Xc = X->c;
  for (i=0;i<Xr;i++,XMi++) for (XMj = X->M,j=0;j<i;j++,XMj++)
  { r=0.0;
    for (xi= *XMi,xj= *XMj,k=0;k<Xc;k++,xi++,xj++) { 
      /*x=X->M[i][k]-X->M[j][k];*/
      x = *xi - *xj;
      r+=x*x; 
    } 
    /*r=sqrt(r);*/                       /* r= ||x_j-x_i||^2 where x_k is kth location vector */
    EM[i][j]=EM[j][i]=fast_eta(m,d,r,eta0);
  } 
}


void gen_tps_poly_powers(int *pi /* **pi */,int *M,int *m, int *d)

/* generates the sequence of powers required to specify the M polynomials spanning the 
   null space of the penalty of a d-dimensional tps with wiggliness penalty order m 
   So, if x_i are the co-ordinates the kth polynomial is x_1^pi[k][1]*x_2^pi[k][2] ....
 
   pi[k][j] actually stored as pi[k + M * j] 
*/

{ int *index,i,j,sum;
  /*  if (2*m<=d) ErrorMessage(_("You must have 2m > d"),1); caught in R */
  index=(int *)R_chk_calloc((size_t) *d,sizeof(int));
  for (i=0;i < *M;i++)
  { /* copy index to pi */
    /* for (j=0;j<d;j++) pi[i][j]=index[j];*/
    for (j=0;j< *d;j++) pi[i + *M * j]=index[j];
    /* update index.... */
    sum=0;for (j=0;j< *d;j++) sum += index[j];
    if (sum< *m-1) /* then increase lowest index */
    index[0]++;
    else         /* pass the problem up */
    { sum -= index[0];
      index[0]=0;
      for (j=1;j< *d;j++)
      { index[j]++;sum++;
        if (sum== *m) { sum-=index[j];index[j]=0;}
        else break; /* problem resolved! */
      } 
    }
  }
  R_chk_free(index); 
}


void tpsT(matrix *T,matrix *X,int m,int d)

/* obtains tps constraint matrix T_{ij}=\phi_j(x_i) where x_i is location (vector) of ith obs. 
   m is the order of the wiggliness penalty, d the dimension of the covariate vectors.
   ith row of X is location of ith datum. 
*/

{ int M,i,j,k,*pin,z;
  double x;
  M=1;
  for (i=0;i<d;i++) M*=d+m-1-i;
  for (i=2;i<=d;i++) M/=i;     /* M = (m+d+1)!/(d!(m-d!) */

  /* pin=(int **)R_chk_calloc((size_t)M,sizeof(int *)); 
     for (i=0;i<M;i++) pin[i]=(int *)R_chk_calloc((size_t)d,sizeof(int));*/

  pin = (int *)R_chk_calloc((size_t) M * d,sizeof(int));
  gen_tps_poly_powers(pin, &M, &m, &d); /* pin[][] contains powers of polynomials in unpenalized basis */
  
  (*T)=initmat(X->r,(long)M);
  for (i=0;i<T->r;i++)
  for (j=0;j<M;j++)
  { x=1.0;/* for (k=0;k<d;k++) for (z=0;z<pin[j][k];z++) x *= X->M[i][k]; */
    for (k=0;k<d;k++) for (z=0;z<pin[j + M * k];z++) x *= X->M[i][k];
    T->M[i][j]=x;
  }
  
  /*for (i=0;i<M;i++) R_chk_free(pin[i]);*/
  R_chk_free(pin);
}


int null_space_dimension(int d, int m)

/* finds the dimension of the penalty null space for a d-dimensional smoother, with mth order 
   penalties: if 2m<=d then default setting is used, which uses smallest m such that 2*m>d+1
*/  

{ int M,i;
  if (2*m<=d) {m=1;while (2*m<d+2) m++;} 
  M=1;     /* dimension of penalty null space */
  for (i=0;i<d;i++) M*=d+m-1-i;/* get (m+d-1)!/(m-1)! = (m+d-1)*(m+d-2) ... *(m) -- d terms */ 
  for (i=2;i<=d;i++) M/=i;     /* M = (m+d-1)!/(d!(m-1)!) */
  return(M);
}


double tps_g(matrix *X,matrix *p,double *x,int d,int m,double *b,int constant)

/* Evaluates the thin plate spline of dimension d with wiggliness penalty of 
   order m, at location x, g(x), say. Also returns vector b such that g(x)=b'p. 
   In the interests of efficiency the index array coding for the polynomials 
   spanning the null space of the penalty is stored statically and changed only 
   when d or m change. Set d to zero to clear this memory. 
   Calling with this d=0 when the memory is empty is now safe - thanks to
   Luke Tierney for spotting that it was not always so!   
   
   It is assumed that coefficients of the null space of the penalty are at the end of p.

   If constant == 0 then the model has no intercept term, while if constant ==1 it does.

   - the intercept parameter is at p[p.r-M], if it is present.
   
   The rows of X contain the covariates from the original data-points, reduced to 
   uniqueness (preferably in tprs_setup())

  - if p.r==0 then the value of the spline is not returned - only b
*/

{ static int sd=0,sm=0,*pin,M;
  static double eta0=1.0;
  double r,g,z,**XM,*dum,*XMi,*pb;
  int i,j,k,off,n;
  if (sd==0&&d==0) return(0.0); /* There is nothing to clear up and nothing to calculate */
  if (2*m<=d&&d>0) { m=0;while (2*m<d+2) m++;} 
  if (sd!=d||sm!=m) /* then re-calculate the penalty null space basis and eta constant */
  { if (sd>0&&sm>0) 
    { /*for (i=0;i<M;i++) R_chk_free(pin[i]);*/ R_chk_free(pin);}
    sd=d;sm=m;
    if (d>0) /* get a new basis for the null space of the penalty */
    { M=1;     /* dimension of penalty null space */
      for (i=0;i<d;i++) M*=d+m-1-i;
      for (i=2;i<=d;i++) M/=i;     /* M = (m+d+1)!/(d!(m-d!) */
      /* pin=(int **)R_chk_calloc((size_t)M,sizeof(int *)); 
         for (i=0;i<M;i++) pin[i]=(int *)R_chk_calloc((size_t)d,sizeof(int));*/
      pin=(int *)R_chk_calloc((size_t)M*d,sizeof(int)); 
      gen_tps_poly_powers(pin, &M, &m, &d);
      eta0 = eta_const(m,d); /* constant multiplying eta */
    } else return(0.0);
  } /* end of change specific setup */
  g=0.0;XM=X->M;n = X->r;
  for (pb=b,i=0;i<n;i++,pb++)
  { r=0.0;XMi=XM[i];
    for (dum=x;dum<x+d;dum++) { z= *XMi - *dum;XMi++;r+=z*z;}
    /* r = sqrt(r); */ /* eta set up to expect squared dist */ 
    *pb = fast_eta(m,d,r,eta0);
    if (p->r) g += *pb *p->V[i];
  } 
  off=1-constant;
  for (i=off;i<M;i++,pb++) /* work through null space */
  { r=1.0;
    /* for (j=0;j<d;j++) for (k=0;k<pin[i][j];k++) r*=x[j];*/
    
    for (j=0;j<d;j++) for (k=0;k<pin[i+M*j];k++)  r*=x[j];
    *pb = r; /* b->V[i+X->r-off]=r;*/
    if (p->r) g+=p->V[i+n-off]*r;
  } 
  return(g);
}

int Xd_row_comp(double *a,double *b,int k)

/* service routine for Xd_strip(), compares k elements of two rows for equality */

{ int i;
  for (i=0;i<k;i++) if (a[i]!=b[i]) return(0);
  return(1);
}

int *Xd_strip(matrix *Xd)

/* The rows of Xd (excluding last col) contain the covariate values for
   a set of data to which a thin plate spline is to be fitted. The purpose
   of this routine is to locate co-incident points, and strip out redundant
   copies of these points. At the same time a record is kept of what has 
   been done, so that the function returns an array yxindex, such that 
   yxindex[i] contains the row of the stripped down Xd that corresponds to 
   the ith response datum. Note that the identification of ties involves 
   sorting Xd - even if there are no ties.
    
   Note that this routine assumes that the final column of Xd consists of the 
   integers 0 to Xd->r-1. These are vital for constructing the index.

   On exit Xd->r will contain the number of unique covariate points.
*/

{ int *yxindex,start,stop,ok,i;
  double xi,**dum;
  yxindex = (int *)R_chk_calloc((size_t)Xd->r,sizeof(int));
  dum = (double **)R_chk_calloc((size_t)Xd->r,sizeof(double *));
  msort(*Xd);
  start=stop=0;ok=1;
  while(ok)
  { /* look for start of run of equal rows ..... */
    while(start<Xd->r-1&&!Xd_row_comp(Xd->M[start],Xd->M[start+1],Xd->c-1)) 
    { /* Xd->M[start] not tied with anything, nothing to erase.... */
      xi=Xd->M[start][Xd->c-1];
      yxindex[ROUND(xi)]=start;
      start++;
    }
    if (start==Xd->r-1) 
    { ok=0; /* reached end with no more ties */
      xi=Xd->M[start][Xd->c-1];
      yxindex[ROUND(xi)]=start; /* final index entry needed */
    }
    if (ok) /* search for end of run */
    { stop=start+1;
      while(stop<Xd->r-1&&Xd_row_comp(Xd->M[stop],Xd->M[stop+1],Xd->c-1)) stop++;
      for (i=start;i<=stop;i++) /* fill out the index array */
      { xi=Xd->M[i][Xd->c-1];
        yxindex[ROUND(xi)]=start;
        dum[i-start]=Xd->M[i]; /* Rows stored to copy back onto end, so matrix can be freed properly */
      }
      for (i=stop+1;i<Xd->r;i++)
      { Xd->M[i-stop+start]=Xd->M[i];}
      Xd->r -= stop-start;
      for (i=1;i<=stop-start;i++)
      { Xd->M[Xd->r-1+i]=dum[i];}
    } 
  }
  R_chk_free(dum); 
  return(yxindex);
}

void tprs_setup(double **x,double **knt,int m,int d,int n,int k,int constant,matrix *X,matrix *S,
                matrix *UZ,matrix *Xu,int n_knots)

/* Takes d covariates x_1,..,x_d and creates the truncated basis for an order m 
   smoothing spline, returning the design matrix and wiggliness penalty matrix 
   for this spline, along with the matrix transforming back to the regular basis.

   The dimension of the truncated basis must be greater than the dimension of 
   the null space of the penalty. 
   
   The inputs are:

   x[i] = array of n values for covariate i (i=0..d-1)
   m    = the order of the penalty (order of derivatives in penalty)
          if 2m>d is not satisfied (e.g. if m==0) then m is set
          to smallest value such that 2m>d+1 (ensures visual smoothness)
   d    = the dimension of the spline = number of covariates.
   n    = number of data.
   k    = dimension of truncated basis. This must be greater than the
          dimension of the null space of the penalty, which is 
          M=(m+d-1)!/[d!(m-1)!]
   constant = 0 if there is to be no intercept term in the model, 1 otherwise
   knt[i] array of n_knot knot location values for covariate i
   n_knot number of knots supplied - 0 for none meaning that the values in x 
          are the knots. n_knots<k equivalent to 0. If n_knot=k then eigen
          decomposition is redundant and is not performed.

   The outputs are X, S and UZ such that the spline is fitted by minimising:

                ||Xp_k - y||^2 + \lambda p_k'Sp_k

   and p = UZ p_k, gives the parameters required to evaluate the spline using
   the regular t.p.s. basis (with the parameters for the null space of the 
   penalty being the last M elements of p). 

   Also output is Xu, the matrix containing all the unique combinations of 
   covariates (this is suitable for use by tps_g() when evaluating the spline)

   This routine applies a linear transformation to the problem, intended to 
   improve the numerical stability of the algorithm. The idea is that p_k is
   replaced by W^{-1} p_k where W is diagonal and constructed so that the mean 
   square size of each column of X is 1. Then the following substitutions take 
   place:
          X -> XW
          UZ -> UZW
          S  -> WSW  
   Provided the user uses UZ to transform back to the t.p.s parameters the 
   rescaling is transparent.
*/

{ matrix X1,E,U,v,TU,T,Z,p;
  const char trans='T'; 
  int l,i,j,M,*yxindex,pure_knot=0,nk,minus=-1,kk,one=1;
  double w,*xc,*XMi,*Ea,*Ua,tol=DOUBLE_EPS,*b,*a,*uz,alpha=1.0,beta=0.0,*p0,*p1;
  tol = pow(tol,.7);

  if (n_knots<k) /* then use the covariate points as knots */
  { *Xu=initmat((long)n,(long)d+1);
    for (i=0;i<n;i++) { for (j=0;j<d;j++) Xu->M[i][j]=x[j][i];Xu->M[i][d]=(double)i;}
  } else /* knot locations supplied */
  { *Xu=initmat((long)n_knots,(long)d+1);
    for (i=0;i<n_knots;i++) { for (j=0;j<d;j++) Xu->M[i][j]=knt[j][i];Xu->M[i][d]=(double)i;}
  }
  /* Now the number of unique covariate "points" must be obtained */
  /* and these points stored in Xu, to avoid problems with E */
  yxindex=Xd_strip(Xu); /*yxindex[i] is the row of Xu corresponding to y[i] */
 
  
  Xu->c--; /* hide indexing column */
  if (Xu->r<k) 
  ErrorMessage(_("A term has fewer unique covariate combinations than specified maximum degrees of freedom"),1);
  if (2*m<=d) { m=0;while (2*m<d+2) m++;} 
  tpsE(&E,Xu,m,d); /* The important matrix in the full t.p.s. problem */
  tpsT(&T,Xu,m,d); /* The tps constraint matrix */
  M=(int)T.c;       /* dimension of penalty null space */
  /*ek=k-(d+1);*/  /* erroneous code - when I thought that -ve's must not be deleted */
  if (k<M+1)  /* re-set basis dimension if it is impossibly small */
  {  k=M+1;
     if (Xu->r<k) ErrorMessage(_("A term has fewer unique covariate combinations than specified maximum degrees of freedom"),1);
  }
  if (Xu->r==k) pure_knot=1; /* basis dimension is number of knots - don't need eigen step */

  if (pure_knot) /* don't need the lanczos step, but need to "fake" various matrices to make up for it! */
  { *UZ=initmat(T.r+M-1+constant,T.r);
    UZ->r=T.r;
    TU=initmat(T.c,T.r);
    for (i=0;i<T.r;i++) for (j=0;j<T.c;j++) TU.M[j][i]=T.M[i][j];
    QT(*UZ,TU,1); /* UZ is now simply Z - but needs to be full, not just HH's */
    for (i=0;i<T.r;i++) for (j=0;j<T.c;j++) TU.M[j][i]=T.M[i][j];
    Z=initmat((long)M,T.r);
    QT(Z,TU,0);  /* Still need Z as HH's for later */
  } else
  { v=initmat((long)k,1L);    /* eigen-value matrix for E */

  
      nk = E.r;
      Ea = (double *) R_chk_calloc((size_t) nk*nk,sizeof(double));
      Ua = (double *) R_chk_calloc((size_t) nk*k,sizeof(double));
      RArrayFromMatrix(Ea,nk,&E);
      minus = -1;kk=k; 
  
      Rlanczos(Ea,Ua,v.M[0],&nk, &kk, &minus,&tol);

      U = Rmatrix(Ua,E.r,k);R_chk_free(Ea);R_chk_free(Ua);
    
  
    /* Now form the constraint matrix for the truncated problem T'U */
    TU=initmat((long)M,k);
    matmult(TU,T,U,1,0);
    /* Now TU \delta_k =0 is the constraint for this problem. To impose it use  */
    /* a QT factorization on TU. i.e. TU Q = [0,B] where B is M by M and Q */
    /* can be written Q=[Z,Y], where Z is the null space of the constraints. */
    Z=initmat((long)M,TU.c);
    QT(Z,TU,0);  /* Z now contains null space as series of householder rotations */
    *UZ=initmat(U.r+M-1+constant,U.c);UZ->r=U.r;
    mcopy(&U,UZ);
    HQmult(*UZ,Z,0,0);UZ->c -= M;      /* Now UZ multiplied by truncated delta gives full delta */
    UZ->c += M-1+constant;  /* adding cols for un-constrained terms to UZ */
  }
  UZ->r +=M-1+constant;
  /* Now add the elements required to get UZ to map from whole real parameter vector to whole t.p.s. vector */
  for (i=0;i<E.r;i++) for (j=k-M;j<UZ->c;j++) UZ->M[i][j]=0.0;
  for (i=0;i<M-1+constant;i++) UZ->M[UZ->r-i-1][UZ->c-i-1]=1.0;
  
  /* Now construct the design matrix X = [Udiag(v)Z,T] .... */
  if (n_knots<k&&!pure_knot) /* then the basis prior to truncation is pure spline basis: 6/5/2002 - !pure_knots added as bug fix*/
  { X1=initmat(U.r,(long)k);
    mcopy(&U,&X1); /* now form Udiag(v) */
    for (i=0;i<X1.r;i++) for (j=0;j<X1.c;j++) X1.M[i][j]*=v.V[j];
    HQmult(X1,Z,0,0);  /* form Udiag(v)Z */
    for (i=0;i<X1.r;i++) for (j=X1.c-M;j<X1.c;j++) X1.M[i][j]=0.0;
    /*now add in T (minus first column if constant=0) */
    if (constant)
    for (i=0;i<X1.r;i++) for (j=0;j<T.c;j++) X1.M[i][X1.c-M+j]=T.M[i][j];
    else 
    { for (i=0;i<X1.r;i++) for (j=1;j<T.c;j++) X1.M[i][X1.c-M+j-1]=T.M[i][j];X1.c--;}
    /* now map the design matrix back onto the design matrix for the original data */
    /* undoing what had to be done to deal with tied covariates ...... */
    *X=initmat((long)n,X1.c);
    for (i=0;i<n;i++)
    { l=yxindex[i];
      for (j=0;j<X1.c;j++) X->M[i][j]=X1.M[l][j];
    }
    freemat(X1);
  } else /* the user supplied a set of knots to generate the original un-truncated basis */
  { p.r=0L; /* don't want a value from tps_g() */
    xc=(double *)R_chk_calloc((size_t)d,sizeof(double));
    kk = (int) UZ->r;
    b=(double *)R_chk_calloc((size_t)kk,sizeof(double));  /* initmat((long)UZ->r,1L);*/
    *X=initmat((long)n,(long)k);
    a = (double *)R_chk_calloc((size_t)k,sizeof(double));
    /* following loop can dominate computational cost, so it is worth 
       using BLAS routines and paying some attention to efficiency */
    uz = (double *) R_chk_calloc((size_t)kk*k,sizeof(double));
    RArrayFromMatrix(uz,kk,UZ);
    for (i=0;i<n;i++) { 
      for (j=0;j<d;j++) xc[j]=x[j][i];
      tps_g(Xu,&p,xc,d,m,b,constant);
      /* now X1'[UZ] p_k evaluates to the correct thing */
      
      /* UZ is kk by k */
      F77_CALL(dgemv)(&trans,&kk,&k,&alpha,uz,&kk, b, &one,&beta, a, &one); /* BLAS call for (UZ)'b */
      XMi = X->M[i];
      for (p0=a,p1=a+k;p0<p1;p0++,XMi++) *XMi = *p0;

      /* XMi=X->M[i]; UZM=UZ->M;
      for (j=0;j<k;j++) // form [UZ]'X1 
      { for (l=0;l<kk;l++) *XMi += UZM[l][j]*b[l];
        XMi++;      
      } */
    }
    tps_g(Xu,&p,xc,0,0,b,constant); /* tell tps_g to clear up its internally allocated memory - only d=0 matters here*/
    R_chk_free(xc);R_chk_free(b);R_chk_free(a);R_chk_free(uz);
  }
  /* Next, create the penalty matrix...... */
  *S=initmat((long)k,(long)k); /* form Z'SZ */
  if (pure_knot) mcopy(&E,S);
  else for (i=0;i<v.r;i++) S->M[i][i]=v.V[i];
  HQmult(*S,Z,0,0);HQmult(*S,Z,1,1);
  for (i=0;i<S->r;i++) for (j=S->r-M;j<S->r;j++) S->M[i][j]=S->M[j][i]=0.0;
  if (!constant) {S->r--;S->c--;}
  /* Now linearly transform everything so that numerical properties of X are as nice as possible.
     Specifically, rescale each column of X so that it has rms value 1. X -> XW.
     This means that S -> WSW and UZ -> UZW.  
  */
 
  for (i=0;i<X->c;i++)
  { w=0; for (j=0;j<X->r;j++) w+=X->M[j][i]*X->M[j][i]; w=sqrt(w/X->r);
    for (j=0;j<X->r;j++) X->M[j][i]/=w;
    for (j=0;j<UZ->r;j++) UZ->M[j][i]/=w;
    for (j=0;j<S->r;j++) S->M[i][j]/=w;
    for (j=0;j<S->r;j++) S->M[j][i]/=w;
  }  
  R_chk_free(yxindex);freemat(Z);freemat(TU);freemat(E);freemat(T);
  if (!pure_knot) {freemat(U);freemat(v);}
}


void construct_tprs(double *x,int *d,int *n,double *knt,int *nk,int *m,int *k,double *X,double *S,
                    double *UZ,double *Xu,int *nXu,double *C)
/* inputs: 
   x contains the n values of each of the d covariates, stored end to end
   knt contains the nk knot locations packed as x
   m is the order of the penalty 
   k is the basis dimension
   max_knots is the maximum number of knots to allow in t.p.r.s. setup.   

   outputs:
   X is the n by k model matrix
   S is the K by K penalty matrix
   UZ is the (nXu+M) by k matrix transforming from the truncated to full bases
   Xu is the nXu by d matrix of unique covariate combinations
   C is the 1 by k sum to zero constraint matrix 
*/ 

{ double **xx,**kk=NULL,*dum,**XM;
  matrix Xm,Sm,UZm,Xum;
  int i,j,Xr;
  xx=(double **)R_chk_calloc((size_t)(*d),sizeof(double*));
  for (i=0;i<*d;i++) xx[i]=x + i * *n;
  if (*nk)
  { kk=(double **)R_chk_calloc((size_t)(*d),sizeof(double*));
    for (i=0;i<*d;i++) kk[i]=knt + i * *nk;
  }
  tprs_setup(xx,kk,*m,*d,*n,*k,1,&Xm,&Sm,&UZm,&Xum,*nk); /* Do actual setup */
  RArrayFromMatrix(X,Xm.r,&Xm);
  RArrayFromMatrix(S,Sm.r,&Sm);
  RArrayFromMatrix(UZ,UZm.r,&UZm);  
  RArrayFromMatrix(Xu,Xum.r,&Xum);
  *nXu=Xum.r;  
  /* construct the sum to zero constraint */
  dum=C;XM=Xm.M;Xr=Xm.r;
  for (i=0;i< *k;i++)
  { *dum = 0.0;
    for (j=0;j<Xr;j++) *dum += XM[j][i];
    dum++;
  }
  freemat(Xm);freemat(Sm);freemat(UZm);freemat(Xum);
  R_chk_free(xx);if(*nk) R_chk_free(kk);
}

void predict_tprs(double *x, int *d,int *n,int *m,int *k,int *M,double *Xu,int *nXu,
                  double *UZ,double *by,int *by_exists,double *X)
/* inputs are:
   * The n values of the d covariates at which to predict - covariates packed end to end in x
     - any required centering to be done before this call.
   * m is the penalty order and M the null space dimension
   * k is the rank of the basis
   * Xu is the nXu by d matrix of unique covariate values
   * UZ is the basis of the reduced space 

   returns the n by k matrix X mapping the parameters to the predicted values.
*/
{ double *b,by_mult,*xx,*a,*xp,*xxp,*xxp1,*xp1,*Xp,alpha=1.0,beta=0.0,*Xup,*Xup1,r,z,*pb,
         eta0;
  int i,j,l,kk,one=1,nobsM,*pin; 
  const char trans='T'; 

  if (2 * *m <= *d && *d > 0) { *m = 0;while ( 2 * *m < *d+2) (*m)++;} 
  /* get null space polynomial powers */
  pin=(int *)R_chk_calloc((size_t) *M * *d,sizeof(int)); 
  gen_tps_poly_powers(pin, M, m, d);
  eta0 = eta_const(*m,*d);

  /*Xum=Rmatrix(Xu,*nXu,*d);*/
  nobsM = *nXu + *M;
  /* UZm=Rmatrix(UZ,nobsM,*k);*/
  b=(double *)R_chk_calloc((size_t)nobsM,sizeof(double)); /* initmat(UZm.r,1L);*/
  a=(double *)R_chk_calloc((size_t)*k,sizeof(double));
  /* Xm=initmat((long)*n,(long)*k);*/
  xx=(double*)R_chk_calloc((size_t) *d,sizeof(double));
  for (Xp=X,xp=x,i=0;i< *n;i++,xp++,Xp++) 
  { if (*by_exists) by_mult=by[i]; else by_mult=1.0;
    if (by_mult==0.0) {         /* then don't waste flops on calculating stuff that will only be zeroed */
      /*for (j=0;j< *k ;j++) Xm.M[i][j]=0.0;*/
      for (xxp=Xp,j=0;j < *k;j++,xxp+= *n) *xxp = 0.0; 
    } else {                   /* proceed as normal */
      for (xxp=xx,xxp1=xx + *d,xp1=xp;xxp < xxp1;xxp++,xp1 += *n) *xxp = *xp1; /*xx[j]=x[j * *n + i];*/
      /* evaluate radial basis */    
      for (Xup=Xu,Xup1=Xu+*nXu,pb=b;Xup<Xup1;Xup++,pb++) { /* work through unique original locations */
        r=0.0;
        for (xxp=xx,xxp1=xx + *d,xp1=Xup;xxp<xxp1;xxp++,xp1+= *nXu) { z = *xp1 - *xxp;r += z*z;}
        /* r = sqrt(r); */ /* eta set up to expect squared dist */ 
        *pb = fast_eta(*m,*d,r,eta0);
      } 
      /* now deal with null space */
      for (l=0;l< *M;l++,pb++) { /* work through null space */
        r=1.0;
        for (j=0;j<*d;j++) for (kk=0;kk<pin[l + *M * j];kk++)  r *= xx[j];
        *pb = r; 
      } 
      /*tps_g(&Xum,&p,xx,*d,*m,b,1);*/             
      /*j=0;
        mgcv_mmult(a,UZ,b,&one,&j,k,&one,&nobsM);*/ /* get a=(UZ)'b */
      F77_CALL(dgemv)(&trans,&nobsM,k,&alpha,UZ,&nobsM, b, &one,&beta, a, &one); /* BLAS call for (UZ)'b */
      if (*by_exists)
      for (xp1=Xp,xxp=a,xxp1=a + *k;xxp<xxp1;xxp++,xp1+= *n) *xp1 = *xxp * by_mult; 
      else 
      for (xp1=Xp,xxp=a,xxp1=a + *k;xxp<xxp1;xxp++,xp1+= *n) *xp1 = *xxp;
      
      /* for (j=0;j< *k;j++) 
      { Xm.M[i][j]=0.0;
        for (l=0;l<UZm.r;l++) Xm.M[i][j] += b[l]*UZm.M[l][j];  // forming b'UZ 
        Xm.M[i][j] *= by_mult;
      }*/
    }
  }
  /* Now clean up and copy X back.*/
  /* RArrayFromMatrix(X,Xm.r,&Xm); freemat(Xm);*/
  /*tps_g(&Xum,&p,x,0,0,b,1);*/ /* have tps_g clear up */ 
  /*freemat(Xum);*/
  /*freemat(UZm);*/
  R_chk_free(b);R_chk_free(a);
  R_chk_free(xx);R_chk_free(pin);
}





/******************************************************************************************/
/* Update log                                                                             

25/8/2001 - tprs_setup modified for enhanced numerical stability.

31/10/2001 - tprs_setup modified - there are d+1 -ve eigenvalues of E not M!! 

2/11/2001 - default_null_space_dimension replaced with null_space_dimension, which allows user 
            to select m, but uses default dimension if 2m>d not satisfied.

11/2/2002 - tprs_setup now retains the largest magnitude eigen-vectors irrespective of sign
            this was not correctly handled previously: -ve's were always kept, due to an
            error in the original tprs optimality derivation.
2-3/2002  - tprs_setup modified to allow knot based tprs bases - pure knot based or knot
            and then eigen are both allowed.
6/5/2002  - bug fix: full spline bases failed - part of tprs_setup treated them as knot based
            and part as eigen-based - resulted in seg fault.
3/10/2002 - tps_g() has a fix so that if told to clear up before having anything to clear up,
            it doesn't write all sorts of things to un-allocated memory. Many thanks to Luke 
            Tierney for finding this. 
3/10/2002 - tprs_setup now tells tps_g() to clear up before returning
1/11/2005 - eta() constants `wrong' for odd d: fixed.
*/






