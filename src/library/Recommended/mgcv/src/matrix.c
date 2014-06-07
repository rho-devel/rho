/* Copyright (C) 1991-2005 Simon N. Wood  simon.wood@r-project.org

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

/* Routines for basic matrix manipulation creation, destruction and file i/o
   for matrices. See end of file for update log */

#include <R.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "mgcv.h"
#include "matrix.h"
#include "general.h"
#define RANGECHECK
#define PAD 1L

#define ROUND(a) ((a)-(int)floor(a)>0.5) ? ((int)floor(a)+1):((int)floor(a))


matrix null_mat; /* matrix for passing when you don't actually need to */
#define PADCON (-1.234565433647588392902028934e270)

/* counter for memory used */


long memused=0L,matrallocd=0L;

/* the routines */

struct mrec
{ matrix mat;
  struct mrec *fp,*bp;
};
typedef struct mrec MREC;

void ErrorMessage(char *msg,int fatal);

matrix null_mat;
MREC *top,*bottom;

matrix initmat(rows,cols) long rows,cols;

/* Don't alter this without altering freemat() as well !! */

{ matrix A;long i,j,pad;
#ifdef RANGECHECK
  pad=PAD;
#else
  pad=0L;
#endif
  A.vec=0;
  A.M=(double **)R_chk_calloc((size_t)(rows+2*pad),sizeof(double *));
  if ((cols==1L)||(rows==1L))
  { if (A.M)
    A.M[0]=(double *)R_chk_calloc((size_t)(cols*rows+2*pad),sizeof(double));
    for (i=1L;i<rows+2*pad;i++)
    A.M[i]=A.M[0]+i*cols;A.vec=1;
  } else
  { if (A.M)
    for (i=0L;i<rows+2*pad;i++)
    A.M[i]=(double *)R_chk_calloc((size_t)(cols+2*pad),sizeof(double));
  }
  A.mem=rows*cols*sizeof(double);
  memused+=A.mem;matrallocd++;
  A.original_r=A.r=rows;A.original_c=A.c=cols;
  if (((!A.M)||(!A.M[rows-1+2*pad]))&&(rows*cols>0L))
  { ErrorMessage(_("Failed to initialize memory for matrix."),1);}
  if (pad)  /* This lot is debugging code that checks out matrix errors
		       on allocation and release */
  { if (A.vec)
    { A.V=A.M[0];for (i=0;i<pad;i++) { A.V[i]=PADCON;A.V[i+pad+A.r*A.c]=PADCON;}
    } else
    { for (i=0;i<A.r+2*pad;i++)
      { for (j=0;j<pad;j++) A.M[i][j]=PADCON;
	     for (j=A.c+pad;j<A.c+2*pad;j++) A.M[i][j]=PADCON;
      }
      for (i=0;i<A.c+2*pad;i++)
      { for (j=0;j<pad;j++) A.M[j][i]=PADCON;
	     for (j=A.r+pad;j<A.r+2*pad;j++) A.M[j][i]=PADCON;
      }
    }
    for (i=0;i<A.r+2*pad;i++)
    for (j=0;j<pad;j++) A.M[i]++;  /* shifting pointers forward past padding */
    if (!A.vec) for (j=0;j<pad;j++) A.M++;
    A.V=A.M[0];
  /* putting a record of the matrix on the linked list of all extant matrices */
    if (matrallocd==1) /*new list*/
    { top=bottom=(MREC *)R_chk_calloc(1,sizeof(MREC));
      bottom->mat=top->mat=A;top->bp=bottom;bottom->fp=top;
    } else  /* expanding the linked list by one */
    { top->fp=(MREC *)R_chk_calloc(1,sizeof(MREC));
      top->fp->mat=A;top->fp->bp=top;top=top->fp; /* crystal clear, no? */
    }
  }
  A.V=A.M[0];/* This allows vectors to be accessed using A.V[i] */
  return(A);
}

matrix initvec(rows) long rows;

{ return(initmat(1L,rows));}

void freemat(A) matrix A;

{ long i,j,pad;int ok=1;
  MREC *delet;
#ifdef RANGECHECK
  pad=PAD;
#else
  pad=0L;
#endif
/*  if (A.original_r*A.original_c!=0L) */
  { if (pad)
    { if (A.vec)
      { for (i=-pad;i<0;i++)
	     if ((A.V[i]!=PADCON)||(A.V[i+A.original_r*A.original_c+pad]!=PADCON))
	     ok=0;
      } else
      { for (i=-pad;i<A.original_r+pad;i++)
	     { for (j=A.original_c;j<A.original_c+pad;j++) if (A.M[i][j]!=PADCON) ok=0;
	       for (j=-pad;j<0;j++) if (A.M[i][j]!=PADCON) ok=0;
	     }
	     for (i=-pad;i<A.original_c+pad;i++)
	     { for (j=A.original_r;j<A.original_r+pad;j++) if (A.M[j][i]!=PADCON) ok=0;
	       for (j=-pad;j<0;j++) if (A.M[j][i]!=PADCON) ok=0;
	     }
      }
      if (!ok)
      { ErrorMessage(_("An out of bound write to matrix has occurred!"),1);
      }
      /* find the matrix being deleted in the linked list of extant matrices */
      i=0L;delet=bottom;
      while ((i<matrallocd)&&(delet->mat.M!=A.M)) { i++;delet=delet->fp;}
      if (i==matrallocd)
      { ErrorMessage(_("INTEGRITY PROBLEM in the extant matrix list."),1);
      } else
      { if (i)
	     delet->bp->fp=delet->fp;
	     else bottom=delet->fp;
	     if (i!=matrallocd-1)
	     delet->fp->bp=delet->bp;
	     else top=delet->bp;
	     R_chk_free(delet);
      }
      /* repositioning pointers so that what was allocated gets freed */
      if (!A.vec) for (i=0;i<pad;i++) A.M--;
      for (i=0;i<A.original_r+2*pad;i++)
      for (j=0;j<pad;j++) A.M[i]--;
    }
    if (A.vec) R_chk_free(A.M[0]); else
    for (i=0;i<A.original_r+2*pad;i++) if (A.M[i]) R_chk_free(A.M[i]);
    if (A.M) R_chk_free(A.M);
    memused -= A.mem;matrallocd--;
  }
}

void matrixintegritycheck()

/* iff RANGECHECK is defined above then you can call this routine to check
   on the integrity of the matrix system. The routine looks for writing out
   of bounds from the matrix */

{ MREC *B;
  int ok=1;
  long pad=PAD,i,j,k=0L;
  matrix A;
#ifndef RANGECHECK
  ErrorMessage(_("You are trying to check matrix integrity without defining RANGECHECK."));
#endif
  B=bottom;
  while (k<matrallocd)
  { A=B->mat;
    if (A.vec)
    { for (i=-pad;i<0;i++)
      if ((A.V[i]!=PADCON)||(A.V[i+A.original_r*A.original_c+pad]!=PADCON))
      ok=0;
    } else
    { for (i=-pad;i<A.original_r+pad;i++)
      { for (j=A.original_c;j<A.original_c+pad;j++) if (A.M[i][j]!=PADCON)
	     ok=0;
	     for (j=-pad;j<0;j++) if (A.M[i][j]!=PADCON)
        ok=0;
      }
      for (i=-pad;i<A.original_c+pad;i++)
      { for (j=A.original_r;j<A.original_r+pad;j++) if (A.M[j][i]!=PADCON)
	     ok=0;
	     for (j=-pad;j<0;j++) if (A.M[j][i]!=PADCON)
	     ok=0;
      }
    }
    if (!ok)
    { ErrorMessage(_("An out of bound write to matrix has occurred!"),1);
    }
    k++;B=B->fp;
  }
}




void vmult(matrix *A,matrix *b,matrix *c,int t)

/* fast multiplication of vector by matrix c=Ab if t==0 c=A'b otherwise*/

{ double **AM,*bV,*cV,*p;
  long i,j,cr,br;
  cr=c->r;br=b->r;
  AM=A->M;bV=b->V;cV=c->V;
  if (t) /* then A transposed */
  for (i=0;i<cr;i++)
  { *cV=0.0;
    for (j=0;j<br;j++) *cV += AM[j][i]*bV[j];
    cV++;
  } else
  for (i=0;i<cr;i++)
  { *cV=0.0;
    p=AM[i];
    for (j=0;j<br;j++)
    *cV += p[j]*bV[j];
    cV++;
  }
}

void mcopy(matrix *A,matrix *B)

/* copies A into B */

{ long Ac;
  double *pA,*pB,**AM,**BM;
  if (A->r>B->r||A->c>B->c) ErrorMessage(_("Target matrix too small in mcopy"),1);
  BM=B->M;Ac=A->c;
  for (AM=A->M;AM<A->M+A->r;AM++)
  { pB= *BM;
    for (pA= *AM;pA< *AM+Ac; pA++) *(pB++) = *pA;
    BM++;
  }
}

void matmult(C,A,B,tA,tB) matrix C,A,B;int tA,tB;

/* Puts A*B in C. A will be transposed in this calculation if tA is not zero.
   B will be transposed if tB is not zero */

{ long i,j,k;
  double temp,*p,*p1,*p2,**CM,**AM,**BM;
  AM=A.M;BM=B.M;CM=C.M; /* Saves address calculation involved in C.M */
  if (tA)
  { if (tB)
    { if ((A.r!=B.c)||(A.c!=C.r)||(B.r!=C.c))
      { ErrorMessage(_("Incompatible matrices in matmult."),1);}
      for (i=0;i<A.c;i++) for (j=0;j<B.r;j++)
      { p2=CM[i]+j;(*p2)=0.0;p=BM[j];
	for (k=0;k<A.r;k++)
	(*p2)+=AM[k][i]*(*p++);
      }
    } else
    { if ((A.r!=B.r)||(A.c!=C.r)||(B.c!=C.c))
      { ErrorMessage(_("Incompatible matrices in matmult."),1);}
      for (i=0;i<A.c;i++)
      for (p=CM[i];p<(CM[i]+C.c);p++)
      (*p)=0.0;
      for (k=0;k<A.r;k++) for (i=0;i<A.c;i++)
      { temp=AM[k][i];p1=BM[k];
	for (p=CM[i];p<(CM[i]+B.c);p++)
	(*p)+=temp*(*p1++);
      }
    }
  } else
  { if (tB)
    { if ((A.c!=B.c)||(A.r!=C.r)||(B.r!=C.c))
      { ErrorMessage(_("Incompatible matrices in matmult."),1);}
      for (i=0;i<A.r;i++) for (j=0;j<B.r;j++)
      { p2=CM[i]+j;*p2=0.0;p1=BM[j];
	for (p=AM[i];p<(AM[i]+A.c);p++)
	(*p2)+=(*p)*(*p1++);
      }
    } else
    { if ((A.c!=B.r)||(C.r!=A.r)||(C.c!=B.c))
      { ErrorMessage(_("Incompatible matrices in matmult."),1);}
      for (i=0;i<A.r;i++) for (p=CM[i];p<(CM[i]+B.c);p++) *p=0.0;
      for (k=0;k<A.c;k++) for (i=0;i<A.r;i++)
      { p1=BM[k];temp=AM[i][k];
	     for (p=CM[i];p<(CM[i]+B.c);p++)
	     (*p)+=temp*(*p1++);
      }
    }
  }
}


void mtest()

/* debugging routine which exercises the matrix storage system */

{ matrix M[1000];
  int i,n=1000,j,k;
  for (i=0;i<n;i++)
  { M[i]=initmat(30L,30L);
    for (j=0;j<30;j++)
    for (k=0;k<30;k++)
    M[i].M[j][k]=(double) k*i;
  }
  for (i=0;i<n;i++) freemat(M[i]);
}

#ifdef SUNC
void multi(va_alist) va_dcl
#else
void multi(int n,matrix C,...)
#endif

/* procedure for multiplying long strings of matrices. For example if we
   want to find Q=A'BAZ then use the command:
   multi(4,Q,A,B,A,Z,1,0,0,0);
   THIS FUNCTION HAS NOT BEEN TRIED USING SUN C */

{ matrix temp0,temp1,*M;
  long r,c;
  int *t,i;
  va_list argptr;

#ifdef SUNC
  matrix C;int n;
  va_start(argptr);
  n=va_arg(argptr,int);
  C=va_arg(argptr,matrix);
#else
  va_start(argptr,C);
#endif
  t=(int *)R_chk_calloc((size_t)n,sizeof(int));
  M=(matrix *)R_chk_calloc((size_t)n,sizeof(matrix)); /* previously no cast for sun C ??! */
  for (i=0;i<n;i++) M[i]=/*(matrix)*/ va_arg(argptr,matrix);
  for (i=0;i<n;i++) t[i]=(int) va_arg(argptr,int);
  if (t[0]) r=M[0].c; else r=M[0].r;
  if (t[1]) c=M[1].r; else c=M[1].c;
  if (n>2) temp0=initmat(r,c);else temp0=C;
  matmult(temp0,M[0],M[1],t[0],t[1]);
  for (i=1;i<n-2;i++)
  { r=temp0.r;
    if (t[i+1]) c=M[i+1].r; else c=M[i+1].c;
    temp1=initmat(r,c);
    matmult(temp1,temp0,M[i+1],0,t[i+1]);
    freemat(temp0);temp0=temp1;
  }
  if (n>2)
  { matmult(C,temp0,M[n-1],0,t[n-1]);
    freemat(temp0);
  }
  R_chk_free(t);
  R_chk_free(M);
  va_end(argptr);
}


void invert(matrix *A)

/* Matrix inversion by Guass-Jordan Elimination with full pivoting.
   See "Numerical Recipes", and Burden and Faires "Numerical Analysis", for basis
   of method (but not actual code). This version written as part of elimination of 
   "Numerical Recipes" routines from my code. Tested against Numerical Recipes code 
   with a variety of random matrices - fine on accuracy and speed. 13/1/2000
 */

{ double **AM,*p,*p1,max,x;
  int *c,*rp,*cp,i,j,k,pr=0,pc=0,*d,cj,ck;
  if (A->r!=A->c) ErrorMessage(_("Attempt to invert() non-square matrix"),1);
  c=(int *)R_chk_calloc((size_t)A->c,sizeof(int)); /* index of columns, used for column pivoting */
  d=(int *)R_chk_calloc((size_t)A->c,sizeof(int));
  rp=(int *)R_chk_calloc((size_t)A->c,sizeof(int)); /* row changes */
  cp=(int *)R_chk_calloc((size_t)A->c,sizeof(int)); /* row changes */
  for (i=0;i<A->c;i++) { c[i]=i;d[i]=i;}
  AM=A->M;            /* saving adress calculations*/
  for (j=0;j<A->c;j++) /* loop through columns to be reduced */
  { max=0.0; 
    for (i=j;i<A->r;i++) /* loop through rows to search for pivot */
    { p=AM[i];
      for (k=j;k<A->c;k++) /* loop through cols to search for pivot */
      { x=p[c[k]];if (fabs(x)>max) { max=fabs(x);pr=i;pc=k;}}
    }
    /* now move pivot to element j,j */
    p=AM[j];AM[j]=AM[pr];AM[pr]=p; /* rows exchanged */
    k=c[j];c[j]=c[pc];c[pc]=k;   /* columns exchanged */
    rp[j]=pr;  /* stores row pivoted with */
    cp[j]=pc;  /* stores column pivoted with */
    cj=c[j]; /* save time */
    /* Now reduce the column */
    x=AM[j][cj];
    if (x==0.0) ErrorMessage(_("Singular Matrix passed to invert()"),1);
    for (p=AM[j];p<AM[j]+A->c;p++) *p/=x; /* divide row j by pivot element */
    AM[j][cj]=1.0/x;
    for (i=0;i<A->r;i++) /* work down rows eliminating column j */
    { p=AM[i];p1=AM[j];
      if (i!=j)
      { x = -p[cj]; /* multiplier for this row */
        for (k=0;k<j;k++) /* work through columns of the inverse matrix */
        { ck=c[k];p[ck]+=x*p1[ck];}
        p[cj]=x*p1[cj]; /* new column for inverse (entries in A implicitly zeroed except jj) */
        for (k=j+1;k<A->c;k++)   /* cols of A */
        { ck=c[k];p[ck]+=x*p1[ck];}
      }
    }
  } 
 
  for (i=A->r-1;i>=0;i--) /*work down through column re-ordering  */
  { if (cp[i]!=i)
    { p=AM[i];AM[i]=AM[cp[i]];AM[cp[i]]=p; /* row exchange */
    }
  }
  
  for (j=0;j<A->c-1;j++) /* implement column exchange */
  if (c[j]!=j)
  { if (c[j]<j) k=c[c[j]]; else k=c[j]; 
    for (i=0;i<A->r;i++)
    { p=AM[i];x=p[j];p[j]=p[k];p[k]=x;}  
    d[k]=d[j];d[j]=c[j];
    c[d[k]]=k;
  } 
  
  for (i=A->r-1;i>=0;i--) /* column exchange implied by row re-ordering */
  if (rp[i]!=i)
  { for (k=0;k<A->r;k++) 
    { p=AM[k];x=p[i];p[i]=p[rp[i]];p[rp[i]]=x;} /* column exchange  */
  }
   
  R_chk_free(c);R_chk_free(rp);R_chk_free(cp);R_chk_free(d);
}

void tricholeski(matrix *T,matrix *l0,matrix *l1)

/* Routine for choleski decomposition of the tridiagonal matrix T. If L is the
   matrix with leading diagonal in vector l0 and leading subdiagonal in l1, then
   T=LL'. The routine is O(n). Note that not only is multiplication of a matrix
   by L 0(n^2), but formation of A=inv(L)B can be done in O(n^2) by solution
   of LA=B. Note also that the trace of the inverse of a tri-diagonal matrix
   can be found cheaply using Elden's trick - see p137 of Wahba.
   20/11/99: Now has steps to deal with +ve semi definite matrices by a slight 
   modification of the choleski decomposition. The modification zeros 
   elements of the decomposition, as needed.

   The zeroing steps are valid for +ve semi-definite (non-negative definite)
   matrices (only), for the following reasons:
   0. a +ve semi definite matrix that is not +ve definite, must have zero eigenvalues
      as is easily seen by spectral decomposition.
   1. A properly tri-diagonal symmetric rank m (sub)matrix  has m or m+1 non-zero 
      rows and columns. This can be seen by counting up the number of possible 
      independent rows and columns of an m+1 by m+1 properly tri-diagonal matrix.
   2. So a +ve semi-definite properly tridiagonal (sub) matrix has at most 1 
      non-independent row/col, i.e. at most one zero eigenvalue.
   3. When present, this non-independence  leads to a zero on the final element of 
      the leading diagonal of the choleski decomposition of a properly tridiagonal +ve
      semi-definite (sub)matrix (I thought I had a proof of this, but am now not sure!). 
   4. Zeroing outside properly tridiagonal sections of the matrix is clearly ok.
   Note that these arguments only hold for +ve semi-definite tri-diagonal matrices.
   Merely being symmetric won't do, and neither will merely being +ve semi-definite!

   NOTE: that this routine will not detect none +ve semi-definite matrices, the only
         way to do that is to check whether the decomposition actually works (it won't 
         for a matrix that isn't +ve semi-definite)
 */

{ double **TM,*l1V,*l0V,z=1.0;
  long k1,k;
  TM=T->M;l0V=l0->V;l1V=l1->V;
  l0V[0]=sqrt(TM[0][0]);
  for (k=1;k<T->r;k++)
  { k1=k-1;
    if (z>0.0) l1V[k1]=TM[k][k1]/l0V[k1];   /* no problem */
    else l1V[k1]=0.0; /* assume TM[k][k1]=0, so no problem */
    z=l1V[k1];z=TM[k][k]-z*z;
    if (z>0.0) l0V[k]=sqrt(z);
    else 
    l0V[k]=0.0;
  }
}



double dot(a,b) matrix a,b;

{ long i,k=0L;double c=0.0,*p,*p1;
  if (a.vec) { p1=b.V;for (p=a.V;p<a.V+a.c*a.r;p++) c+=(*p)*(*p1++);}
  else
  for (i=0;i<a.r;i++) for (p=a.M[i];p<(a.M[i]+a.c);p++)
  { c+=(*p)*b.M[k/b.c][k%b.c];k++;}
  return(c);
}

double mean(a) matrix a;

/* gets mean of a */

{ int i;
  double m=0.0,*aV;
  aV=a.V;
  for (i=0;i<a.r*a.c;i++) m+=aV[i];
  m/=(a.r*a.c);
  return(m);
}



double enorm(d) matrix d;

/* Euclidian norm of vector d, or rms for matrix */

{ double e=0.0,m=0.0,y,*p;
  long i;
  if (d.vec) for (p=d.V;p<d.V+d.r*d.c;p++) { y=fabs(*p); if (y>m) m=y; }
  else for (i=0;i<d.r;i++) for (p=d.M[i];p<d.M[i]+d.c;p++) 
  { y=fabs(*p);if (y>m) m=y;}/* m=max(m,fabs(*p)); */
  if (!m) return(0.0);
  if (d.vec) for (p=d.V;p<d.V+d.r*d.c;p++)
  { y= *p / m; e+=y*y;} else
  for (i=0;i<d.r;i++) for (p=d.M[i];p<(d.M[i]+d.c);p++)
  { y= *p / m;e+=y*y;}
  e=sqrt(e)*m;
  return(e);
}





void householder(u,a,b,t1) matrix *u,a,b;long t1;

/* transforms a to b, iff they are of equal Euclidian length. u is the
   (t1+1) vector such that the full post multiplying householder matrix is
   H' = [ I - vv' ]   where v'  = [u',0] where 0 is a vector of zeroes.   */

{ long i;double v,*aV,*bV,*uV;
  aV=a.V;bV=b.V;uV=u->V;
  u->r=t1+1;
  for (i=0;i<u->r;i++) uV[i]=aV[i]-bV[i];
  v=enorm((*u))/sqrt(2.0);
  for (i=0;i<u->r;i++) uV[i]/=v;
}



void Hmult(C,u) matrix C,u;

/* This routine is for post multiplication by Housholder matrices only */

{ double temp,*p,*p1,*uV,**CuM,**CM;
  long i,j;
  matrix Cu;
  Cu=initmat(C.r,u.c);
  uV=u.V;CuM=Cu.M;CM=C.M;
  for (i=0;i<Cu.r;i++)
  { p1=CuM[i];(*p1)=0.0;p=CM[i];
    for (j=0;j<u.r;j++) (*p1)+=(*p++)*uV[j];
  }
  for (i=0;i<Cu.r;i++)
  { temp=Cu.V[i];p=CM[i];
    for (j=0;j<u.r;j++)
    (*p++) -= temp*uV[j];
  }
  freemat(Cu);
}

void HQmult(C,U,p,t) matrix C,U;int p,t;

/* This routine is for multiplying by householder matrices, stored as a
   series of vectors of the type u defined in routine householder above.
   If ui is ith row of U, then Hi=(I-ui ui') = Hi'. Let Q = H1 H2 H3 ....
   then Q' = .... H3 H2 H1.
   Returned in C: p==0,t==0 => CQ; 
                  p==0,t==1 => CQ'; 
                  p==1,t==0 => QC;
		          p==1,t==1 => Q'C
   
   NOTE that the routine expects C to be compatible with the Hi's - if
   this routine is being used for projection in and out of Null spaces, then
   make sure that C is appropriately packed with zeroes.

   If appropriate zero packing conventions have been used then OrthMult() is
   more efficient....
   */

{ double *u,*CuV,**CM;
  matrix Cu;
  long i,j,k;
  if (p) Cu=initmat(C.c,1L);else Cu=initmat(C.r,1L);
  CuV=Cu.V;CM=C.M;
  if (p)
  { if (t)
    { for (k=0;k<U.r;k++) /* loop through the householder matrices */
      { u=U.M[k];
        for (i=0;i<C.c;i++)
	     { CuV[i]=0.0;
          for (j=0;j<C.r;j++) CuV[i]+=CM[j][i]*u[j];
	     }
	     for (i=0;i<C.r;i++) for (j=0;j<C.c;j++) CM[i][j] -= CuV[j]*u[i];
      }
    }  else
    { for (k=U.r-1;k>=0;k--) /* loop through the householder matrices */
      { u=U.M[k];
	     for (i=0;i<C.c;i++)
	     { CuV[i]=0.0;
	       for (j=0;j<C.r;j++) CuV[i]+=CM[j][i]*u[j];
	     }
	     for (i=0;i<C.r;i++) for (j=0;j<C.c;j++) CM[i][j] -= CuV[j]*u[i];
      }
    }
  } else  /* postmultiplication */
  { if (t)
    { for (k=U.r-1;k>=0;k--) /* loop through the householder matrices */
      { u=U.M[k];
	     for (i=0;i<C.r;i++)
	     { CuV[i]=0.0;
	       for (j=0;j<C.c;j++) CuV[i]+=CM[i][j]*u[j];
	     }
	     for (i=0;i<C.r;i++) for (j=0;j<C.c;j++) CM[i][j] -= CuV[i]*u[j];
      }
      } else
      { for (k=0;k<U.r;k++) /* loop through the householder matrices */
      { u=U.M[k];
	     for (i=0;i<C.r;i++)
	     { CuV[i]=0.0;
	       for (j=0;j<C.c;j++) CuV[i]+=CM[i][j]*u[j];
	     }
	     for (i=0;i<C.r;i++) for (j=0;j<C.c;j++) CM[i][j] -= CuV[i]*u[j];
      }
    }
  }
  freemat(Cu);
}


void QT(Q,A,fullQ) matrix Q,A;int fullQ;

/* Uses householder matrices to perform the factorization of A (nxm),n<=m: */
/*                  AQ=[0,T]  where Tij=0 if i+j<n for T an (nxn) matrix   */
/*                    i.e. T reverse lower triangular - top left is zero   */
/* if fullQ!=0 then Q is formed explicitly (what a waste!), otherwise the  */
/* first A.r rows of Q will be used to store the vectors u used to define  */
/* the householder matrix. These can be used to multiply some other matrix */
/* using the routine HQmult. Revised 13/1/2000 - more efficient, over/under*/
/* flow protected, cancellation error free, but still behaves as before.   */
/* Tested using variety of random matrices.                                */

{ long i,j,Ar,Ac,k;
  double lsq,*p,*p1,**QM,**AM,g,x,m;
  QM=Q.M;AM=A.M;Ar=A.r;Ac=A.c;
  if (fullQ) for (i=0;i<Ac;i++) 
  { p=QM[i];
    for (j=0;j<Ac;j++) if (i==j)
    p[j]=1.0; else p[j]=0.0;
  }
  if (Ar>0)
  { for (i=0;i<Ar;i++)
    { /* rotate elements 0 to A.c-i-1 row i of A into element A.c-i-1 of that row  */
      p=AM[i];
      m=0.0;for (j=0;j<Ac-i;j++) { x=p[j];x=fabs(x);if (x>m) m=x;} /* scale factor */
      if (m) for (j=0;j<Ac-i;j++) p[j]/=m; /* avoid over/underflow */
      lsq=0.0;for (j=0;j<Ac-i;j++) lsq+=p[j]*p[j];
      lsq=sqrt(lsq);
      if (p[Ac-i-1]<0.0) lsq= -lsq;
      p[Ac-i-1]+=lsq;
      if (lsq)
      g=1/(lsq*p[Ac-i-1]); /* multiplier for HH rotation (I-g*uu') */
      else g=0.0;
      lsq*=m; /* Element to end up on A.M[i][A.c-i-1] */
      for (j=i+1;j<Ar;j++) /* Apply rotation down through the rows of A */
      { x=0.0;p1=AM[j];
        for (k=0;k<Ac-i;k++) x+=p[k]*p1[k];
        x*=g;
        for (k=0;k<Ac-i;k++) p1[k] += -x*p[k];
      } 
      if (fullQ)
      for (j=0;j<Q.r;j++) /* down through rows of Q */
      { x=0.0;p=AM[i];p1=QM[j];
        for (k=0;k<Ac-i;k++) x+=p[k]*p1[k];
        x*=g;
        for (k=0;k<Ac-i;k++) p1[k] += -x*p[k];
      } else
      { g=sqrt(g);
        p=QM[i];p1=AM[i]; /* address saving */
        for (j=0;j<Ac-i;j++) p[j]=p1[j]*g;
        for (j=Ac-i;j<Ac;j++) p[j]=0.0;  
      }
      AM[i][Ac-i-1]=-lsq;
      for (j=0;j<Ac-i-1;j++) AM[i][j]=0.0;
    }
  }
}


void OrthoMult(matrix *Q,matrix *A,int off,int rows,int t,int pre,int o_pre)

/* The first `rows' of Q are vectors for a householder transformation. The ith
   vector starts with i+off zero elements (i starts at 0). The vectors are
   stored in the order that they should be applied. If o_pre==1 then they
   originally pre-multiplied, otherwise they originally post multiplied.
   if t==1 then the transform is transposed. If pre==1 then it is applied from
   the left.
   Each householder transform has the same form: (I-uu') (treating u as a column
   vector).
   The transformation is applied to A.
*/

{ double au,*u,*a,**AtM=NULL,**AM,**QM;
  long i,j,k,Ar,Qc,kk;
  matrix At;
  if (o_pre) t=1-t; /* default assumption is that creation was for post mult. */
  if (pre) /* use fact that QA=(A'Q')' and Q'A=(A'Q)' */
  { At=initmat(A->c,A->r);
    AM=A->M;AtM=At.M;
    for (i=0;i<A->r;i++) for (j=0;j<A->c;j++) AtM[j][i]=AM[i][j];
    t=1-t;
  } else At=*A;
  AM=At.M;QM=Q->M;Ar=At.r;Qc=Q->c;
  for (kk=0;kk<rows;kk++)
  { if (t) k=rows-1-kk; else k=kk;
    u=QM[k];
    for (i=0;i<Ar;i++)
    { a=AM[i];au=0.0;
      for (j=off+k;j<Qc;j++) au+=a[j]*u[j];
      for (j=off+k;j<Qc;j++) a[j] -= au*u[j];
    }
  }
  if (pre)
  { AM=A->M;
    for (i=0;i<At.r;i++) for (j=0;j<At.c;j++) AM[j][i]=AtM[i][j];
    freemat(At);
  }
}


void Rsolv(matrix *R,matrix *p,matrix *y, int transpose)

/* Solves Rp=y for p when R is upper triangular - i.e. lower left 0.
   dimensions of p and y are not checked
   if transpose!=0 then solves: R'p=y
   
*/

{ int i,j,k;
  double x,*pV,*yV,*RMi,**RM,*dum,**pM,**yM;
  pV=p->V;yV=y->V;
  if (y->r==1L) /* then p and y are vectors */
  { if (transpose) /* solve R'p=y for p */
    { RM=R->M;
      for (i=0;i<R->r;i++)
      { x=0.0;dum=pV;for (j=0;j<i;j++) { x+=RM[j][i] * *dum;dum++;}
        *dum=(yV[i]-x)/RM[i][i];
      } 
    } else /* solve Rp=y for p */
    for (i=R->r-1;i>=0;i--) 
    { RMi=R->M[i];
      x=0.0;for (j=i+1;j<R->r;j++) x+=RMi[j]*pV[j];
      pV[i]=(yV[i]-x)/RMi[i];
    }
  } else /* p and y are matrices */
  { pM=p->M;yM=y->M;
    if (transpose) /* solve R'p=y for p */
    { RM=R->M;
      for (k=0;k<p->c;k++)
      for (i=0;i<R->r;i++)
      { x=0.0;for (j=0;j<i;j++) x+=RM[j][i] * pM[j][k];
        pM[i][k]=(yM[i][k]-x)/RM[i][i];
      } 
    } else /* solve Rp=y for p */
    for (k=0;k<p->c;k++)
    for (i=R->r-1;i>=0;i--) 
    { RMi=R->M[i];
      x=0.0;for (j=i+1;j<R->r;j++) x+=RMi[j]*pM[j][k];
      pM[i][k]=(yM[i][k]-x)/RMi[i];
    }
  }
}


int QR(matrix *Q,matrix *R)

/* Does a QR factorisation of the matrix supplied in R. In Q the householder
   vectors are supplied to perform the transformation
   QR(in) -> R(out)
   R(out) is upper triangular (elements are 0 below leading diagonal).
   If Q->r is none zero then the vectors u are stored in successive rows of
   Q. The u vectors make up Q as a series of (stable) householder transformations.
   (I-uu'). The transformations are to be applied from the left in row order.
   The first i elements of the ith u are zero (i starting at zero).
   If A is the matrix input in R then QA=R, so that A=Q'R.
   Q can be used with OrthoMult(). 
   Under/overflow avoidance added 13/1/2000 along with more efficient calculation
   of length of u (modifications tested).
   
*/

{ long i,j,k,n,Rr;
  double *u,t,z,**RM,*p,m;
  RM=R->M;Rr=R->r;
  if (Rr<R->c) n=Rr; else n=R->c;
  u=(double *)R_chk_calloc((size_t)Rr,sizeof(double));
  for (k=0;k<n;k++)
  { m=0.0;for (i=k;i<Rr;i++) { z=RM[i][k];z=fabs(z);if (z>m) m=z;}
    if (m) for (i=k;i<Rr;i++) RM[i][k]/=m; /* avoid over/underflow problems */
    t=0.0;for (i=k;i<Rr;i++) { z=RM[i][k];t+=z*z;} /* get euclidean length of column */
    if (RM[k][k]>0.0) t = -sqrt(t);else t= sqrt(t);  /* value of new RM[k][k] (stable) */
    for (i=k+1;i<Rr;i++) { u[i]=RM[i][k];RM[i][k]=0.0;}
    z=RM[k][k]; 
    u[k]=RM[k][k]-t;RM[k][k]=t*m;
    t=t*t;t+=u[k]*u[k]-z*z; /* efficient t calculation */
    /* t=0.0;for (p=u+k;p<u+Rr;p++) t+= *p * *p; - old inefficient calculation */
    t=sqrt(t/2);
    if (t==0.0) {R_chk_free(u);return(0);} /* singular matrix */
    for (p=u+k;p<u+Rr;p++) *p /= t;
    for (j=k+1;j<R->c;j++)
    { t=0.0;for (i=k;i<Rr;i++) t+=u[i]*RM[i][j];
      for (i=k;i<Rr;i++) RM[i][j]-=u[i]*t;
    }
    if (Q->r) /* store vectors u for making Q */
    { p=Q->M[k];
      for (i=k;i<Rr;i++) p[i]=u[i];
    }
  }
  R_chk_free(u);
  return(1);
}


void interchange(matrix *M,long i,long j,int col)

/* interchanges rows i and j of M if col==0 otherwise cols i and j */

{ int k;
  double t,**MM;
  MM=M->M;
  if (col)
  for (k=0;k<M->r;k++) { t=MM[k][i];MM[k][i]=MM[k][j];MM[k][j]=t;}
  else
  for (k=0;k<M->c;k++) { t=MM[i][k];MM[i][k]=MM[j][k];MM[j][k]=t;}
}

void UTU(matrix *T,matrix *U)

/* does orthogonal tridiagonalisation of the symmetric matrix supplied in T;
   U is returned with successive householder vectors in successive
   rows of U. The first i+1 elements of the ith row will be zero (i starts at 0).
   There are only T->r - 2 non-zero rows. The transformations must be applied
   in order from the right.
   Recall that a householder transformation to take a->b is constructed as
   follows: u=a-b;  u=u/sqrt(u'u/2);  H=(I-uu'); then Ha=b and a'H=b'
   (of course to form Ha...  form u'a; form u(u'a); form a-u(u'a); never
    form H first!).
   If A is the input matrix then U'AU=T => A=UTU'
   Underflow and overflow protection added 13/1/2000, and lt 
   efficiency improved. Improved code tested with variety of 
   random matrices. (Householder rotations are stable.)
   OrthoMult() works with U storage convention used here.
*/

{ long i,j,k;
  double *u,*t,lt,x,m;
  for (i=0;i<T->r-2;i++)
  { u=U->M[i];t=T->M[i];lt=0.0;
    m=0.0;for (j=i+1;j<T->c;j++) { x=fabs(t[j]); if (m<x) m=x;} /* find max_j(|t_j|) for scaling */
    if (m) for (j=i+1;j<T->c;j++) t[j]/=m; /* avoid over/underflow */
    for (j=i+1;j<T->c;j++) lt+=t[j]*t[j];
    if (t[i+1]>0.0) lt= -sqrt(lt);else lt=sqrt(lt);  /* ensures stability (by maximising element i+1 of u) */
    x=t[i+1]; /* stored for altering lt efficiently */
    u[i+1]=lt-t[i+1];T->M[i+1][i]=t[i+1]=lt*m;
    lt*=lt;lt+= -x*x+u[i+1]*u[i+1];
    for (j=i+2;j<T->c;j++)
    { u[j]= -t[j];T->M[j][i]=t[j]=0.0;}
    if (lt>0.0) /* only do this if u is non-zero */
    { lt=sqrt(0.5*lt);
      for (j=i+1;j<T->c;j++) u[j]/=lt;
    }
    for (j=i+1;j<T->c;j++) /* apply rotations to remaining rows */
    { t=T->M[j];lt=0.0;
      for (k=i+1;k<T->c;k++) lt+=u[k]*t[k];
      for (k=i+1;k<T->c;k++) t[k] -= u[k]*lt;
    }
    /* Apply rotations from the left */
    for (j=i+1;j<T->c;j++)
    { lt=0.0;
      for (k=i+1;k<T->c;k++) lt+=u[k]*T->M[k][j];
      for (k=i+1;k<T->c;k++) T->M[k][j] -= u[k]*lt;
    }
  }
}




void root(matrix *M,matrix *C,double tol)

/* produces a square root of non-negative definite M, by obtaining M=UTU',
   then getting the choleski decomposition of the non-zero part of T. T=LL'
   so C=UL and M=CC'.... C will have as few columns as possible. C is
   initialised in this routine.

   Upgraded 20/11/99: Previous version assumed that zero rows and columns only
   occured at the end of T. This is no longer the case. tricholeski() has been
   modified to deal with non-negative definite T (although this may still be
   weakest link). If L contains a column of zeros then this column is ommitted
   from C altogether. zero is judged relative to tol multiplied by the maximum
   element on the leading diagonal of L if tol>0, otherwise zero is any number 
   that leads to no change in the maximum element when added to it. Algorithm 
   appears to be substantially better than svdroot() (and should be much quicker).

   Commented out code is the old code, left in in case of future difficulties:
   when it is removed some further tidying could be done.

   16/1/2000: tested with variety of random rank deficient matrices, and theoretical
   basis re-checked more carefully - see tricholeski() comments.

*/

{ matrix T,U,u0,u1;
  long i,j,k,rows;
  int fswap=0,ok;
  double max,uc,*u,x,m;
  T=initmat(M->r,M->c);
  U=initmat(M->r,M->c);
  for (i=0;i<T.r;i++) for (j=0;j<T.c;j++) T.M[i][j]=M->M[i][j];
  UTU(&T,&U);
  /* make absolutely symmetric */
  for (i=0;i<T.r-1;i++) T.M[i][i+1]=T.M[i+1][i]=(T.M[i+1][i]+T.M[i][i+1])*0.5;
  rows=T.r;
  u0=initmat(T.r,1L);u1=initmat(T.r-1,1L);
  u0.r=T.c=T.r=rows;u1.r=rows-1;
  tricholeski(&T,&u0,&u1);
  /* now check result, since I'm not quite happy with step 4 of theoretical justification
     of treatment of +ve semi-definiteness in tricholeski() */
  max=0.0;for (i=0;i<u0.r;i++) if (fabs(T.M[i][i])>max) max=fabs(T.M[i][i]);
  ok=1;x=u0.V[0]*u0.V[0]-T.M[0][0];m=0.0;
  if (x>m) m=x;
  for (i=1;i<T.r;i++) 
  { x=u1.V[i-1]*u0.V[i-1]-T.M[i][i-1];x=fabs(x);
    if (x>m) { m=x;k=i;}
    x=u1.V[i-1]*u1.V[i-1]+u0.V[i]*u0.V[i]-T.M[i][i];x=fabs(x);
    if (x>m) { m=x;k=i;}
  }
  if (m>10.0*DOUBLE_EPS*max) ok=0;
  if (!ok)
  { /*ErrorMessage("Using svd to find root of penalty!",0);*/ 
    (*C)=svdroot(*M,tol);
    freemat(U);freemat(T);freemat(u0);freemat(u1);
    return; 
  }
  freemat(T);
  T=initmat(U.r,u0.r);  
  
  /* now apply householder rotations from the left to obtain C=UL */
  for (i=0;i<u0.r;i++) /* row counter */
  { T.M[i][i]=u0.V[i];
    if (i<u0.r-1) T.M[i+1][i]=u1.V[i];
  }
  for (i=U.r-3;i>=0;i--)
  { u=U.M[i]; /* first i+1 elements of u are zero */
    for (j=0;j<T.c;j++)
    { uc=0.0;for (k=i+1;k<U.c;k++) uc += u[k]*T.M[k][j];
      for (k=i+1;k<U.c;k++) T.M[k][j] -= u[k]*uc;
    }
  }
  /* now remove zero columns */
  *C=initmat(U.r,u0.r);
  k=0;
  for (j=0;j<T.c;j++)
  { ok=0;
    for (i=0;i<T.r;i++)
    if (tol<=0.0) 
    { if ((T.M[i][j]+max!=max)||(T.M[i][j]+max!=max)) {ok=1;break;}}
    else
    { if ((fabs(T.M[i][j])>tol*max)||(fabs(T.M[i][j])>tol*max)) {ok=1;break;}}
    if (ok) /* then include this column */
    { for (i=0;i<C->r;i++) C->M[i][k]=T.M[i][j]; 
      k++;
    } 
  }
  C->c=k;
  if (fswap)
  { interchange(C,1L,0L,0);}
  freemat(T);freemat(U);freemat(u0);freemat(u1);
}


void bidiag(matrix *A,matrix *wl,matrix *ws,matrix *V)


/* This routine bi-diagonalizes A using Housholder rotations applied from 
   left and right. wl and ws are vectors of dimension A.c and A.c-1 respectively.
   V is an orthogonal A.c by A.c matrix. Let W be the matrix with wl as leading 
   diagonal and ws as leading superdiagonal, then: A = UWV' where U is 
   orthogonal and output in A. The routine's main use is as the first stage in 
   a singular value decomposition of A. 
   The Orthogonal matrices are composed of Householder rotations applied left, 
   right, left, right, left, right etc. The left rotations (reflectors) zero 
   elements of A in columns below the leading diagonal starting from the left. 
   The right reflectors zero elements of A in rows left of the first super diagonal
   starting from the top. Reflectors are calculated as described on p149 of 
   Watkins (1991) Fundamentals of Matrix Computations, Wiley (but note that 
   \gamma<-1/\sigma x_1 should read \gamma<-(\sigma x_1 )!!).  
   Reflectors are of the form H=(I-g*uu'), where u is a vector and g a scalar. 

   This routine has been tested on a variety of random matrices of differing dimensions
   as well as on strictly singular matrices. The tests checked that A = UWV'.

   Most important address optimization has been performed.

   9/1/2000
	 
*/

{ double m,s=0.0,g,temp,**AM,**VM,*p,*p1;
  int i,j,k,nv,nu;
  nv=0; /* counts up number of v_i's */
  AM=A->M;VM=V->M;
  for (i=0;i<A->c;i++)
  { wl->V[i]=0.0;if (i<A->c-1) ws->V[i]=0.0;
    if (i<A->r) /* zero subdiagonal column i */
	{ m=0.0;for (j=i;j<A->r;j++) { s=fabs(AM[j][i]); if (s>m) m=s;} /* max of column for scaling  */
      if (m==0.0) g=0.0; 
	  else /* work out reflector elements */
	  { s=0.0;for (j=i;j<A->r;j++) { AM[j][i]/=m;s+=AM[j][i]*AM[j][i];} /* scale reflector etc.  */
	    s=sqrt(s);
	    if (AM[i][i]<0.0) s = -s; /* avoid cancellation error */
        AM[i][i]+=s;
	    g=1/(AM[i][i]*s);
	    s*=m;
	  } /* Now u is stored in rows i to A.r of column i of A */
	  wl->V[i] = -s;
	  VM[i][i] = g; /* temporary storage for g, for use in later assembly of U  */
	  /* must apply reflector to remaining columns */
      for (j=i+1;j<A->c;j++)
      { s=0.0;for (k=i;k<A->r;k++) s+=AM[k][i]*AM[k][j];
	    s*=g;for (k=i;k<A->r;k++) AM[k][j] += -s*AM[k][i];
      }
    } 

	/* zero elements i+2 to A.c of row i ..... */
    if ((i<A->r) && (i<A->c-1))
    { m=0.0;/*for (j=i+1;j<A->c;j++) { s=fabs(AM[i][j]); if (s>m) m=s;} */ /* max for scaling */
      for (p=AM[i]+i+1;p<AM[i]+A->c;p++) { s=fabs(*p);if (s>m) m=s;} /* max for scaling */
      if (m==0.0) g=0.0;
      else
      { s=0.0;/*for (j=i+1;j<A->c;j++) { AM[i][j]/=m;s+=AM[i][j]*AM[i][j];} */
        for (p=AM[i]+i+1;p<AM[i]+A->c;p++) { *p/=m;s+=(*p)*(*p);}
        s=sqrt(s);
        if (AM[i][i+1]<0.0) s = -s; /* avoid cancellation error */
        AM[i][i+1] += s;
        g=1.0/(AM[i][i+1]*s);
        s*=m; 
      }
      ws->V[i] = -s;
      VM[i][i+1]=g; /* temporary storage */
      /* Now apply reflector to remaining rows */
      for (j=i+1;j<A->r;j++)
      { s=0.0;/*for (k=i+1;k<A->c;k++) s+=AM[i][k]*AM[j][k]; */
        p1=AM[j]+i+1;for (p=AM[i]+i+1;p<AM[i]+A->c;p++) { s+=(*p)*(*p1);p1++;}
        s*=g;/*for (k=i+1;k<A->c;k++) AM[j][k] += -s*AM[i][k]; */
        p1=AM[j]+i+1;for (p=AM[i]+i+1;p<AM[i]+A->c;p++) { *p1 += -s*(*p);p1++;}
      } 
      nv++; /* number of v_i's */
    }  
  } 
  /* At this point wl and ws are complete, but U and V are stored in A as reflector 
     vectors, with associated g values stored on the leading diagonal and leading
     superdiagonal of V. Now form U and V. 
     U is the first A.c columns of  U_1 U_2 U_3 .... i.e. U_1 U_2 U_3 ... [I,0]'(same 
     dim as A) where U_i = (I - g_i u_i u_i'): g_i is in V->M[i][i] and u_i is zero up to 
     element i, with the remaining elements stored in rows i to A.r of column i of A.
     V= V_1 V_2 V_3 ... where V_i = (I-d_i v_i v_i') where d_i=V->M[i][i+1]. 
     v_i is zero until element i+1, with remaining elements stored in columns i+1 to A.c 
     of row i of A. */
  /* first form V in order to free up space in A */
  /* initialize rows nv to A->c of V */
  nu=A->c; if (A->r<nu) nu=A->r; /* number of U_i's */
  for (i=nv+1;i<A->c;i++) 
  for (p=VM[i];p<VM[i]+A->c;p++) *p=0.0;
  for (i=A->c-1;i>nv;i--) { if (i<nu) AM[i-1][i]=VM[i][i];VM[i][i]=1.0;}
  for (i=nv-1;i>=0;i--) /* working down through the V_i's */
  { temp=VM[i+1][i+1];
    /* for (j=0;j<A->c;j++) VM[i+1][j]=0.0; */
    for (p=VM[i+1];p<VM[i+1]+A->c;p++) *p=0.0;
    VM[i+1][i+1]=1.0; /* initialize row of V */
    for (j=A->c-1;j>i;j--) /* columns affected by V_i */
    { s=0.0;p=AM[i]+i+1;for (k=i+1;k<A->c;k++) { s+=VM[k][j]*(*p);p++;}
      s*=VM[i][i+1];
      p=AM[i]+i+1;for (k=i+1;k<A->c;k++) { VM[k][j] += -s*(*p);p++;}
    }
    AM[i][i+1]=temp; /* temporary storage for g_i's  */
  }        
  /* Now all but first row and column of V are formed, but V->M[0][0] still contains
     g_0, while g_i is in AM[i-1][i] otherwise, so form U now and then finish off V */
  
  for (i=nu-1;i>=0;i--) /* work down through the u_i's */
  { if (i>0) g=AM[i-1][i]; else g=VM[0][0];
    for (j=0;j<i;j++) AM[j][i]=0.0; /* zeroing column above the diagonal */
    for (j=A->c-1;j>i;j--) /* columns above i are affected */
    { s=0.0;for (k=i;k<A->r;k++) s+= AM[k][i]*AM[k][j];
      s*=g;
      for (k=i;k<A->r;k++) AM[k][j] += -s*AM[k][i]; 
    } 
    /* as is column i itself.... */
    for (j=A->r-1;j>i;j--) AM[j][i]*= -g*AM[i][i];
    AM[i][i] = 1 - g*AM[i][i]*AM[i][i]; 
  }
  /* now finish off V */
  p=VM[0];for (i=0;i<A->c;i++) { *p=VM[i][0]=0.0;p++;}
  VM[0][0]=1.0; 
}   


void svd_bidiag(matrix *U, matrix *w, matrix *ws,matrix *V)


/* This routine produces a singular value decomposition of the matrix UWV', where:
   1. W is a di-diagonal matrix with leading diagonal w, and leading super diagonal ws.
   2. U and V are orthogonal.
   
   Because W is not always properly bi-diagonal the following steps are needed:

   i) Deflate the problem if possible, which may involve zeroing an element of the
      super-diagonal.
   ii) Check whether (deflated) bi-diagonal matrix can be partioned, if so find 
       start of final partition (again may need to zero an element on the super 
       diagonal)
   iii) Apply iteration of implicit QR algorithm (p405 Watkins) to the sub matrix 
        identified above.
   iv) Return to (i) assuming that there are singular values left to find.

   Note that the Givens Rotators used here are calculated as follows to avoid rounding problems:

   assume xj to be zeroed into xi:
   m = max(fabs(xi),fabs(xj));
   xi/=m;xj/=m
   r=sqrt(xi*xi+xj*xj);
   c=xi/r;s=xj/r;
   xi=m*r;xj=0.0;

   (c and s can obviously be applied to other vectors without needing to know m.)

   See page 271 of Watkins, for suggestion of how to test for zeroes.

   Most important address optimization has been done (stopped when it was as efficient as
   Numerical Recipes code). (Commented out code is pre-optimization, left in for readability)

   Tested against NR routine for a variety of random matrices and matrices that are rank 
   deficient in various ways.

   Check for m>0.0 added 15/5/00 - otherwise division by zero is possible, leading to failure
   of routine!

*/

{ double wnorm=0.0,x,y,s,c,m,r,a,b,sig,**VM,**UM,*wV,*wsV,*p1,*p2,tol; 
  int finished=0,end,start,i,j,k,maxreps=100;
  tol=DOUBLE_EPS; /* convergence tolerance */
  VM=V->M;UM=U->M;wV=w->V;wsV=ws->V;
  for (i=0;i<ws->r;i++) /* get something against which to judge zero */
  { x=fabs(wV[i]);y=fabs(wsV[i]);if (x<y) x=y;if (wnorm<x) wnorm=x;}
  end=w->r-1;
  while (!finished)
  { for (k=0;k<maxreps;k++) /* QR iteration loop */
    { /*if (wV[end]+wnorm==wnorm)*/ /* zero singular value - can deflate */
      if (fabs(wV[end])<=tol*wnorm)
      { /*if (wsV[end-1]+wnorm!=wnorm)*/ /* need to zero wsV[end-1] before deflating */
        if (fabs(wsV[end-1])>tol*wnorm)
        { /* Series of rotators (Givens rotations from right) zero this element */
          y=wsV[end-1];wsV[end-1]=0.0;
          for (i=end-1;i>=0;i--) /* work out sequence of rotations */
          { m=fabs(y);x=fabs(wV[i]); if (x>m) m=x;
            x=wV[i];
            if (m>0.0) 
            { y/=m;x/=m; /* now rotate y into x */
              r=sqrt(y*y+x*x);
              c=x/r;s=y/r;
            } else {r=0.0;c=1.0;s=0.0;}
            wV[i]=r*m; /* rotation zeros y (implicitly) */
            if (i>0) /* propagate the problem element! */
            { y= -wsV[i-1]*s;
              wsV[i-1]*=c;
            } 
            /* Need to update V as well V -> V G where G is the rotation just applied.... */ 
            for (j=0;j<V->r;j++) /* work down the rows */
            { p2=VM[j]+end;p1=VM[j]+i;x=*p1; /*x=VM[j][i]; */
              /*VM[j][i]=c*x+s*VM[j][end]; */
              *p1=c*x+s*(*p2);
              /*VM[j][end]*=c;VM[j][end] += -s*x; */
              *p2 *= c; *p2 += -s*x;
            }
          }
        }
        end--; /*   */
       /* Check here for termination .....  */
        if (end<=0) finished=1;
        break; /* successfully deflated, so start new QR iteration cycle or finish */
      } else 
      if (fabs(wsV[end-1])<=tol*wnorm)  /* inelegant condition needed because below can fail in R because of register optimizations */
      /*if  (wsV[end-1]+wnorm==wnorm)*/ /*too restrictive?? wV[end] is a singular value => deflate  */
      { end--;
        if (end==0) finished=1; /* all elements of ws are zeroed so we're done */
        break; /* deflated so start new QR cycle or finish */
      } else /* no deflation possible, search for start of sub-matrix  */
      { start=end-1;
        /* while ((wnorm+wV[start]!=wnorm)&&(wnorm+wsV[start]!=wnorm)&&(start>=0)) start--; R needs less elegant version below*/
        while ((fabs(wV[start])>tol*wnorm)&&(fabs(wsV[start])>tol*wnorm)&&(start>=0)) start--;
        start++; /* this is now the row and column starting the sub-matrix */
        /*if ((start>0)&&(wnorm+wV[start-1]==wnorm)&&(wnorm+wsV[start-1]!=wnorm)) R needs sloppier version in order to use fp register opts. */
        if ((start>0)&&(fabs(wV[start-1])<=tol*wnorm)&&(fabs(wsV[start-1])>tol*wnorm)) 
        { /* ws.V[start-1] must be zeroed.... */
          y=wsV[start-1];wsV[start-1]=0.0;
          for (i=start;i<=end;i++) /* get sequence of rotators from left.... */
          { m=fabs(y);x=fabs(wV[i]); if (x>m) m=x;
            x=wV[i];
            if (m>0.0)
            { x/=m;y/=m;
              r=sqrt(x*x+y*y);
              c=x/r;s=y/r;
            } else {r=1.0;c=1.0;s=0.0;}
            wV[i]=r*m; /* y zeroed implicitly */
            if (i<end) /* propagate the problem element now at (start-1,i) */
            { y= -s*wsV[i];
              wsV[i]*=c;
            }  
            /* Now U must be updated, by transposed rotators - from the right */
            for (j=0;j<U->r;j++) /* work down the rows */
            { p1=UM[j]+start-1;x = *p1;p2=UM[j]+i;/*x=UM[j][start-1]; */
              /* UM[j][start-1] = c*x-s*UM[j][i]; */
              *p1 = c*x - s*(*p2);
              /* UM[j][i]*=c; UM[j][i] += +s*x; */
              *p2 *= c; *p2 += s*x; 
            } 
          }
        }
      }  
      /* iterate QR algorithm on sub-matrix */
      /* First find the Wilkinson shift which is given by the eigenvalue of the 
         bottom right 2 by 2 submatrix, closest to the final matrix element. 
         The required eigenvalues are found directly from the characteristic equation.
         See page 405 of Watkins.*/
      
      a=wV[end-1]*wV[end-1]+wsV[end-1]*wsV[end-1];b=wV[end];b*=b;
      c=wV[end]*wsV[end-1];
      y=sqrt((a-b)*(a-b)+4*c*c)/2;
      x=(a+b)/2+y;y=(a+b)/2-y; /* x and y are the eigenvalues */
      if (fabs(x-b)<fabs(y-b)) sig=x; else sig=y;
     
      /* ...... this could be improved!! */
      /* Now apply first step and then chase the bulge ...... */
    
      x=wV[start];
      y=wsV[start]*x;
      x=x*x-sig; /* x and y are the first and second elements of the rotator starting implicit QR step */
      m=fabs(x);if (fabs(y)>m) m=fabs(y);
      if (m>0.0)
      { y/=m;x/=m; /* avoid over/underflow */
        r=sqrt(y*y+x*x);
        c=x/r;s=y/r; /* elements of rotator to apply from right operating in start,start+1 plane */
      } else { r=1.0;c=1.0;s=0.0;}
      for (i=start;i<end;i++) 
      { /* start with post-multiplication */
        if (start<i) /* then rotator needs to be calculated to remove element at (i-1,i+1) (stored in y) */
        { x=wsV[i-1]; /* location y rotated into */
          m=fabs(y);if (fabs(x)>m) m=fabs(x);
          if (m>0.0)
          { x/=m;y/=m;   /* avoiding overflow */
            r=sqrt(x*x+y*y);
            c=x/r;s=y/r;
          } else {r=1.0;c=1.0;s=0.0;} /* rotator for zeroing y (at i-1,i+1) int x at (i-1,i) */
          wsV[i-1]=r*m;y=0.0;
        }
        /* now apply rotator from right to rows i and i+1.... */
        x=wV[i];
        wV[i]=c*x+s*wsV[i];
        wsV[i]=c*wsV[i]-s*x;
        y=s*wV[i+1];wV[i+1]*=c; /* y contains the bulge at (i+1,i) */
        /* and also apply from right to V.... */
        for (j=0;j<V->r;j++) /* work down the rows */
        { p1=VM[j]+i;x= *p1;p2=VM[j]+i+1; /*x=VM[j][i]; */
          /*VM[j][i]=c*x+s*VM[j][i+1]; */
          *p1=c*x + s* (*p2);
          /*VM[j][i+1]*=c;VM[j][i+1] += -s*x; */
          *p2 *= c; *p2 += -s*x;
        }  
          /* Obtain rotator from left to zero element at (i+1,i) into element at (i,i) 
           thereby creating new bulge at (i,i+2) */ 
        x=wV[i];
        m=fabs(y);if (fabs(x)>m) m = fabs(x);
        if (m>0.0)
        { x/=m;y/=m; /* avoid overflow */
          r=sqrt(x*x+y*y);
          c=x/r;s=y/r;
        } else {r=1.0;c=1.0;s=0.0;} /* transform to zero y into x (i+1,i) into (i,i) */
        wV[i]=r*m;y=0.0; 
        /* apply from left.... */
        x=wsV[i];
        wsV[i]=c*x+s*wV[i+1];
        wV[i+1]=c*wV[i+1]-s*x;
        if (i<end-1)
        { y=wsV[i+1]*s;
          wsV[i+1]*=c;
        } 
        /* and apply transposed rotator from right to U  */
        for (j=0;j<U->r;j++) /* work down the rows */
        { p1=UM[j]+i;x= *p1;p2=UM[j]+i+1;/*x=UM[j][i]; */
          /*UM[j][i]=c*x+s*UM[j][i+1]; */
          *p1=c*x+s*(*p2);
          /*UM[j][i+1]*=c; UM[j][i+1] += -s*x; */
          *p2 *= c; *p2 += -s*x;
        } 
      }
    }  
    if (k==maxreps) 
    ErrorMessage(_("svd() not converged"),1);
  }
  /* make all singular values  non-negative */
  for (i=0;i<w->r;i++) 
  if (wV[i]<0.0)
  { wV[i]= -wV[i];
    for (j=0;j<V->r;j++) VM[j][i]= -VM[j][i];
  } 
}

  

void svd(matrix *A, matrix *w, matrix *V)


/* This routine produces a singular value decomposition of A. On exit V will be 
   an A.c by A.c orthogonal matrix, w will be a vector of A.c singular values 
   and A will contain U of the same dimension as A such that U'U=I. If W is
   the diagonal matrix with w as its leading diagonal then: 
                        A=UWV'   - the singluar value decomposition.
   This routine is based on:
   Watkins (1991) Fundamentals of Matrix Computations, Wiley. (see section 7.2)
   The algorithm has 2 steps:
   1. Bi-diagonalise A using reflectors (Householder transformations) from left 
      and right - this is achieved by routine bidiag(), above.
   2. Find singular values of Bi-diagonal matrix. This is achieved by routine 
      svd_bidiag(), above.

*/

{ matrix *U,ws;
  int i;
  if (A->c==1) /* then the svd is trivial to compute */
  { w->V[0]=0.0;
    for (i=0;i<A->r;i++) w->V[0]+=A->M[i][0]*A->M[i][0];
    w->V[0]=sqrt(w->V[0]);
    for (i=0;i<A->r;i++) A->M[i][0]/=w->V[0]; 
    V->M[0][0]=1.0;
    return;
  }
  ws=initmat(w->r-1,1L);
  /* bi-diagonalize A, so A=UWV', w = l.diag(W), ws=l.super.diag(W), A contains U   */
  bidiag(A,w,&ws,V);  
  U=A;
  /* Now call svd_bidiag() for a result..... */
  svd_bidiag(U,w,&ws,V);
  freemat(ws);
}








matrix svdroot(matrix A,double reltol)

/* Finds smallest squareroot of a non-negative definite matrix. reltol is
   used to decide which columns to remove... */

{ long k=0l,i,j;
  double tol=0.0,prod;
  char err[100];
  matrix a,v,w;
  a=initmat(A.r,A.c);mcopy(&A,&a);
  v=initmat(A.r,A.c);
  w=initmat(A.r,1L);
  svd(&a,&w,&v);   /*a * diag(w) * v' */
  for (i=0;i<w.r;i++) { w.V[i]=sqrt(w.V[i]);if (w.V[i]>tol) tol=w.V[i];}
  tol*=sqrt(reltol);
  for (i=0;i<w.r;i++)
  { if (w.V[i]>tol)
    { for (j=0;j<a.c;j++) v.M[j][k]=a.M[j][i]*w.V[i];k++;
      prod=0.0;for (j=0;j<a.r;j++) prod+=a.M[j][i]*v.M[j][i];
      if (prod<0.0) 
	  { sprintf(err,_("svdroot matrix not +ve semi def. %g"),w.V[i]*w.V[i]);
		ErrorMessage(err,1); 
	  }
    }
  }
  v.c=k;
  freemat(a);freemat(w);
  return(v);
}



int elemcmp(const void *a,const void *b)

{
  if (*(double *)a<*(double *)b) return(-1);
  if (*(double *)a>*(double *)b) return(1);
  return(0);
}


void sort(matrix a)

/* sorts a vector, in situ, using standard routine qsort */

{ int i;
  qsort(a.V,(size_t)a.r*a.c,sizeof(a.V[0]),elemcmp);
  for (i=0;i<a.r*a.c-1;i++) if (a.V[i]>a.V[i+1])
  ErrorMessage(_("Sort failed"),1);

}





int real_elemcmp(const void *a,const void *b,int el)

{ static int k=0;
  int i;
  double *na,*nb;
  if (el>=0) { k=el;return(0);}
  na=(*(double **)a);nb=(*(double **)b);
  for (i=0;i<k;i++) 
  { if (na[i]<nb[i]) return(-1);
    if (na[i]>nb[i]) return(1);
  }
  return(0);
}

int melemcmp(const void *a,const void *b)

{ return(real_elemcmp(a,b,-1));
}


void msort(matrix a)

/* sorts a matrix, in situ, using standard routine qsort so 
   that its first col is in ascending order, its second col
   is in ascending order for any ties in the first col, and 
   so on.....
*/

{ double z=0.0;
  real_elemcmp(&z,&z,a.c); 
  qsort(a.M,(size_t)a.r,sizeof(a.M[0]),melemcmp);
}

void RArrayFromMatrix(double *a,long r,matrix *M)

/* copies matrix *M into R array a where r is the number of rows of A treated as
  a matrix by R */

{ int i,j;
  for (i=0;i<M->r;i++) for (j=0;j<M->c;j++) a[i+r*j]=M->M[i][j];
}


matrix Rmatrix(double *A,long r,long c)

/* produces a matrix from the array containing a (default) R matrix stored:
   A[0,0], A[1,0], A[2,0] .... etc */

{ int i,j;
  matrix M;
  M=initmat(r,c);
  for (i=0;i<r;i++) for (j=0;j<c;j++) M.M[i][j]=A[i+j*r];
  return(M);
}



/*********************************************************************************/
/* Update Log (started Jan 2000)                                                 */
/*********************************************************************************/

/* 1. Numerical Recipes code removed. This meant writing 2 new routines: svd() and
      invert(), to replace versions that were just modified from Numerical Recipes.
      An extra routine bidiag() was also written to bidiagonalize a matrix.
      Routines were heavily tested against old NR routines and perform at least as 
      well. See internal documentation for algorithm description and references.
      Both written to be as robust as possible. 12/1/2000
   2. UTU() and QR() modified to remove danger of under/overflow in householder 
      calculations, and increase efficiency. Tested using random matrices. 13/1/2000
   3. Dependence on inverse.h removed, options for SUNC flag removed. 8/1/2000
   4. QT() re-written - now much more efficient (consistent with producing output
      in its old format), no cancellation error, under/overflow protected, address
      optimized. NOTE: householder() and Hmult() no longer used in matrix.c -
      check qp.c before removing!
   6. NOTE: choleskir1ud() has not been checked for underflow/overflow and stability.
   7. tricholeski() has been checked again with a variety of rank deficient tridiagonal
      matrices. Theoretical basis for approach has been much strengthened, but I still
      can not *prove* that the choleski factor will require a zero only on the *final* 
      element of the main diagonal for a properly tri-diagonal matrix. Hence root() now 
      has a check that the  "choleski" factors of the tridiagonal matrix are adequate - 
      if not singular value decomposition is used instead. root() also has an improved 
      column removal strategy. 16/1/2000 
  
   8. This -> if (wsV[end-1]+wnorm==wnorm) test for convergence of singular values is 
      rather too stringent! Replaced with something more reasonable. 25/2/2000
      15/5/2000 - convergence criteria changed back - eigenvalue used for shift was
      calculated in a manner that allowed bad cancellation errors - this caused 
      convergence problems AND occasional imaginary eigenvalues for the shift. Fixing
      this fixed the convergence problem.

   9. svd() bug fixed - it division by zero was possible while trying to avoid over/
      underflow in Givens rotations - check added to fix problem. 15/5/00

  10. initmat() problem fixed. There was a hangover from segmented memory architectures 
      left in initmat(): this meant that vectors could not be accessed using a.V[i]
      if they were larger than about 8192 elements ("about" because the out of bound write 
      checking mechanism uses some elements when switched on). This restriction has 
      now been removed.

  11. 21/5/01 svd() could not cope with single column matrices. Fixed.
  12. 23/5/01 svd() re-structured so that svd on bi-diagonal matrix is now done in a separate
      service routine. This allows efficient svd of matrices with special structure, by allowing 
      different versions of svd() with special bi-diagonalization steps, which can then call 
      the new routine svd_bidiag(). 
  
  15. msort() added, to sort the rows of a matrix so that the first column is in 
      ascending order, entries in 2nd col are ascending if corresponding 1st col 
      entries are tied, etc.  
  19. 1/5/02: R default compile options allow dangerous use of register variables
      which can result in:
      double a,b;a=b;if(a==b) 
      evaluating as false. This means that Watkins suggested convergence check
      if (big+small==big) can fail. Hence replaced in svd and eigen routines.
      All routines now default to using DOUBLE_EPS in place of any hard coded 
      1e-14's! Also Watkins construction changed to: if (small<=DOUBLE_EPS*big).
      header called general.h is good place to define DOUBLE_EPS - will be defined 
      in R.h, which can be included there.   
  20. 5/5/02 One convergence test in svd had been left unchanged - fixed this and loosened
      convergence criteria by 2 bits.
 
*/







