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
  A.M=(double **)calloc((size_t)(rows+2*pad),sizeof(double *));
  if ((cols==1L)||(rows==1L))
  { if (A.M)
    A.M[0]=(double *)calloc((size_t)(cols*rows+2*pad),sizeof(double));
    for (i=1L;i<rows+2*pad;i++)
    A.M[i]=A.M[0]+i*cols;A.vec=1;
  } else
  { if (A.M)
    for (i=0L;i<rows+2*pad;i++)
    A.M[i]=(double *)calloc((size_t)(cols+2*pad),sizeof(double));
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
    { top=bottom=(MREC *)calloc(1,sizeof(MREC));
      bottom->mat=top->mat=A;top->bp=bottom;bottom->fp=top;
    } else  /* expanding the linked list by one */
    { top->fp=(MREC *)calloc(1,sizeof(MREC));
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
	     free(delet);
      }
      /* repositioning pointers so that what was allocated gets freed */
      if (!A.vec) for (i=0;i<pad;i++) A.M--;
      for (i=0;i<A.original_r+2*pad;i++)
      for (j=0;j<pad;j++) A.M[i]--;
    }
    if (A.vec) free(A.M[0]); else
    for (i=0;i<A.original_r+2*pad;i++) if (A.M[i]) free(A.M[i]);
    if (A.M) free(A.M);
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

long fsafewrite(ptr,size,n,stream)
     double *ptr;size_t size;long n;FILE *stream;

{ long i,j,k=0L;
  for (i=0;i<(n/32000L);i++)
  k+=fwrite(ptr+i*32000L,size,(size_t)32000L,stream);
  j=n%32000L;
  k+=fwrite(ptr+i*32000L,size,(size_t)j,stream);
  return(k);
}

long fsaferead(ptr,size,n,stream)
     double *ptr;size_t size;long n;FILE *stream;

{ long i,j=32000L,k=0L;
  for (i=0;i<(n/32000L);i++)
  k+=fread(ptr+i*32000L,size,(size_t)j,stream);
  j=n%32000L;
  k+=fread(ptr+i*32000L,size,(size_t)j,stream);
  return(k);
}


void dumpmat(M,filename) matrix M;char *filename;

{ FILE *out;
  long i,j=0L;
  out=fopen(filename,"wb");
  j+=fwrite(&(M.r),sizeof(long),1,out);j+=fwrite(&(M.c),sizeof(long),1,out);
  for (i=0;i<M.r;i++) 
  fwrite(M.M[i],sizeof(double),(size_t)M.c,out);
  fclose(out);
}

void readmat(M,filename) matrix *M;char *filename;

{ FILE *in;long i,j,k;char str[200];
  in=fopen(filename,"rb");
  if (in==NULL)
  { sprintf(str,_("\n%s not found, nothing read!"),filename);
    ErrorMessage(str,1);}
    k=fread(&i,sizeof(long),1,in);
    k=fread(&j,sizeof(long),1,in);
  (*M)=initmat(i,j);
  for (k=0L;k<M->r;k++)
  { i=fread((*M).M[k],sizeof(double),(size_t)M->c,in);
  }
  fclose(in);
}


matrix vecmult(A,x,t) matrix A,x;int t;

/* Optimised multiplication of x by A. where x is a column vector and A a matrix
   Result returns Ax and FREES x BEFORE RETURNING
   - so usual use is x=vecmult(A,x,t)
*/
{ matrix y;
  double *yV,*xV,**AM,*a;
  long Ar,Ac,i,j;
  if (t) y.r=A.c; else y.r=A.r;
  y=initmat(y.r,1L);yV=y.V;AM=A.M;Ar=A.r;Ac=A.c;
  if (t)
  { for (i=0;i<Ac;i++)
    { xV=x.V;
      for (j=0;j<Ar;j++) { *yV += AM[j][i]* *xV;xV++;}
      yV++;
    }
  }
  else
  { for (i=0;i<Ar;i++)
    { xV=x.V;
      for (a=AM[i];a<AM[i]+Ac;a++) { *yV += *a * *xV;xV++;}
      yV++;
    }
  }
  freemat(x);
  return(y);
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
  t=(int *)calloc(n,sizeof(int));
  M=(matrix *)calloc(n,sizeof(matrix)); /* previously no cast for sun C ??! */
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
  free(t);
  free(M);
  va_end(argptr);
}

void mad(C,A,B,mA,mB)
matrix C,A,B;double mA,mB;

/* forms sum C=A+B. address optimization added 27/10/94 */

{ long i;
  double *pA,*pB,*pC;
  if (C.vec)
  { pB=B.V;pA=A.V;
    for (pC=C.V;pC<C.V+C.r*C.c;pC++)
    *pC= (*pA++)*mA+(*pB++)*mB;
  } else
  for (i=0;i<A.r;i++)
  { pA=A.M[i];pB=B.M[i];
    for (pC=C.M[i];pC<(C.M[i]+A.c);pC++)
    *pC= (*pA++)*mA+(*pB++)*mB;
  }
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
  c=(int *)calloc((size_t)A->c,sizeof(int)); /* index of columns, used for column pivoting */
  d=(int *)calloc((size_t)A->c,sizeof(int));
  rp=(int *)calloc((size_t)A->c,sizeof(int)); /* row changes */
  cp=(int *)calloc((size_t)A->c,sizeof(int)); /* row changes */
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
   
  free(c);free(rp);free(cp);free(d);
}




double triTrInvLL(matrix *l0,matrix *l1)

/* this routine finds the trace of inv(LL') where L is a bi-diagonal lower
   triangular choleski factor, with leading diagonal l0 and leading sub diagonal
   l1. Returns -1 on failure (i.e. none +ve definite matrix). Note that routine 
   is *not* set up as a means of detecting numerical loss of +ve definiteness - 
   the checks in here are *only* to avoid actual divide by zeroes.

*/

{ double trC,x,z,*l1V,*l0V;
  long i,l0r;
  l0V=l0->V;l1V=l1->V;l0r=l0->r;
  x=l0V[l0r-1];x*=x;
  if (x==0.0) return(-1.0);
  z=1.0/x;trC=z;
  for (i=l0r-2;i>=0;i--)
  { x=l1V[i];z=(1.0+x*x*z);x=l0V[i];
    x*=x;
    /* if (z==0.0) z=0.0;  could be removed - condition impossible 
       else*/
    { if (x==0.0) return(-1.0); 
      z/=x;
    }
    trC+=z;
  }
  return(trC);
}

void bicholeskisolve(matrix *A,matrix *B,matrix *l0,matrix *l1)

/* solves LL'A=B where L is a matrix with leading diagonal l0 and leading
   sub-diagonal l1. The algorithm is O(n^2) and hence much better than
   performing the same calculation by inversion of L (since inverse of L is
   fully lower triangular, rather than banded) */

{ long i,j;
  double *pA1,*pA,*pB,lv0,lv1;
  /* solve LC=B (C actually stored in A) */
  pA=A->M[0];pB=B->M[0];lv0=l0->V[0];
  for (i=0;i<A->c;i++) pA[i]=pB[i]/lv0;
  for (i=1;i<A->r;i++)
  { lv0=l0->V[i];lv1=l1->V[i-1];pA1=pA;pA=A->M[i];pB=B->M[i];
    for (j=0;j<A->c;j++)
    pA[j]=(pB[j]-lv1*pA1[j])/lv0;
  }

  /* now solve L'A=C */
  pA=A->M[A->r-1];
  lv0=l0->V[l0->r-1];
  for (i=0;i<A->c;i++) pA[i]/=lv0;
  for (i=A->r-2;i>=0;i--)
  { pA1=pA;pA=A->M[i];
    lv0=l0->V[i];lv1=l1->V[i];
    for (j=0;j<A->c;j++)
    pA[j]=(pA[j]-lv1*pA1[j])/lv0;
  }
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

void choleski(A,L,invert,invout) matrix A,L;int invert,invout;
/* version that halts execution on failure */
{ if (!chol(A,L,invert,invout))
  ErrorMessage(_("Not a +ve def. matrix in choleski()."),1);
}

int chol(A,L,invert,invout) matrix A,L;int invert,invout;

/* Routine to perform inversion and Choleski decomposition A=LL'  where L is */
/* LOWER triangular (zero elements are to the top right).Inversion is       */
/* performed only if invert is not zero. If invout is non-zero then A will  */
/* be returned containing the inverse of A passed to the routine, otherwise */
/* L will contain the inverse of choleski decomposition (so that inverse of */
/* A is L'L) but A will be unchanged. If invert is zero invout is ignored.  */
/* returns 1 for a +ve def. matrix: 0 otherwise....                         */ 

{ long i,j,k;
  double sigmaL,*p,*p1,temp,**AM,**LM,**invM,z;
  matrix inv;
  AM=A.M;LM=L.M; /* A.M replaced by AM and L.M replaced by LM to save address calc. */
  for (i=0;i<L.r;i++) for (p=LM[i];p<(LM[i]+L.c);p++) *p=0.0;
  if (AM[0][0]<=0)
  return(0);
  LM[0][0]=sqrt(AM[0][0]);
  for (j=1;j<A.r;j++) LM[j][0]=AM[j][0]/LM[0][0];
  for (i=1;i<(A.r-1);i++)
  { sigmaL=0.0;
    for (p=LM[i];p<(LM[i]+i);p++) sigmaL+=(*p)*(*p);
    z=AM[i][i]-sigmaL;
    if (z<=0.0)   /* test should be register safe */
    return(0);
    else LM[i][i]=sqrt(z);
    for (j=i+1;j<A.r;j++)
    { sigmaL=0.0;p1=LM[i];
      for (p=LM[j];p<(LM[j]+i);p++) sigmaL+=(*p)*(*p1++);
      LM[j][i]=(AM[j][i]-sigmaL)/LM[i][i];
    }
  }
  sigmaL=0.0;j=A.r-1;
  for (p=LM[j];p<(LM[j]+j);p++)
  { sigmaL+=(*p)*(*p);
    j=j;
  }
  z=AM[j][j]-sigmaL;
  if (z<=0) /* test ok even with register opt. */
  return(0);
  LM[j][j]=sqrt(z);
  /* Inverting L (it stays lower triangular)*/
  if (invert)
  { inv=initmat(A.r,A.c);invM=inv.M;
    for (i=0;i<A.r;i++) invM[i][i]=1.0;
    for (i=0;i<A.r;i++)
    { temp=LM[i][i];
      for (p=invM[i];p<=(invM[i]+i);p++) (*p)/=temp;
      for (j=i+1;j<A.r;j++)    /* original code had rows in inner loop */
      { temp=LM[j][i];p1=invM[j];
	     for (p=invM[i];p<=(invM[i]+i);p++)
	     (*p1++) -= (*p)*temp;
      }
    }
    for (i=0;i<inv.r;i++)
    { p1=invM[i];
      for (p=LM[i];p<=(LM[i]+i);p++) *p = *p1++;
    }
    /* Getting the Inverse of A */
    if (invout)
    { for (i=0;i<A.r;i++) for (p=AM[i];p<(AM[i]+A.r);p++) *p = 0.0;
      for (i=0;i<A.r;i++) for (k=i;k<A.r;k++)
      { temp=invM[k][i];p1=AM[i];
        for (p=invM[k];p<(invM[k]+A.r);p++) (*p1++) += temp * (*p);
      }
    }
    freemat(inv);
  }
  return(1);
}





matrix choleskiupdate(L,a) matrix L,a;

/* produces the modified choleski decomposition of the matrix A' constructed
   by adding vector a as the last row and column of matrix A                */


{ long i,Lr;
  double rr,*p,*p1,**LM,**RM,*aV;
  matrix R;
  Lr=L.r;
  R=initmat(Lr+1,Lr+1);
  LM=L.M;RM=R.M;aV=a.V;
  for (i=0;i<Lr;i++)
  { p1=LM[i];
    for (p=RM[i];p<=(RM[i]+i);p++) *p = *p1++;
  }
  for (i=0;i<R.c;i++)
  { rr=0.0;p1=RM[Lr];
    for (p=RM[i];p<(RM[i]+i);p++) rr+=(*p)*(*p1++);
    if (i!=Lr) RM[Lr][i]=(aV[i]-rr)/RM[i][i];
    else
    { if (aV[i]-rr<0.0) RM[Lr][i]=DOUBLE_EPS; /* ERROR condition! */
      else RM[Lr][i]=sqrt(aV[i]-rr);
    }
  }
  freemat(L);
  return(R);
}

void choleskir1ud(L,u,alpha) matrix L,u;double alpha;

/* Finds the choleski factors of LL'+ alpha*uu' where u is a (column) vector,
   using algorithm from Gill, Gulub, Murray and Saunders (1974). This operation
   is O(n^2), and mildly innefficient since the given algorithm assumes that
   the choleski decomposition is LDL' where L is unit lower triangular (1s on
   leading diagonal) and D is diagonal) */

{ double *v,*dV,*pV,**LM,t0,bj,qj,sj,rj2,si,tj;
  int i,j;
  matrix p,d;

  /* convert to LDL' form where L are unit lower triangular */
  d=initmat(u.r,1L);
  dV=d.V;LM=L.M;
  for (i=0;i<u.r;i++)
  { dV[i]=LM[i][i];
    for (j=i;j<u.r;j++)
    { LM[j][i]/=dV[i];
    }
    dV[i]*=dV[i];
  }
  /* solve Lp=u */
  p=initmat(u.r,1L);pV=p.V;
  for (i=0;i<p.r;i++)
  { t0=0.0;
    for (j=0;j<i;j++) t0+=LM[i][j]*pV[j];
    pV[i]=(u.V[i]-t0)/LM[i][i];
  }
  /* Householder based stable method */
  v=u.V;
  sj=0.0;
  for (i=0;i<p.r;i++) sj+=pV[i]*pV[i]/dV[i];
  if (alpha*sj>-1.0)
  si=alpha/(1.0+sqrt(1.0+alpha*sj));
  else
  si=alpha;
  for (j=0;j<u.r;j++)
  { qj=pV[j]*pV[j]/dV[j];
    tj=1.0+si*qj;
    sj -= qj;
    rj2=tj*tj + si*si*sj*qj;
    d.V[j] *=rj2;
    bj=alpha*pV[j]/dV[j];
    alpha/=rj2;
    if (rj2>0.0) rj2=sqrt(rj2);
    else
    rj2=2e-15;
    si*=(1.0+rj2)/(rj2*(tj+rj2));
    for (i=j+1;i<u.r;i++)
    { v[i] -= pV[j]*LM[i][j];
      LM[i][j] += bj*v[i];
    }
  }

  /* choleski based (not so stable) and assumes alpha=1 */

  for (i=0;i<u.r;i++)
  { if (d.V[i]<=0.0)
    d.V[i]=DOUBLE_EPS;
    else d.V[i]=sqrt(d.V[i]);
    for (j=i;j<u.r;j++)
    { L.M[j][i]*=d.V[i];
    }
  }
  freemat(d);freemat(p);
}

void choleskisolve(L,z,y) matrix L,z,y;

/*                                                                         */
/* Solves the system LL'z=y for z. L is a lower triangular choleski factor */
/*				                             		   */

{ long i,j,n;
  matrix x;
  double xsum,zsum,*p,**LM,*zV,*xV,*yV;
  x=initmat(y.r,1L);
  n=y.r;LM=L.M;zV=z.V;xV=x.V;yV=y.V;
  for (i=0;i<n;i++)
  { xsum=0.0;p=LM[i];
    for (j=0;j<i;j++) xsum+=(*p++)*xV[j];
    xV[i]=(yV[i]-xsum)/LM[i][i];
  }
  for (i=(n-1);i>=0;i--)
  { zsum=0.0;
    for (j=(i+1);j<n;j++) zsum+=LM[j][i]*zV[j];
    zV[i]=(xV[i]-zsum)/LM[i][i];
  }
  freemat(x);
}



double variance(a) matrix a;

{ long i,ar;
  double y2=0.0,y=0.0,*aV;
  ar=a.r;aV=a.V;
  for (i=0;i<ar;i++) { y2+=aV[i]*aV[i];y+=aV[i];}
  y=y2/ar-y*y/(ar*ar);
  if (y<0.0)
  { y=0.0;}    /* can get here by rounding ! */
  return(y);
}

double cov(a,b) matrix a,b;

{ long i;
  double y2=0.0,y=0.0,y1=0.0;
  if (a.r*a.c!=b.r*b.c)
  { ErrorMessage(_("Error in Covariance(a,b) - a,b not same length."),1);}
  for (i=0;i<a.r;i++) { y2+=a.V[i]*b.V[i];y+=a.V[i];y1+=b.V[i];}
  return(y2/a.r-y*y1/(a.r*a.r));
}

double corr(a,b) matrix a,b;

{ return(cov(a,b)/(sqrt(variance(a)*variance(b))));}

double acf(matrix s,int lag)

/* calculates autocorrelation with prescribed lag */

{ double r;
  matrix t;
  if (lag==0) return(1.0);
  s.r-=lag;t=s;t.V+=lag;
  if (s.r>2) r=cov(s,t); else r=0.0;
  return(r);
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

double trace(matrix *A)

{ double **p,tr=0.0;long i;
  p=A->M;
  for (i=0;i<A->r;i++) tr+=p[i][i];
  return(tr);
}

double absdev(a) matrix a;

/* gets mean absolute deviation from mean */

{ int i;
  double m,d=0.0,*aV;
  aV=a.V;
  m=mean(a);
  for (i=0;i<a.r*a.c;i++) d+=fabs(aV[i]-m);
  d/=(a.r*a.c);
  return(d);
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


double matrixnorm(M) matrix M;

/* This is not the Euclidean norm of a matrix, it is the Frobenius norm
e.g. Watkins D.S. 1991 Fundamentals of matrix computation ,Wiley New York
p. 90.*/

{ double res=0.0;
  long i;
  for (i=0;i<M.r*M.c;i++)
  res+=(M.M[i/M.c][i%M.c]*M.M[i/M.c][i%M.c]);
  res=sqrt(res);
  return(res);
}

double m1norm(M) matrix M;

/* returns ||M||_1
*/
{ double maxsum=0.0,sum;
  long i,j;
  for (j=0;j<M.c;j++)
  { sum=0.0;
    for (i=0;i<M.r;i++) sum+=fabs(M.M[i][j]);
    if (sum>maxsum) maxsum=sum;
  }
  return(maxsum);
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

void InvertTriangular(matrix *R)

/* inverts a triangular matrix - returns result in R */

{ long i,j,k,n;
  double Lii,s;
  n=R->r;
  for (i=n-1;i>=0;i--)
  { Lii=1.0/R->M[i][i];
    for (j=n-1;j>i;j--)
    { s=0.0;
      for (k=i+1;k<=j;k++) s+=R->M[i][k]*R->M[k][j];
      R->M[i][j] = -s/R->M[i][i];
    }
    R->M[i][i]=Lii;
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
  u=(double *)calloc((size_t)Rr,sizeof(double));
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
    if (t==0.0) {free(u);return(0);} /* singular matrix */
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
  free(u);
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


void svdcheck(matrix *U,matrix *w,matrix *ws,matrix *wl,matrix *V)

/* this is a debugging routine for checking that svd() has not messed up the decomposition,
   by checking that the bidiagonal decomposition is still ok. */
{ matrix W,A;
  int i;
  W=initmat(w->r,w->r);
  for (i=0;i<w->r-1;i++) 
  { W.M[i][i]=w->V[i];
    W.M[i][i+1]=ws->V[i];
    W.M[i+1][i]=wl->V[i];
  }
  W.M[i][i]=w->V[i];
  A=initmat(U->r,U->c);
  multi(3,A,*U,W,*V,0,0,1);
  printmat(W," %7.3g");
  printmat(A," %7.3g");
  freemat(A);freemat(W);
  getc(stdin);
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







void symproduct(A,B,C,trace,chol) matrix A,B,C;int trace,chol;

/* This routine aims to find the product C=ABA' as fast as possible, when
   B is symmetric.
   If 'trace' is non-zero then only the elements on the trace of C are
   returned.
   If 'chol' is non-zero then it is assumed that B contains L the
   choleski factor of B and is lower triangular (zeros at top right).
   If chol is 1 then it is assumed that the decomposition is LL' otherwise
   it is assumed to be L'L.  The routine choleski returns LL' for the decomp
   of the matrix or L'L for the decomp of the inverse.
*/

{ long i,j,k,Mr,Mc,Ac,Ar,Bc; 
  double temp,*p,*p1,*p2,**AM,**BM,**CM,**MM;
  matrix M;
  AM=A.M;BM=B.M;CM=C.M;Ac=A.c;Ar=A.r;Bc=B.c;
  if (chol)
  { M=initmat(Ar,B.c);
    MM=M.M;Mr=M.r;Mc=M.c;
    if (chol==1)
    for (i=0;i<Mr;i++)
    for (j=0;j<Mc;j++)
    { p2=MM[i]+j;p=AM[i]+j;
      for (k=j;k<Ac;k++) (*p2)+=(*p++)*BM[k][j];
    } else
    for (i=0;i<Mr;i++)
    for (j=0;j<Mc;j++)
    { p2=MM[i]+j;p1=AM[i];
      for (p=BM[j];p<=(BM[j]+j);p++) (*p2)+=(*p1++)*(*p);
    }
    if (trace)
    { for (i=0;i<Mr;i++)
      { CM[i][i]=0.0;p2=CM[i]+i;
	     for (p=MM[i];p<(MM[i]+Mc);p++) { (*p2)+= (*p)*(*p);}
      }
    } else
    { for (i=0;i<Mr;i++)
      { for (j=i;j<Mr;j++)
   	  { CM[i][j]=0.0;p2=CM[i]+j;p1=MM[j];
	       for (p=MM[i];p<(MM[i]+Mc);p++) (*p2) += (*p)*(*p1++);
	       CM[j][i]=(*p2);
	     }
      }
    }
    freemat(M);
  } else
  { if (trace)
    { for (i=0;i<C.c;i++)
      { CM[i][i]=0.0;
	     for (j=0;j<Bc;j++)
	     { temp=0.0;p1=AM[i]+j+1;
	       for (p=BM[j]+j+1;p<(BM[j]+Bc);p++)
	       temp += (*p)*(*p1++);
          CM[i][i]+=temp*AM[i][j];
	     }
	     CM[i][i]*=2.0;
	     p2=CM[i]+i;p=AM[i];
	     for (j=0;j<Bc;j++) { (*p2)+= (*p)*(*p)*BM[j][j];p++;}
      }
    } else
    { M=initmat(Ac,Ar);
      MM=M.M;Mc=M.c;
      matmult(M,B,A,0,1);
      for (i=0;i<Ar;i++)
      { for (j=i;j<Mc;j++)
	     { CM[i][j]=0.0;p2=CM[i]+j;p=AM[i];
	       for (k=0;k<Ac;k++) (*p2)+=(*p++)*MM[k][j];
	       CM[j][i]=(*p2);
	     }
      }
      freemat(M);
    }
  }
}

matrix getmask(index,smalln,bign) int *index,smalln,bign;

/* This is a routine for returning the matrix M such that
   a=Mp where a.M[i]=p.M[index[i]] */

{ int i;
  matrix M;
  double **MM;
  M=initmat((long)smalln,(long)bign);
  MM=M.M;
  for (i=0;i<smalln;i++) MM[i][index[i]]=1.0;
  return(M);
}



void gettextmatrix(M,name) matrix M;char *name;

/* reads a text file with M.r rows and M.c columns into matrix M */

{ FILE *f;
  long i,j;
  int k=1;
  char c,str[200];
  f=fopen(name,"rt");
  if (!f)
  { sprintf(str,_("%s not found by routine gettextmatrix().\n"),name);
    ErrorMessage(str,k);}
  for (i=0;i<M.r;i++)
  { for (j=0;j<M.c;j++)
    { k = fscanf(f,"%lf",M.M[i]+j);
    }
    c=' ';while ((c!='\n')&&(!feof(f))) c=(char)fgetc(f);
  }
  fclose(f);
}


void notinv(J,K,B) matrix J,K,B;

/* solves the problem of finding a K such that JK=B when J has more
   columns than rows and B is square with same number of rows as J. Note that
   J gets overwritten by this routine, during QT factorisation. The algorithm
   is as follows: Factorise J=[0,T]Q'; solve [0,T]Z=B for Z allowing first
   J.c-J.r cols of Z to be zero; then K = QZ. This algorithm makes maximum use
   of info in J. 14/1/96 */

{ matrix Q,Z;
  long i,j,k,Jc1,Zr1,Jr;
  double e,**JM,**ZM,**BM,**KM;
  Jr=J.r;
  Q=initmat(J.r,J.c);
  QT(Q,J,0);
  Z=initmat(J.c,Jr);
  JM=J.M;ZM=Z.M;BM=B.M;KM=K.M;
  Zr1=Z.r-1L;Jc1=J.c-1L;
  for (i=0;i<Jr;i++)
  for (j=0;j<Jr;j++)
  { e=0.0;
    for (k=0;k<i;k++) e+=JM[i][Jc1-k]*ZM[Zr1-k][j];
    ZM[Zr1-i][j]=(BM[i][j]-e)/JM[i][Jc1-i];
  }
  for (i=0;i<Z.r;i++) for (j=0;j<Z.c;j++) KM[i][j]=ZM[i][j];
  HQmult(K,Q,1,0);
  freemat(Z);
  freemat(Q);
}

void leastsq(A,p,y,w) matrix A,p,y,w;

/* dummy routine for fullLS */

{ matrix T,z;T.r=0L;z.r=0L;
  fullLS(A,p,y,w,T,z,0);
}


void fullLS(A,p,y,w,T,zo,Tzout) matrix A,p,y,w,T,zo;int Tzout;

/* Minimises ||w(Ap-y)|| for p using TQ orthogonal factorisation of A' -
   This is the way it should be done (i.e. not by forming inv(A'A)A'
   explicitly! ) 14/1/96 - Could be made a bit faster by using a QR
   factorisation in place of TQ - speeded up by sensible use
   of orthogonal factorisation, call QT with Qfull=0, and Q same dimension
   as At, then instead of premultiplying y by Q in matmult, use
   HQmult(z,Q,1,0) having set values in z to y.
   Method is ||Ap-y|| = ||y'-p'A'||, so use a QT factorisation of A'
   A'Q = [0,T] where Q is orthogonal. Then solve the soluble equations
   from y'Q = p'[0,T] to get p.
   NOTE: w vector with square roots of normal weights - i.e. 1 over s.d.s
   for no weights set w.r==0L;
   The matrices T (p.r by p.r) and the zo last p.r elements of Q'y are output
   if Tzout is non-zero.
*/

{ matrix At,Q,z;
  double e,**AM,**AtM,*pV,*zV;
  long i,j,Atr,Atc,pr,zr;
  At=initmat(A.c,A.r);
  Q=initmat(At.r,At.c);
  AtM=At.M;AM=A.M;Atr=At.r;Atc=At.c;pV=p.V;pr=p.r;
  if (!w.r)
  for (i=0;i<A.r;i++) for (j=0;j<A.c;j++) AtM[j][i]=AM[i][j];
  else
  for (i=0;i<A.r;i++) for (j=0;j<A.c;j++) AtM[j][i]=AM[i][j]*w.V[i];
  QT(Q,At,0);
  z=initmat(y.r,1L);zV=z.V;zr=z.r;
  if (w.r)
  for (i=0;i<zr;i++) zV[i]=y.V[i]*w.V[i]; /* new code for faster orthogonal transforms */
  else
  for (i=0;i<zr;i++) zV[i]=y.V[i];
  HQmult(z,Q,1,1);                 /* does orthogonal rotation the fast way */
  /* end of debug code */
  for (i=0;i<Atr;i++)
  { e=0.0;
    for (j=0;j<i;j++) e+=AtM[Atr-j-1][Atc-Atr+i]*pV[pr-j-1];
    pV[pr-1-i]=(zV[zr-Atr+i]-e)/AtM[Atr-i-1][Atc-Atr+i];
  }
  if (Tzout) /* outputs the factorized problem, to allow updates */
  { for (i=0;i<zo.r;i++) zo.V[i]=zV[zr-zo.r+i];
    for (i=0;i<T.r;i++) for (j=T.r-i-1;j<T.r;j++)
    T.M[i][j]=AtM[i][Atc-Atr+j];
  }
  freemat(At);
  freemat(Q);
  freemat(z);
}

void updateLS(T,z,x,y,w) matrix T,z,x;double y,w;

/* This routine takes the matrices T and z from the orthogonal factorisation
   used to solve the least squares problem in fullLS(). Given a single extra
   observation, y, with a corresponding weight, w, and design matrix row, x,
   T and z are updated efficiently using Givens rotations so that solution
   of Tp=z will now yield the solution of the ammended least squares problem.
   This routine has been tested in the weighted case.
   
   13/1/2000 Givens rotations are protected against under/overflow as follows...
   Assume xj to be zeroed into xi:
   m = max(fabs(xi),fabs(xj));
   xi/=m;xj/=m
   r=sqrt(xi*xi+xj*xj);
   c=xi/r;s=xj/r;
   xi=m*r;xj=0.0;
*/

{ double xi,ti,*xp,*tp,r,s,c,m;
  int i,j;
  matrix xx;
  xx=initmat(x.r,1L); /* needed because xx has to be over-written */
  for (i=0;i<x.r;i++) xx.V[i]=w*x.V[i];y*=w;
  for (i=0;i<T.r;i++) /* work through the columns of x rows of T */
  { xi=xx.V[i];ti=T.M[i][T.r-i-1];
    m=fabs(xi);r=fabs(ti);if (r>m) m=r; /* scale factor */
    if (m) { xi/=m;ti/=m;} /* avoid over/underflow */
    r=sqrt(xi*xi+ti*ti);
    if (r) { s=xi/r;c= -ti/r;} else {s=0.0;c=1.0;}
   /* T.M[i][T.r-i-1]=r;*/
    for (j=i;j<T.r;j++)
    { xp=xx.V+j;tp=T.M[j]+T.r-i-1;
      xi=c*(*xp)+s*(*tp);
      *tp=s*(*xp)-c*(*tp);
      *xp=xi;
    }
    xi=c*y+s*z.V[z.r-i-1];
    z.V[z.r-i-1]=s*y-c*z.V[z.r-i-1];
    y=xi;
  }
  freemat(xx);
}

void rtsolve(T,p,z) matrix T,p,z;

/* solves p'T=z' for p when T is reverse lower triangular (as from a QT
   factorization) */

{ int i,j;
  double e;
  for (i=p.r-1;i>=0;i--)
  { e=0.0;
    for (j=i+1;j<p.r;j++) e+= p.V[j]*T.M[j][T.c-i-1];
    p.V[i]=(z.V[z.r-i-1]-e)/T.M[i][T.c-i-1];
  }
}




long alias(X,aliased,tol) matrix X;long *aliased;double tol;

/* checks that all columns of X are independent and if not produces a list
   of those that are not. aliased must have the dimension of the X.r on
   entry. On exit its first n elements contain the columns to be removed
   to get a full rank matrix. The return value is the number of aliased
   columns. The algorithm checks each column in turn for independence of
   the preceding set of independent columns, by regressing the column on the
   previous independent set and checking that the rss is large enough.*/

{ matrix F,x,xp,p,w;
  double e,x2,y;
  long i,j,k,n;
  w=initmat(1L,1L); /* keep gcc -Wall happy */
  w.r=0L;
  F=initmat(X.r,X.c);x=initmat(X.r,1L);p=initmat(X.c,1L);
  xp=initmat(x.r,1L);
  j=1L;n=0L;
  for (k=0;k<X.r;k++) F.M[k][0]=X.M[k][0];F.c=1L;
  for (i=1;i<X.c;i++)
  { for (k=0;k<X.r;k++) x.V[k]=X.M[k][i];p.r=F.c;
    leastsq(F,p,x,w);
    matmult(xp,F,p,0,0);
    x2=0.0;e=0.0;
    for (k=0;k<x.r;k++)
    { y=x.V[k];x2+=y*y;
      y -= xp.V[k];e+=y*y;
    }
    if (e>tol*x2) /* column sufficiently independent */
    { for (k=0;k<X.r;k++) F.M[k][j]=x.V[k];
      F.c++;j++;
    } else
    { aliased[n]=i;n++;}
  }
  freemat(F);freemat(xp);freemat(x);freemat(p);
  w.r=1L;freemat(w);
  return(n);
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

void svdLS(A,p,y,tol) matrix A,p,y; double tol;

/* Minimises ||Ap-y|| for p, whilst minimising ||p|| if A not of full rank.
   Rank is estimated as the number of singular values greater than (tol
   multiplied by the largest singular value). Minimisation is performed
   by use of (truncated) S.V.D. */

{ matrix u,s,v;
  long i,j,Ar,Ac;
  double **uM,**AM,*sV,maxsv;
  AM=A.M;Ar=A.r;Ac=A.c;
  u=initmat(A.r,A.c);
  s=initmat(A.c,1L);
  v=initmat(A.c,A.c);
  sV=s.V;uM=u.M;
  for (i=0;i<Ar;i++) for (j=0;j<Ac;j++) uM[i][j]=AM[i][j];
  svd(&u,&s,&v);
  if (tol<0.0) tol=0.0;if (tol>1.0) tol=1.0;
  maxsv=0.0;for (i=0;i<s.r;i++) if (maxsv<sV[i]) maxsv=sV[i];
  for (i=0;i<s.r;i++)
  if (sV[i]>tol*maxsv)   /* estimate rank and do truncation */
  { sV[i]=1.0/sV[i];} else sV[i]=0.0;
  for (i=0;i<Ar;i++) for (j=0;j<Ac;j++) uM[i][j]*=sV[j];
  freemat(s);
  s=initmat(u.c,1L);
  matmult(s,u,y,1,0);
  matmult(p,v,s,0,0);
  freemat(u);freemat(s);freemat(v);
}

long rank(a) matrix a;

/* estimates rank of a */

{ double max;
  long i,j,r=0L;
  matrix b,c,d;
  b=initmat(a.r,a.c);
  for (i=0;i<a.r;i++) for (j=0;j<a.c;j++) b.M[i][j]=a.M[i][j];
  c=initmat(a.c,1L);d=initmat(a.c,a.c);
  svd(&b,&c,&d);
  max=c.V[0];
  for (i=1;i<c.r;i++)
  { if (fabs(c.V[i])>max) max=fabs(c.V[i]);}
  max*=DOUBLE_EPS;
  for (i=0;i<c.r;i++) if (fabs(c.V[i])>max) r++;
  freemat(b);freemat(c);freemat(d);
  return(r);
}




double condition(a) matrix a;

/* finds the condition number of a as the ratio of the smallest to largest
   singular values - returns -ve for infinity */

{ int i,j;
  double res,min,max;
  matrix b,c,d;
  b=initmat(a.r,a.c);
  for (i=0;i<a.r;i++) for (j=0;j<a.c;j++) b.M[i][j]=a.M[i][j];
  c=initmat(a.c,1L);d=initmat(a.c,a.c);svd(&b,&c,&d);
  /*for (i=0;i<c.r;i++) { printf("%g",c.V[i]); getc(stdin);} */ /* comment out usually */
  min=max=c.V[0];
  for (i=1;i<c.r;i++)
  { if (c.V[i]<min) min=c.V[i]; else if (c.V[i]>max) max=c.V[i];}
  if (!min) res=-1.0; else res=max/min;
  freemat(b);freemat(c);freemat(d);
  return(res);
}

void specd(U,W) matrix U,W;

/* obtains the spectral decomposition of a symmetric matrix U:
   it is returned as U diag(W) U'.
   The routine uses svd, and then checks the signs of the eigenvalues
   for correctness - not exactly the most efficient method!! */

{ matrix V;
  double dot,max,dum;
  long i,j,k;
  V=initmat(U.r,U.r);
  svd(&U,&W,&V);
  for (j=0;j<U.c;j++) /* work through eigenvectors */
  { dot=0.0;for (i=0;i<U.r;i++) dot+=U.M[i][j]*V.M[i][j]; /* have to do all elements to avoid round off near zero problems */
    if (dot<0.0) W.V[j]= -W.V[j];
  }
  for (i=0;i<W.r-1;i++) /* sorting into descending order */
  { k=i;max=W.V[k];for (j=i;j<W.r;j++) if (W.V[j]>=max) { max=W.V[j];k=j;}
    dum=W.V[i];W.V[i]=W.V[k];W.V[k]=dum;
    if (i!=k) for (j=0;j<W.r;j++) { dum=U.M[j][i];U.M[j][i]=U.M[j][k];U.M[j][k]=dum;}
  }
  freemat(V);
}

long pinv(matrix *A,double trunc)

/* forms pseudoinverse of A - returning it in A, return value is rank estimate
   for A. If trunc is >=1.0 then this is taken as the required rank of the
   pseudoinvese, otherwise its the tolerance used for truncation 
*/

{ matrix ws,B,w,C;
  long i,j,r=0L;
  double max;
  B=initmat(A->c,A->c);w=initmat(A->c,1L);
  svd(A,&w,&B); /* now original A is A diag(w) B' */
  /* form pseudoinverse B diag(1/w) A' - but truncating small w_i's */
  C=initmat(A->r,A->c);
  if (trunc<1.0)
  { max=0.0;for (i=0;i<w.r;i++) if (fabs(w.V[i])>max) max=fabs(w.V[i]);
    max*=trunc;
  }
  else /* the rank of the required pinverse has been supplied */
  { if (r-(long)floor(trunc)>0.5) r=(long)floor(trunc)+1;else r=(long)floor(trunc);
    ws=initmat(w.r,1L);
    for (i=0;i<ws.r;i++) ws.V[i]=fabs(w.V[i]);
    sort(ws);
    max=ws.V[ws.r-r];
    freemat(ws);
  }
  r=0L;
  for (j=0;j<A->c;j++)
  if (fabs(w.V[j])>=max)
  { for (i=0;i<A->r;i++)
    C.M[i][j]=A->M[i][j]/w.V[j];
    r++;
  }
  if (A->r!=A->c)
  { freemat(*A);
    *A=initmat(B.r,C.r);
  }
  matmult(*A,B,C,0,1);
  freemat(w);
  freemat(C);
  freemat(B);
  return(r);
}




void suminvert(A,B,U,W)
matrix A,B,U,W;

/* If A and B are symmetric matrices then this routine returns U,W, where
   U is a square matrix and W a vector of eigenvalues. The inverse of
   A+cB where c is a constant can now be obtained from:
   U Diag(1.0/(W.V[i]+c)) U'
   NOTE: B must be +ve definite.
*/

{ matrix L,M,MA,MAM;
  long i,j,k;
  double *p,temp,*p1,**MM,**LM;
/* first get choleski factor of B */
  L=initmat(B.r,B.r);
  choleski(B,L,0,0); /* L contains choleski factor */
/* Inverting L (it stays lower triangular)*/
  M=initmat(B.r,B.c);MM=M.M;LM=L.M;
  for (i=0;i<B.r;i++) MM[i][i]=1.0;
  for (i=0;i<B.r;i++)
  { temp=LM[i][i];
    for (p=MM[i];p<=(MM[i]+i);p++) (*p)/=temp;
    for (j=i+1;j<B.r;j++)    /* original code had rows in inner loop */
    { temp=LM[j][i];p1=MM[j];
      for (p=MM[i];p<=(MM[i]+i);p++)
      (*p1++) -= (*p)*temp;
    }
  }
  /* now if LL' = A and M=inv(L) so that M'M= inv (A) then:
   A+cB = A + cLL' = LM(A+cLL')M'L' = L(MAM'+cI)L'
   and inv(A+cB) = M'inv(MAM'+cI)M - the convoluted way of doing things ensures
   the symmetry of MAM' so that its spectral decomp. can be obtained by svd */
  /* Form MAM efficiently */
  MA=initmat(A.r,A.r);
  for (i=0;i<A.r;i++) for (j=0;j<A.r;j++)
  for (k=0;k<=i;k++) MA.M[i][j]+=M.M[i][k]*A.M[k][j];
  MAM=initmat(A.r,A.r);
  for (i=0;i<A.r;i++) for (j=0;j<=i;j++)
  { for (k=0;k<=j;k++) MAM.M[i][j]+=MA.M[i][k]*M.M[j][k];
    MAM.M[j][i]=MAM.M[i][j];
  }
  /* get its svd/ spectral decomp. the decomp of MAM will be returned as
     MAM Diag(W) MAM' */

  specd(MAM,W);
  /* form U = M' MAM  */

  for (i=0;i<U.r;i++) for (j=0;j<U.c;j++)
  { U.M[i][j]=0.0; for (k=i;k<U.r;k++) U.M[i][j]+=M.M[k][i]*MAM.M[k][j];}

  freemat(M);freemat(MAM);freemat(MA);freemat(L);
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


void lu_tri(double *d,double *g,double *u,int n)

/* Let T be an n by n tri-diagonal symmetric matrix, with leading diagonal 
   d and leading sub (=super) diagonal matrix g. This routine uses LU 
   decomposition to solve:
                           Tv=u 
   for v, and returns v in u. Note that d gets overwritten by this routine,
   while g remains unchanged.

   No restriction on T beyond full rank and symmetry.
*/

{ double *dg,*dd,*du,*dd1,*du1,mult;
  /* LU decomposition .... */
  dd=d;dd1=d+1;du=u;du1=u+1;
  for (dg=g;dg<g+n-1;dg++)
  { mult = *dg/ *dd;
    *dd1 -= *dg * mult;
    *du1 -= *du * mult;
    dd++;du++;dd1++;du1++;
   }
  /* Backsubstitution ..... */
  u[n-1]/=d[n-1];
  dd=d+n-2;du1=u+n-1;dg=g+n-2;
  for (du=u+n-2;du>=u;du--) 
  { *du = ( *du - *dg * *du1 )/ *dd;dd--;du1--;dg--;}
}

void eigen_tri(double *d,double *g,double **v,int n,int getvec)

/* Routine to find the eigenvalues of a symmetric tri-diagonal matrix, A, using the
   implicit QR algorithm with Wilkinson shifts. Eigen-vectors can be found by
   
   a) setting getvec==1 and initializing v to an n by n array (contents ignored)

   or,   
 
   b) Using inverse iteration.

   The latter is O(n^2), but less stable than the former which is O(n^3). 
 
   d contains the leading diagonal of the matrix. 
   g contains the leading sub(=super) diagonal
   v is storage for eigen vectors
   n is the length of d.
   getvec specifies whether or not to accumulate eigen-vectors

   on exit d contains the eigen values of the matrix A, in descending order, 
   and g is altered. 

   If getvec==1 then v[i] will contain the eigenvector corresponding to d[i] for i=0..n-1

   A assumed full rank at present.

   Note that eigenvalues are O(n^2), but eigenvectors O(n^3)

*/

{ int end,start=0,finished=0,os,oe,counter=0,iter_limit=100,ok,k,i,j;
  double big,small,d1,d2,g1,g2,sig,x,y,z,b,s,c,s2,c2,cs,r,*dg,*dg1,*dg2,*dd1,*dd2,v0,*vv1,*vv0;
  if (getvec) /* set eigenvector matrix to I */ 
  for (k=0;k<n;k++) { for (dd1=v[k];dd1<v[k]+n;dd1++) *dd1=0.0;v[k][k]=1.0;}  
  end=n-1;
  if (n==1) finished=1;
  while (!finished)
  { oe=end;os=start;
    /* locate the end.... */
    ok=1;
    while (ok)
    { big=fabs(d[end])+fabs(d[end-1]);
      /*small=g[end-1]+big;if (big==small) R default register optimizations mess this up*/
      if (fabs(g[end-1])<DOUBLE_EPS*big) end--; else ok=0;
      if (end==0) { finished=1;ok=0;}  /* matrix is diagonal */
    }
    if (!finished)
    { /* locate the start ..... */
      start=end-1;ok=0;
      if (start>0) ok=1;
      while (ok)
      { big=fabs(d[start])+fabs(d[start-1]);
        /*small=g[start-1]+big;if (big==small) R default use of fp registers messes this up */ 
        if (fabs(g[start-1])<DOUBLE_EPS*big) ok=0; else start--;
        if (start==0) ok=0;
      } 
      /* check whether iteration limit exceeded.... */
      if (os==start&&oe==end)
      { counter++; if (counter>iter_limit) ErrorMessage(_("eigen_tri() failed to converge"),1);
      } else counter=0;
      /* calculate the Wilkinson shift, sig ...... */
      d1=d[end-1];d2=d[end];g1=g[end-1];
      x=(d1+d2)/2;y=(d1-d2)/2;y*=y;z=sqrt(y+g1*g1);
      big=x+z;small=x-z;
      if (fabs(big-d2)<fabs(small-d2)) sig=big; else sig=small;
      /* now form the implicit QR step rotator V..... */
      x=d[start]-sig;y=g[start];
      z=sqrt(x*x+y*y);
      c=x/z;s=y/z;
      /* Apply implicit QR step rotator: VAV'.... */
      c2=c*c;s2=s*s;cs=c*s;
      g1=g[start];d1=d[start];d2=d[start+1];
      d[start]=c2*d1+2*cs*g1+s2*d2;
      d[start+1]=s2*d1+c2*d2-2*cs*g1;
      g[start]=g1*(c2-s2)+cs*(d2-d1);
      if (getvec) /* then apply rotator to v */
      { vv1=v[start+1];
        for (vv0=v[start];vv0<v[start]+n;vv0++)
	{ v0= *vv0;
	  *vv0 = c*v0+s* *vv1; *vv1 = c * *vv1-s*v0;
          vv1++;
        }
      }
      if (start+1<end) /* then a bulge will be created and must be chased... */
      { g2=g[start+1];
        g[start+1]*=c;
        b=g2*s;
        /* now need to chase bulge off end of matrix..... */
        dd1=d+start+1;dd2=dd1+1;dg=g+start;dg1=dg+1;dg2=dg1+1;
        for (k=start;k<end-1;k++)
        { x = *dg;/* x=g[k]; */
          r=sqrt(x*x+b*b);
          c=x/r;s=b/r;
          *dg = r;   /* g[k]=r; */
          d1 = *dd1; /* d1=d[k+1]; */
          d2 = *dd2; /* d2=d[k+2]; */
          g1 = *dg1; /*g1=g[k+1]; */
          c2=c*c;s2=s*s;cs=c*s;
          *dd1 = c2*d1+2*cs*g1+s2*d2; /*d[k+1] */
          *dd2 = s2*d1+c2*d2-2*cs*g1; /*d[k+2]  */
          *dg1 = g1*(c2-s2)+cs*(d2-d1); /*g[k+1] */
          if (k+2<end) /* then end of matrix not reached and bulge propagates */
          { g2 = *dg2; /*g2=g[k+2]; */
            b=g2*s;
            *dg2 *= c;           /*g[k+2]*=c;  */
          } 
          dd1++;dd2++;dg1++;dg2++;dg++;
          if (getvec) /* apply rotator to v */
	  {  vv1=v[k+2];
             for (vv0=v[k+1];vv0<v[k+1]+n;vv0++)
	     { v0= *vv0;
	       *vv0 = c*v0+s* *vv1; *vv1 = c * *vv1-s*v0;
               vv1++;
             }
	     /* for (i=0;i<n;i++)
	      { v0=v[k+1][i];v1=v[k+2][i];
	      v[k+1][i]=c*v0+s*v1;v[k+2][i]=c*v1-s*v0;
	      }*/
          }  
        }           
      } 
    }    
  }
  for (i=0;i<n-1;i++) /* sorting into descending order */
  { k=i;big=d[k];for (j=i;j<n;j++) if (d[j]>=big) { big=d[j];k=j;}
    x=d[i];d[i]=d[k];d[k]=x;
    if (i!=k&&getvec) 
    { vv1=v[k]; for (vv0=v[i];vv0<v[i]+n;vv0++) { x= *vv0;*vv0 = *vv1;*vv1=x;vv1++;}
    }
  }
}


int elcomp(const void *a,const void *b)

/* comparison function to get quicksort to sort into descending order */

{
  if (*(double *)a>*(double *)b) return(-1);
  if (*(double *)a<*(double *)b) return(1);
  return(0);
}

void eigenvv_tri(double *d,double *g,double **v, int n)

/* Routine to find the eigen values and eigen vectors of symmetric tri-diagonal
   n by n matrix A. d is the leading diagonal of A. g if the leading sub(=super)
   diagonal of A.
   
   The routine first uses implicit QR with Wilkinson shifts to find the eigenvalues 
   of A, by calling eigen_tri(). This is 0(n^2).

   Given the eigenvalues of A  the eigenvectors can be found most efficiently by
   inverse iteration - a process that is also O(n^2), given the tri-diagonal 
   structure of A.

   * The eigen-values are supplied in decreasing order in d.
   
   * The ith eigenvector of A is returned in v[i].

   * g is returned unchanged.

   - inner loops have been optimized.

*/

{ double *vo,*d1,*d2,*g1,vnorm,x,*dum,*dum1,*dum2,*dum4,*b,xx,bt;
  int i,k,ok,ok1,iter,iter_max=1000; 
  char msg[200];
  unsigned long jran=2,ia=106,ic=1283,im=6075;
  if (n==1) { v[0][0]=1;return;}
  d1=(double *)calloc((size_t)n,sizeof(double));
  d2=(double *)calloc((size_t)n,sizeof(double));
  vo=(double *)calloc((size_t)n,sizeof(double));
  g1=(double *)calloc((size_t)(n-1),sizeof(double));
  for (i=0;i<n;i++) d1[i]=d[i];  /* keep a copy of A */
  for (i=0;i<n-1;i++) g1[i]=g[i];
  eigen_tri(d,g1,&dum,n,0);   /* get eigenvalues (only) */
  
  free(g1);
  /* work through eigen-vectors */
  for (k=0;k<n;k++)
  { b=v[k];bt=0.0;
    for (i=0;i<n;i++)   /* somewhat randomized start vector!  */
    { jran=(jran*ia+ic) % im;   /* quick and dirty generator to avoid too regular a starting vector */
      xx=(double) jran / (double) im -0.5; 
      b[i]=xx;xx=-xx;bt+=b[i]*b[i];
    } 
    bt=sqrt(bt); 
    for (i=0;i<n;i++) b[i]/=bt;
    /*v[k][0]=1.0;dum1=v[k];for (dum=v[k]+1;dum<v[k]+n;dum++) { *dum = - *dum1;dum1++;}*/
    ok=1;iter=0;
    while (ok)
    { dum1=d2;dum2=d1;dum4=vo;x=d[k];
      for (dum=v[k];dum<v[k]+n;dum++) 
      { *dum1 = *dum2 - x; /* forming A - \lambda_k I d2[i]=d1[i]-d[k] */
        *dum4 = *dum;     /* vo[i]=v[k][i] */
        dum1++;dum2++;dum4++;
      } 
      lu_tri(d2,g,v[k],n);

      vnorm=0.0;for (dum=v[k];dum<v[k]+n;dum++) { x = *dum;vnorm+=x*x;}
      vnorm=sqrt(vnorm);
      for (dum=v[k];dum<v[k]+n;dum++) *dum/=vnorm;
      ok=0; /* test for convergence.... */
      dum1=vo;
      for (dum=v[k];dum<v[k]+n;dum++) 
      { x = *dum1 - *dum;dum1++;x=fabs(x);if (x>DOUBLE_EPS) { ok=1;break;}}
      ok1=0;dum1=vo;
      for (dum=v[k];dum<v[k]+n;dum++) 
      { x = *dum1 + *dum;dum1++;x=fabs(x);if (x>DOUBLE_EPS) { ok1=1;break;}}
      if (ok==0||ok1==0) ok=0; else ok=1;
      iter++;
      if (iter>iter_max) 
	  {sprintf(msg,_("eigenvv_tri() Eigen vector %d of %d failure. Error = %g > %g"),k,n,x,DOUBLE_EPS);
           ErrorMessage(msg,1); 
          
          }
    }
  }
  
  /* checking code ..... do the eigen value- vector pairs work? */
  /*for (i=0;i<n;i++) 
  { k=0;err=d1[k]*v[i][k]+g[k]*v[i][k+1]-d[i]*v[i][k];  
    errmax=fabs(err);
    for (k=1;k<n-1;k++) 
    { err=g[k-1]*v[i][k-1]+d1[k]*v[i][k]+g[k]*v[i][k+1]-d[i]*v[i][k];  
      err=fabs(err);if (err>errmax) errmax=err;
    }
    k=n-1;err=g[k-1]*v[i][k-1]+d1[k]*v[i][k]-d[i]*v[i][k];  
    err=fabs(err);if (err>errmax) errmax=err;
  }
  Rprintf("The maximum error in eigenvv_tri() is %g\n",errmax);*/
  free(vo);free(d1);free(d2);
  for (i=0;i<n;i++)
  { x=0.0;for (dum=v[i];dum<v[i]+n;dum++) x += *dum;
    if (x<0.0) for (dum=v[i];dum<v[i]+n;dum++) *dum = - *dum;
  } 
}

int lanczos_spd(matrix *A, matrix *V, matrix *va,int m,int lm)

/* Routine to find trunctated spectral decomposition of a symmetric matrix, A,  using Lanczos 
   iteration. This is most useful for finding some of the highest and lowest eigen-values 
   of large matrices. Basic idea is to iteratively construct a (k by k) tri-diagonal matrix 
   T_k, whose eigenvalues/vectors  will well approximate the eigen values/vectors of A.

   m is the number of eigenvalues/vectors required at high end. Let A be n by n.
   lm is number of eigenvalues/vectors that must converge at low end.
   if lm<0 then the m largest magnitude eigenvalue + corresponding eigenvectors
   are found.

   V must be initialised to be an n by (max(0,lm)+m) matrix and va an (m+max(0,lm)) vector. 

   On exit V diag(va) V' is a truncated approximation to A

   Algorithm:

   1. Set q[0] to b/||b|| where b is a random n-vector.
   2. Repeat the following steps for j=0,1,2..... until convergence
   3. Form z=Aq[i]
   4. Form a[j]=q[j]'z
   5. Form z=z-a[j]*q[j]-b[i-1]q[j-1] (last term only for j>0)
   6. Re-orthogonalize against previous q[j]'s
   7. Find b[j]=||z||
   8. q[j+1]=z/b[j]
   9. Tj is tri-diagonal j+1 by j+1 matrix with a as leading diagonal
      and b as leading sub=super diagonal. Find its spectral decomposition,
      with eigen values d and ith eigenvector v[i].
   10. Error on d[i] is magnitude of b[j]*v[i][j].
   
   Note that some further optimization of the eigen-value step should be possible.
   At present implicit QR with Wilkinson shifts is used for the eigen values and
   eigen vectors. This is order j^3. However, inverse iteration would only be O(j^2)
   and it might be worth using previous eigen-values as shifts, and only finding the 
   eigenvectors corresponding to the eigenvalues just below (above) the eigenvalues
   already converged. The eigenvalues step would still be order j^2 (although should 
   be faster than before), but the eigen vector step would drop to order j. 
   Experiences problems with inverse iteration, however => using current method.
*/

{ double **v,*d,*b,*g,*a,bt,**q,**AM,*zz,*AMi,*qj,*qj1,xx,yy,
         *z,*zd,*err,normTj,*dum,eps_stop=DOUBLE_EPS,max_err;
  int i,j,k,n,ok,l,kk,vlength=0,biggest=0,low_conv,high_conv,use_low,conv,f_check;
  unsigned long jran=1,ia=106,ic=1283,im=6075;
  
  if (lm<0) { biggest=1;lm=0;} /* get m largest magnitude eigen-values */
  f_check = (m + lm)/2; /* how often to get eigen_decomp */
  AM=A->M;
  v=AM; /* purely to avoid spurious compiler warning */
  n=(int)A->r;
  q=(double **)calloc((size_t)(n+1),sizeof(double *));
  /* initialize first q vector */
  q[0]=(double *)calloc((size_t)n,sizeof(double));
  b=q[0];bt=0.0;
  for (i=0;i<n;i++)   /* somewhat randomized start vector!  */
  { jran=(jran*ia+ic) % im;   /* quick and dirty generator to avoid too regular a starting vector */
    xx=(double) jran / (double) im -0.5; 
    b[i]=xx;xx=-xx;bt+=b[i]*b[i];
  } 
  bt=sqrt(bt); 
  for (i=0;i<n;i++) b[i]/=bt;
  /* initialise vectors a and b - a is leading diagonal of T, b is sub/super diagonal */
  a=(double *)calloc((size_t)n,sizeof(double));
  b=(double *)calloc((size_t)n,sizeof(double));
  g=(double *)calloc((size_t)n,sizeof(double));
  d=(double *)calloc((size_t)n,sizeof(double)); 
  z=(double *)calloc((size_t)n,sizeof(double));
  zd=(double *)calloc((size_t)n,sizeof(double));
  err=(double *)calloc((size_t)n,sizeof(double));
  for (i=0;i<n;i++) err[i]=1e300;
  /* The main loop. Will break out on convergence. */
  for (j=0;j<n;j++) 
  { /* form z=Aq[j]......  */
    qj=q[j];
    for (i=0;i<n;i++) 
    { z[i]=0.0;zz=z+i;dum=qj;
      for (AMi=AM[i];AMi<AM[i]+n;AMi++) { *zz += *AMi * *dum;dum++;}
    }
    /* Now form a[j].... */
    zz=a+j;*zz=0.0;for (i=0;i<n;i++) *zz += qj[i]*z[i];
    /* Update z..... */
    if (!j)
    { xx=*zz;
      for (i=0;i<n;i++) z[i] -= xx*qj[i];
    } else
    { xx=*zz;yy=b[j-1];qj1=q[j-1];
      for (i=0;i<n;i++) z[i] -= xx*qj[i]+yy*qj1[i];
      
      /* Now stabilize by full re-orthogonalization.... */
      
      for (i=0;i<n;i++) zd[i]=z[i];
      for (i=0;i<=j;i++) 
      { xx=0.0;zz=zd; /* actually z here works just as well!! */
        for (dum=q[i];dum<q[i]+n;dum++) { xx+= *dum * *zz;zz++;}
        zz=z;
        for (dum=q[i];dum<q[i]+n;dum++) { *zz -= xx * *dum;zz++;}
      } 
      for (i=0;i<n;i++) zd[i]=z[i];
      for (i=0;i<=j;i++) 
      { xx=0.0;zz=zd; /*zd */
        for (dum=q[i];dum<q[i]+n;dum++) { xx+= *dum * *zz;zz++;}
        zz=z;
        for (dum=q[i];dum<q[i]+n;dum++) { *zz -= xx * *dum;zz++;}
       
      }
    } 
    /* calculate b[j].... */
    zz=b+j;*zz = 0.0;for (i=0;i<n;i++) { xx=z[i];*zz+=xx*xx;}*zz=sqrt(*zz);
    if (b[j]==0.0&&j<n-1) ErrorMessage(_("Lanczos failed"),1); /* Actually this isn't really a failure => rank(A)<l+lm */
    /* get q[j+1]      */
    if (j<n-1)
    { q[j+1]=(double *)calloc((size_t)n,sizeof(double));
      zz=q[j+1];xx=b[j];for (i=0;i<n;i++) zz[i]=z[i]/xx; /* forming q[j+1] */
    }
    /* Now get the spectral decomposition of T_j.  */
    if (((j>=m+lm-1)&&(j%f_check==0))||(j==n-1))   /* no  point doing this too early or too often */
    { for (i=0;i<j+1;i++) d[i]=a[i]; /* copy leading diagonal of T_j */
      for (i=0;i<j;i++) g[i]=b[i]; /* copy sub/super diagonal of T_j */   
      /* set up storage for eigen vectors */
      if (vlength) /* free up first */
	{/* for (i=0;i<vlength;i++) free(v[i]);*/free(v[0]);free(v); 
      } 
      v=(double **)calloc((size_t)(j+1),sizeof(double *));
      vlength=j+1;
      v[0] = (double *)calloc((size_t)(j+1)*(j+1),sizeof(double));
      for (i=1;i<j+1;i++) v[i] = v[i-1] + j+1;
      /* for (i=0;i<j+1;i++) v[i]=(double *)calloc((size_t)(j+1),sizeof(double)); */ 
 
      /* obtain eigen values/vectors of T_j in O(j^3) flops */
      /*eigenvv_tri(d,b,v,j+1); this was O(j^2), but not stable enough */   
      /* eigen_tri(d,g,v,j+1,1);*/ 
       kk = j+1;
       mgcv_trisymeig(d,g,v[0],&kk,1,1);
      /* debug code to check eigenvv_tri() results */     
      /* v[i] is ith  eigenvector, d[i] corresponding eigenvalue */
      /* Evaluate ||Tj|| .... */
      normTj=fabs(d[0]);if (fabs(d[j])>normTj) normTj=fabs(d[j]);
      for (k=0;k<j+1;k++) /* calculate error in each d[i] */
      { err[k]=b[j]*v[k][j];
        err[k]=fabs(err[k]);
      }
      /* and check for termination ..... */
      if (j>=m+lm)
      { max_err=normTj*eps_stop;
        if (biggest) 
	{ low_conv=0;i=j; while (i>=0&&err[i]<max_err&&d[i]<0.0) { low_conv++;i--;}
	  high_conv=0;i=0; while (i<=j&&err[i]<max_err&&d[i]>=0.0) { i++;high_conv++;}
          conv=high_conv;use_low=0;
          for (i=0;i<low_conv;i++) /* test each of the negatives to see if it should be in the set of largest */
	  { if (-d[j-i]>=d[high_conv-1]) { conv++;use_low++;}
          } 
          if (conv>=m) /* then have enough - need to reset lm and m appropriately and break */
	  { while (conv>m) /* drop smallest magnitude terms */ 
	    { if (use_low==0) {high_conv--;conv--;} else
	      if (high_conv==0) {use_low--;conv--;} else
              if (-d[j-use_low+1]>d[high_conv-1]) {high_conv--;conv--;} else { use_low--;conv--;}
            } 
            lm=use_low;m=high_conv;
            j++;break;
	  }
        } else
        { ok=1;
          for (i=0;i<m;i++) if (err[i]>max_err) ok=0;
          for (i=j;i>j-lm;i--) if (err[i]>max_err) ok=0;
          if (ok) 
          { j++;break;}
        }
      }
    }
  }
  /* At this stage, complete construction of the eigen vectors etc. */
  
  /* Do final polishing of Ritz vectors and load va and V..... */
  for (k=0;k<m;k++) /* create any necessary new Ritz vectors */
  { va->V[k]=d[k];
    for (i=0;i<n;i++) 
    { V->M[i][k]=0.0; for (l=0;l<j;l++) V->M[i][k]+=q[l][i]*v[k][l];}
  }
  for (k=m;k<lm+m;k++) /* create any necessary new Ritz vectors */
  { kk=j-(lm+m-k); /* index for d and v */
    va->V[k]=d[kk];
    for (i=0;i<n;i++) 
    { V->M[i][k]=0.0; for (l=0;l<j;l++) V->M[i][k]+=q[l][i]*v[kk][l];}
  }
 
  /* clean up..... */
  free(a);
  free(b);
  free(g);
  free(d);
  free(z);
  free(zd);
  free(err);
  if (vlength) /* free up first */
    { /*for (i=0;i<vlength;i++) free(v[i]);*/ free(v[0]);free(v);}
  for (i=0;i<n+1;i++) if (q[i]) free(q[i]);free(q);  
  return(j);
} /* end of lanczos_spd */





void printmat(matrix A,char *fmt)

{ int i,j;
  double n;
  n=matrixnorm(A);
  for (i=0;i<A.r;i++)
  { Rprintf("\n");
    for (j=0;j<A.c;j++) 
    if (fabs(A.M[i][j])>1e-14*n) Rprintf(fmt,A.M[i][j]);
    else Rprintf(fmt,0.0);
  }
  Rprintf("\n");
}

void fprintmat(matrix A,char *fname,char *fmt)

{ int i,j;
  double n;
  FILE *f;
  f=fopen(fname,"wt");
  n=matrixnorm(A);
  for (i=0;i<A.r;i++)
  { fprintf(f,"\n");
    for (j=0;j<A.c;j++) 
    if (fabs(A.M[i][j])>1e-14*n) fprintf(f,fmt,A.M[i][j]);
    else fprintf(f,fmt,0.0);
  }
  fclose(f);
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
   5. updateLS() has had its Givens rotations protected against under/overflow. 
      (not tested). 
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
  13. 5/01 lanczos_spd() added to get truncated spectral decomposition of a matrix
      by stabilized lanczos iteration.
  14. 5/01 lu_tri(), eigen_tri() and eigenvv_tri() added to get eigen-values and vectors
      of symmetric tri-diagonal matrices (in O(n^2)).
  15. msort() added, to sort the rows of a matrix so that the first column is in 
      ascending order, entries in 2nd col are ascending if corresponding 1st col 
      entries are tied, etc.  
  16. 28/6/01 lanczos_spd() simplified to always perform full re-orthogonalization - 
      The selective scheme taken from Demmel (1997) doesn't seem to work in practice. 
      The problem is that you get small error estimates before convergence on occasion, 
      and these seem to mess up the re-orthogonalisation process. The difficulty was 
      most apparent on when working with the E matrix for 1d smoothing splines. Full
      orthogonalization actually required fewer iterates to converge - and hence is 
      really not that bad. (Old code moved to mtest.c)
  17. 18/12/01: convergence tolerance tightened massively in lanczos_spd() otherwise there are
      serious problems in particular with repeated eigenvalues. iter_max increased
      in eigenvv_tri() otherwise can fail on repeated eigenvalues. 
  18. 20/12/01: eigenvv_tri() seems to be insufficiently stable for thin plate regression 
      splines on regular grids. The inverse iteration steps can fail. It might be possible 
      to use a hybrid algorithm that uses full accumulation if convergence too slow on 
      inverse iteration, but for the moment I have upgraded eigen_tri, so that it can 
      accumulate eigenvectors in a completely stable manner. Lanczos_spd() now uses the 
      stable version, but doesn't obtain the complete eigen-decomposition of T_k so
      early or so often, to ofset the increase in eigenvector cost from 0(j^2) to O(j^3).
      Also svdroot() now signals an error if it gets passed a matrix that has any -ve 
      eigenvalues. 6/1/02 - svdroot() error check had a bug - fixed.
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
  21. 6/9/02 lanczos_spd() now checks for convergence when (j>=m+lm), rather than when (j>m+lm)
      - the old code could drop the wrong eigenvalue/vector if m==n-1 (resulting e.g. in lousy 
      performance of tprs of basis dimension 1 less than the number of data!)
*/







