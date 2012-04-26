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

/* library of routines designed for unstructured GCV problems: */

#define ANSI   
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "matrix.h"
#include "gcv.h"
#include "general.h"
void ErrorMessage(char *msg, int fatal);




double TrInf(matrix *X,matrix *Z,matrix *w,matrix *S,double rho)

/* Finds the trace of the influence matrix:

   rho*XZ[Z'X'WXZ*rho + Z'SZ]^{-1}Z'X'W

   Z is the column basis for a null space of constraints.
   It is assumed that these are stored efficiently as householder
   transformations in the manner of routine QT. For example to impose
   Cp=0: QT(Z,C,0);Z.r=C.r;
   written and tested 26/11/97

*/
{ double *rw,zz;
  matrix R,Q,U,T,l0,l1;
  long n,i,j,k;
  n=X->r;
  /* Get Q'R=W^{0.5} X Z */
  rw=(double *)calloc((size_t)n,sizeof(double));
  for (i=0;i<n;i++) rw[i]=sqrt(w->V[i]);   /* rw contains l.d. of W^{0.5} */
  if (Z->r)
  { R=initmat(n,Z->c);
    mcopy (X,&R);HQmult(R,*Z,0,0);R.c-=Z->r;  /* R=XZ */
  } else
  { R=initmat(n,X->c);mcopy(X,&R);}          /* case when Z=I */
  for (i=0;i<n;i++) for (j=0;j<R.c;j++) R.M[i][j]*=rw[i];  /* R=W^{0.5} XZ */
  Q=initmat(n,n);
  QR(&Q,&R);  /* done in up to O(n^3) */
  /* invert R - it is this step that requires n>=nz*/
  freemat(Q);
  R.r=R.c; /* getting rid of zero part */
  InvertTriangular(&R);   /* R now contains L */
  T=initmat(S->r,S->c);
  mcopy(S,&T);
  if (Z->r)
  { HQmult(T,*Z,1,1);HQmult(T,*Z,0,0);
    T.r=T.c=Z->c-Z->r;
  }
  U=initmat(T.r,T.c);
  multi(3,U,R,T,R,1,0,0);
  for (j=T.c-1;j>=0;j--) for (i=0;i<T.r;i++)
  { zz=0.0; for (k=0;k<=j;k++) zz+=T.M[i][k]*R.M[k][j];T.M[i][j]=zz;}
  for (i=T.r-1;i>=0;i--) for (j=0;j<=i;j++)
  { zz=0.0; for (k=0;k<=i;k++) zz+=R.M[k][i]*T.M[k][j];T.M[i][j]=zz;}
  for (i=T.r-1;i>=0;i--) for (j=0;j<=T.c;j++) T.M[j][i]=T.M[i][j];
  zz=0.0;
  for (i=0;i<T.r;i++) for (j=0;j<T.c;j++) zz+=fabs(T.M[i][j]-U.M[i][j]);
  freemat(U);
  freemat(R);U=initmat(R.c,R.c);
  UTU(&T,&U);
  l0=initmat(T.r,1L);l1=initmat(T.r-1,1L);
  for (i=0;i<T.r;i++) T.M[i][i] += rho;  /* forming I*r + T */
  tricholeski(&T,&l0,&l1);                  /* get LL'=I*r + T */
  zz=triTrInvLL(&l0,&l1)*rho;
  freemat(l0);freemat(l1);freemat(U);freemat(T);
  free(rw);
  return(zz);
}

double EScv(double *ldt,matrix *T,matrix *l0,matrix *l1,matrix *x,double nx, matrix *z,double r,long n,
            double *trace,double *ress,double *sig2)

/* service routine for EasySmooth - obtains GCV score (*sig<=0.0) or UBRE score */

{ long i;
  int ubre=0;
  double rss=0.0,el,tr;
  if (*sig2>0.0) ubre=1;
  for (i=0;i<T->r;i++) { ldt[i]=T->M[i][i];T->M[i][i] += r;}  /* forming I*r + T */
  tricholeski(T,l0,l1);                  /* get LL'=I*r + T */
  tr=1.0-triTrInvLL(l0,l1)*r/n;

  z->r=x->r;
  bicholeskisolve(x,z,l0,l1);
  for (i=0;i<x->r;i++)
  { el=z->V[i]- r * x->V[i];rss+=el*el;T->M[i][i] = ldt[i];
  }
  rss+=nx;
  if (!ubre) *sig2=rss/(n*tr); /* variance estimate - GCV only */
  z->r=n;rss/=n;
  if (ubre) /* then use UBRE */
  { el = rss - 2*(*sig2)*tr+ *sig2;tr=tr*tr;}
  else /* use GCV */
  { tr=tr*tr;el=rss/tr;}
  *ress=rss;*trace=tr;
  /* debugging */
  return(el)/*(n-tr*n-5)*(n-tr*n-5))*/;
}

double EScheck(matrix *y,matrix Q,matrix *LZSZL,double *rw,double *trial,int m,double rho)

    /* debugging routine to check GCV score according to EScv, given trial eta and rho */

{ double sig2,xx,**MM,**M1M,rss,tr,nx,V,*ldt;
  int i,l,n,j;
  matrix T,U,z,l0,l1,x;
  n=y->r;
  T=initmat(LZSZL[0].r,LZSZL[0].c);
  ldt=(double *)calloc((size_t)T.r,sizeof(double));
  l0=initmat(T.r,1L);l1=initmat(T.r-1,1L);x=initmat(l0.r,1L);
  U=initmat(T.r,T.c);
  z=initmat(y->r,1L);
  xx=exp(trial[0]);MM=LZSZL[0].M;M1M=T.M;
  for (i=0;i<T.r;i++) for (j=0;j<T.c;j++)
  M1M[i][j]=xx*MM[i][j];
  for (l=1;l<m;l++) 
  { xx=exp(trial[l]);MM=LZSZL[l].M;
    for (i=0;i<T.r;i++) for (j=0;j<T.c;j++)
    M1M[i][j] += xx*MM[i][j];
  }
  
  UTU(&T,&U);    /* Form S=UTU' */
  
  z.r=n;
  for (i=0;i<n;i++) z.V[i]=rw[i]*y->V[i]; /* z=W^{1/2}y */
  /* matrixintegritycheck(); */
  OrthoMult(&Q,&z,0,Q.r,0,1,1);           /* z=QW^{1/2}y */
  z.r=T.r;                                /* z=[I,0]QW^{1/2}y */
  OrthoMult(&U,&z,1,T.r-2,1,1,0);         /* z=U'[I,0]QW^{1/2}y */
  z.r=n;                                  /* z->[z,x_1]' */
  nx=0.0;for (i=x.r;i<n;i++) nx+=z.V[i]*z.V[i]; /* ||x||^2 */
  sig2=-1.0; /* setting to signal GCV rather than ubre */
  V=EScv(ldt,&T,&l0,&l1,&x,nx,&z,rho,n,&tr,&rss,&sig2);
 
  
  freemat(l0);freemat(l1);freemat(x);freemat(T);freemat(U);freemat(z);
  free(ldt); 
 return(V);
}

void DumpEarg(matrix *T,matrix *z)
     /* Dump arguments to EasySmooth */
{ dumpmat(*T,"/home/snw/bugger.T");
  dumpmat(*z,"/home/snw/bugger.z");
}

void ReadEarg(matrix *T,matrix *z)

{ readmat(T,"/home/snw/bugger.T");
  readmat(z,"/home/snw/bugger.z");
}


double EasySmooth(matrix *T,matrix *z,double *v,double *df,long n,double *sig2,double tol,int mesh,double *edf,
                  double *score,double min_edf,double target_edf,double low_edf)

/* routine that minimises
   (||(I-r*(I*r + T)^{-1})z ||^2+||x||^2) / (n - r*Tr(I*r+T)^{-1})^2
   This is a gcv score for a problem with an influence matrix:
   r JZ[Z'J'WJZ r + Z'SZ]^{-1}Z'J'W
   This will have been re-written using the decomposition:
   W^{0.5}JZ=QR as....
   r W^{-0.5}QR(R'R r + Z'SZ)^{-1} R'Q'W^{0.5}
   which becomes....
   r W^{-0.5}(I*r+L'Z'SZL)^{-1}W^{0.5} where L=R^{-1}
   The decomposition UTU' = L'Z'SZL is then formed. As is z=UQ'W^{0.5} y.

   Alternatively the routine minimises an equivalent UBRE score if *sig2>0.0
   on entry.

   tol is golden section search tolerence 1e-6 usually ok
   mesh is the number of grid points to search on in the first phase of
        overall s.p. search.
   edf[] is an array for storing model edf's used in direct search
   score[] is an array for corresponding gcv/ubre scores.
   min_edf is the minimum possible edf for the model. Useful for setting limits
           on r, but ignored if <= 0.
   lower_edf is a lower limit to impose when grid searching for optimum r - this can be 
             higher than the absolute limit, min_edf. 

   If target_edf < 0 then it is ignored and the global minimizer is returned, 
   but if target_edf is >=0 then the local minimizer resulting in the edf  
   closest to target_edf is returned: this is usually the global minimizer, 
   but in multiple minima cases may not be. The facility is useful when the 
   least squares problem passed to MultiSmooth is an approximation, so that
   GCV minima taking the model parameters too far from their current values 
   may move the model too far from the true problem being approximated to
   be meaningful. This occurs, for example, with some binary data gams.
   Note that this local minimum searching is only done on the initial coarse
   grid - the algorithm does not attempt to locate each local minimum 
   exactly. Furthermore local minima are only considered if they meet some 
   depth threshold. 
  
   returns smoothing parameter and minimum gcv/ubre score.

   NOTE: there is a problem if r/rho is allowed to be too small, since T is 
   only positive *semi* definite. For this reason search proceeds from high 
   r/rho to low.   
*/

{ matrix l0,l1,x;
  double xx,r,V,V0,rm,tr,maxr,minr,minV,rss,r0,r1,rt,r1t,ft,f1t,tau,nx,*ldt,r4eps,reps,eps=DOUBLE_EPS,tr0,
         min_target_distance,dfo,db;
  double maxV=0.0,maxdV=0.0,firstdV=0.0,bracket_mult=20.0;
  int k,i,gcv=1,ok=0,mink,kb,kf,step;
  char msg[200];
  minV=0.0;
  reps=sqrt(eps); /* square root of machine precision */
  r4eps=sqrt(eps);
  if (*sig2>0.0) gcv=0;
  /* Initialise work space matrices */
  ldt=(double *)calloc((size_t)T->r,sizeof(double)); /* storage for leading diagonal of T*/
  l0=initmat(T->r,1L);l1=initmat(T->r-1,1L);
  x=initmat(l0.r,1L);
  nx=0.0;for (i=x.r;i<n;i++) nx+=z->V[i]*z->V[i]; /* ||x||^2 */
  /* get initial smoothing parameter estimates */
  r=0.0;for (i=0;i<T->r;i++) r+=T->M[i][i];r/=T->r;  /* first guess */
  /* search for upper r */
  maxr=r;tr=0.0;
  while (fabs(tr-T->r)>reps*T->r)  /* maxr too small to give max df */   
  { maxr*=bracket_mult;
    if (gcv) *sig2=-1.0;
    V=EScv(ldt,T,&l0,&l1,&x,nx,z,maxr,n,&tr,&rss,sig2);
    tr=(1-sqrt(tr))*n;
  } 
  /* search for lower r */
  /*minr=0.0;for (i=0;i<T->r;i++) if (minr<T->M[i][i]) minr=T->M[i][i];*/ 
  minr = r* eps;  /* provisional lower limit on r */
  ok=1;
  while(ok)          /* continue reducing r to search for lower search boundary */
  { r/=bracket_mult;tr0=tr;
    if (gcv) *sig2=-1.0;
    V=EScv(ldt,T,&l0,&l1,&x,nx,z,r,n,&tr,&rss,sig2);
    tr=(1-sqrt(tr))*n; 
    /* now test whether this is acceptable */
    if (tr<min_edf-r4eps||tr>tr0+eps) /* numerical instability has certainly set in - can go no lower - step back up */
    { r*=bracket_mult;ok=0;
    } else
    if (tr<min_edf+r4eps||((fabs(tr0-tr)<reps*T->r)&&(T->r-tr>=1-reps)&&(r<minr*1e3))) ok=0; /* reached boundary edf of trace stopped changing */
    if (r<minr) bracket_mult=2.0; /* get more cautious as reaching ill-conditioned region */
  }
  minr=r;
  rm=exp(log(maxr/minr)/mesh);
  r=maxr*rm;
  /* Rprintf("\nsp grid search.... \n");*/
  V0=0.0;ok=0;
  for (k=mesh;k>0;k--)
  { r/=rm;
    if (gcv) *sig2=-1.0;
    V=EScv(ldt,T,&l0,&l1,&x,nx,z,r,n,&tr,&rss,sig2); 
    score[k-1]=V;edf[k-1]=n*(1-sqrt(tr)); /* storing diagnostics */
    if ((V<minV&&edf[k-1]>=low_edf)||k==mesh) 
    { minV=V; minr=r; mink=k-1;if (k<mesh) ok=1; 
    }
    if (V>maxV||k==mesh) 
    { maxV=V;
    }
    if (k<mesh) /* need to store step sizes so that step sizes at boundary can be assessed if need be */
    { xx=fabs(V-V0);  /* store biggest V change to compare with change at upper boundary */
      if (k==mesh-1) firstdV=maxdV=xx;
      else if (xx>maxdV) maxdV=xx; 
    }
    V0=V;
  } /* minV and minr now contain the global minimum */
  
  if (!ok&&firstdV>reps*minV)  /* GCV score never decreased from first value and gradient not low enough at first value */
  { sprintf(msg,_("Overall smoothing parameter estimate on upper boundary.\nBoundary GCV score change: %g. Largest change: %g"),
                firstdV,maxdV);
    ErrorMessage(msg,0);
  }
 
  if (target_edf>=0) /* look for local minimum closer to target than the global minimum */
  { /* first search for the edf that is closest to the target */
    k=0;min_target_distance=fabs(target_edf-edf[k]);
    for (i=1;i<mesh;i++)
    { xx=fabs(target_edf-edf[i]);
      if (xx<min_target_distance)
      { k=i;
        min_target_distance=xx;
      }
    } /* edf[k] is the closest element of edf[] to target_edf - start search from here */  
    minV=score[k];mink=k;
    dfo=db=0.0;kb=kf=k;ok=1;
    while (ok) /* now search for a local minimum in the (edf) locality of target_edf */ 
    { step=1;
      while (step&&(kf<mesh-1)) /* step forward */    
      { kf++;      /* index of forward edge of search */
        dfo=edf[kf]-target_edf; /* forward distance from target */  
        if (dfo>db) step=0; /* forward distance from target larger than backwards distance, so stop */
        if (score[kf]<minV&&edf[kf]>=low_edf)
	{ minV=score[kf];mink=kf;
        } 
      }
      step=1;
      while (step&&(kb>0)) /* step back */
      { kb--;     /* index of back edge of search region */
        db=target_edf-edf[kb]; /* backward edge distance from target */
        if (db>dfo) step=0; /* backward distance exceeds forward distance, so stop and go forward instead */
        if (score[kb]<minV&&edf[kb]>=low_edf)
	{ minV=score[kb];mink=kb;
        } 
      } 
      /* now test for local minimum */
      if (mink==0||mink==mesh-1) ok=0; /* minimum is on global search boundary */
      if (mink>kb&&mink<kf) ok=0; /* minimum so far not on search boundary, so it is valid local minimum */
    }
    minr=maxr/pow(rm,mesh-1-mink);
  } 

  /* golden section search to polish minimisation */
  r0=minr/rm;r1=rm*minr;
  tau=2.0/(1.0+sqrt(5.0));
  rt=r0+(r1-r0)*tau;
  if (gcv) *sig2=-1.0;
  ft=EScv(ldt,T,&l0,&l1,&x,nx,z,rt,n,&tr,&rss,sig2);
  r1t=r0+(r1-r0)*(1.0-tau);
  if (gcv) *sig2=-1.0;
  f1t=EScv(ldt,T,&l0,&l1,&x,nx,z,r1t,n,&tr,&rss,sig2);
  while ((rt-r1t)>tol*fabs(rt+r1t))
  { if (ft<f1t)
    { r0=r1t;r1t=rt;f1t=ft;rt=r0+(r1-r0)*tau;
      if (gcv) *sig2=-1.0;
      ft=EScv(ldt,T,&l0,&l1,&x,nx,z,rt,n,&tr,&rss,sig2);
    } else
    { r1=rt;rt=r1t;ft=f1t;r1t=r0+(r1-r0)*(1.0-tau);
      if (gcv) *sig2=-1.0;
      f1t=EScv(ldt,T,&l0,&l1,&x,nx,z,r1t,n,&tr,&rss,sig2);
    }
  }
  minr=rt;
  minV=ft;
  
  *v = minV;
  *df=(1.0-sqrt(tr))*n;
  if (gcv) *sig2=-1.0;
  *v=EScv(ldt,T,&l0,&l1,&x,nx,z,minr,n,&tr,&rss,sig2); /* here for debugging purposes */
  
  freemat(l0);freemat(l1);freemat(x);free(ldt);
  return(minr);
}



/* tediouscv and boringHg are debugging routines for MultiSmooth() */

double tediouscv(matrix R,matrix Q,matrix *LZSZL,matrix *y,double *rw,
                 double *trial,double rho,int m,double *tr,double *rss,double sig2)


{ long i,j,l,n;
  matrix T,U,z,l0,l1,x;
  double v,nx,*ldt;
  n=y->r;
  T=initmat(LZSZL[0].r,LZSZL[0].r);
  U=initmat(T.r,T.r);
  z=initmat(n,1L);
  for (i=0;i<T.r;i++) for (j=0;j<T.c;j++)
  T.M[i][j]=exp(trial[0])*LZSZL[0].M[i][j];
  for (l=1;l<m;l++) for (i=0;i<T.r;i++) for (j=0;j<T.c;j++)
  T.M[i][j] += exp(trial[l])*LZSZL[l].M[i][j];
  UTU(&T,&U);    /* Form S=UTU' */
  z.r=n;
  for (i=0;i<n;i++) z.V[i]=rw[i]*y->V[i]; /* z=W^{1/2}y */
  OrthoMult(&Q,&z,0,Q.r,0,1,1);           /* z=QW^{1/2}y */
  nx=0.0;for (i=R.r;i<n;i++) nx+=z.V[i]*z.V[i];
  z.r=R.r;                                /* z=[I,0]QW^{1/2}y */
  OrthoMult(&U,&z,1,T.r-2,1,1,0);         /* z=U'[I,0]QW^{1/2}y */
  z.r=n;
  l0=initmat(T.r,1L);l1=initmat(T.r-1,1L);x=initmat(T.r,1L);
  ldt=(double *)calloc((size_t)T.r,sizeof(double));
  v=EScv(ldt,&T,&l0,&l1,&x,nx,&z,rho,n,tr,rss,&sig2);
  free(ldt);
  freemat(l0);freemat(l1);freemat(x);freemat(T);freemat(U);freemat(z);
  return(v);
}

void boringHg(matrix R,matrix Q,matrix *LZSZL,matrix *y,double *rw,
                 double *trial,double rho,int m,double sig2,double dt1)
/* Does f.d. estimation of gradient and Hessian dt is interval to use for
   differencing  */

{ double f,v,v1,v2,tr,rss,tr1,rss1,
    t1,t2,r1,r2;/*,t,r;*/
         
  int i,j,k;
  matrix a,M,p;
  Rprintf("\nHit Return ... ");getc(stdin);
  v=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&tr,&rss,sig2);/*t=tr;r=rss;*/
  Rprintf("\ntedious cv = %g\n",v);
  for (i=0;i<m;i++) /* the gradients of the gcv function */
  { trial[i]+=dt1;
    v1=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&tr1,&rss1,sig2);
    trial[i] -= dt1;
    tr1=(tr1-tr)/(dt1);
    rss1=(rss1-rss)/(dt1);
    Rprintf("\ng%d = %g drss=%g  dtr=%g",i,(v1-v)/dt1,rss1,tr1);
  }
  Rprintf("\n");
  for (i=0;i<m;i++) for (j=0;j<=i;j++)
  { if (i!=j)
    {
      M=initmat(6L,6L);a=initmat(6L,1L);p=initmat(6L,1L);
      trial[i]+=dt1/2;
      v1=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&t1,&r1,sig2);
      k=0;
      M.M[k][0]=1.0;M.M[k][1]=dt1/2;M.M[k][2]=0.0;M.M[k][3]=0.0;
      M.M[k][4]=dt1*dt1/4;M.M[k][5]=0.0;a.V[k]=v1;
      trial[i]-=dt1;
      v1=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&t1,&r1,sig2);
      k=1;
      M.M[k][0]=1.0;M.M[k][1]= -dt1/2;M.M[k][2]=0.0;M.M[k][3]=0.0;
      M.M[k][4]=dt1*dt1/4;M.M[k][5]=0.0;a.V[k]=v1;
      trial[i]-=dt1/2;trial[j]-=dt1;
      v1=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&t1,&r1,sig2);
      k=2;
      M.M[k][0]=1.0;M.M[k][1]= -dt1;M.M[k][2]= -dt1;M.M[k][3]= dt1*dt1;
      M.M[k][4]=dt1*dt1;M.M[k][5]=dt1*dt1;a.V[k]=v1;
      trial[j]+=2*dt1;
      v1=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&t1,&r1,sig2);
      k=3;
      M.M[k][0]=1.0;M.M[k][1]= -dt1;M.M[k][2]= dt1;M.M[k][3]= -dt1*dt1;
      M.M[k][4]=dt1*dt1;M.M[k][5]= dt1*dt1;a.V[k]=v1;
      trial[i]+=2*dt1;
      v1=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&t1,&r1,sig2);
      k=4;
      M.M[k][0]=1.0;M.M[k][1]=dt1;M.M[k][2]=dt1;M.M[k][3]=dt1*dt1;
      M.M[k][4]=dt1*dt1;M.M[k][5]=dt1*dt1;a.V[k]=v1;
      trial[j]-=2*dt1;
      v1=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&t1,&r1,sig2);
      k=5;
      M.M[k][0]=1.0;M.M[k][1]=dt1;M.M[k][2]=-dt1;M.M[k][3]=-dt1*dt1;
      M.M[k][4]=dt1*dt1;M.M[k][5]=dt1*dt1;a.V[k]=v1;
      trial[i]-=dt1;trial[j]+=dt1;
      svdLS(M,p,a,1e-10);
      Rprintf("%8.4g  ",p.V[3]);
      freemat(p);freemat(M);freemat(a);
    } else
    { trial[i] += dt1;
      v1=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&t1,&r1,sig2);
      trial[i] -= 2*dt1;
      v2=tediouscv(R,Q,LZSZL,y,rw,trial,rho,m,&t2,&r2,sig2);
      trial[i] +=dt1;
      f=(v1-2*v+v2);f/=dt1*dt1;
   
      Rprintf("%8.4g\n",f);
    }
  }
}



void MSgH(int ubre,int m,matrix *LZrS,matrix *ULZrS,matrix *ULZSZLU,matrix *U,matrix *T,matrix *l0,matrix *l1,matrix *A,matrix *H
	  ,matrix *d,matrix *c,matrix *Wy,matrix *Ay,matrix *dAy,matrix *dpAy,matrix *Q,matrix *g,matrix *Hess,double rho,double sig2
,double *trdA,double **trd2A,double *eta,double *da,double **d2a,double *db,double **d2b)
/* service routine for MultiSmooth - calculates gradient and Hessian of GCV score w.r.t. smoothing parameters */
{ int l,i,j,k,n;
  double **MM,**M1M,x,*pp,*pp1,trA,a,b; 
  /* form U'L'Z'S_i^{0.5} and U'L'Z'S_iZLU - This is the expensive part -
         <= O(n^3) worth further optimization of address calculation later.... */
  for (l=0;l<m;l++)
  { mcopy(LZrS+l,ULZrS+l);
    OrthoMult(U,ULZrS+l,1,T->r-2,1,1,0);
    MM=ULZrS[l].M;M1M=ULZSZLU[l].M;
    for (i=0;i<ULZrS[l].r;i++) for (j=0;j<=i;j++)
    { x=0.0;pp=MM[i];pp1=MM[j];
      for (k=0;k<ULZrS[l].c;k++) {x+= *pp * *pp1;pp++;pp1++;}
      M1M[i][j]=M1M[j][i]=x;
    }
  }
     
  /* now calculate the various traces needed in the calculation.
     These will be stored in trA, trdA[], trd2A[][]. These calculations
     are up to O(n^2)*/
  trA=triTrInvLL(l0,l1)*rho;
  for (l=0;l<m;l++)
  { A->r=ULZrS[l].r;A->c=ULZrS[l].c;
    bicholeskisolve(A,ULZrS+l,l0,l1);
    trdA[l]=0.0;
    for (i=0;i<A->r;i++) for (j=0;j<A->c;j++)
    trdA[l] -= A->M[i][j]*A->M[i][j];
    trdA[l]*=rho;
  }
    
  /* first form (Ir+T)^{-1}U'L'Z'S_iZLU(Ir+T)^{-0.5} for all i */
  for (l=0;l<m;l++)
  { A->r=ULZSZLU[l].r;A->c=ULZSZLU[l].c;
    bicholeskisolve(A,ULZSZLU+l,l0,l1);
    for (i=0;i<A->r;i++) H[l].M[i][0]=A->M[i][0]/l0->V[0];
    for (j=1;j<A->c;j++) for (i=0;i<A->r;i++)
    H[l].M[i][j]=(A->M[i][j]-H[l].M[i][j-1]*l1->V[j-1])/l0->V[j];
        /* H algorithm checked directly - ok */
  }
  for (l=0;l<m;l++) for (k=0;k<=l;k++)
  { x=0.0;
    for (i=0;i<H[l].r;i++) for (j=0;j<H[k].c;j++)
    x+=H[l].M[i][j]*H[k].M[i][j];
    trd2A[l][k]=trd2A[k][l]=2*x*rho;
    if (l==k) trd2A[l][k]+=trdA[l]/exp(eta[l]);
  }
  /* Now form b, db[], d2b[][] ...... */
  n=(int)Wy->r;
  b=1-trA/Wy->r;b=b*b;
  for (l=0;l<m;l++) db[l]= exp(eta[l])*2.0/n*(trA/n-1)*trdA[l];
  for (l=0;l<m;l++) for (k=0;k<=l;k++)
  d2b[k][l]=d2b[l][k]=
  exp(eta[l]+eta[k])*2.0/n*(trdA[l]*trdA[k]/n-(1-trA/n)*trd2A[l][k]);
  /* Need the derivatives of the rss term next. Use W^{0.5}y in place of y.
     Form and store vector Ay = Q'[I,0]'U(I*r+T)^{-1}U'[I,0]Qy */
  d->r=n;for (i=0;i<n;i++) d->V[i]=Wy->V[i];  /* W^{0.5}y */
  OrthoMult(Q,d,0,Q->r,0,1,1);
  Ay->r=d->r=T->r;
  OrthoMult(U,d,1,T->r-2,1,1,0);
  bicholeskisolve(Ay,d,l0,l1); /* Ay = (I*r+T)^{-1}U'[I,0]Qy */
  /* now for the dAy[] and dpAy[] terms, before finishing off Ay :
     dpAy[i] = (I*r+T)^{-1}U'L'Z'S_iZLU(I*r+T)^{-1}U'[I,0]Qy
     dAy[i] = -Q'[I,0]'U dpAy[i]
     the dpAy[] terms save time in calculating d^2A/dt_idt_j
  */
  for (l=0;l<m;l++)
  { A->r=T->r;A->c=1L;
    matmult(*A,ULZSZLU[l],*Ay,0,0);
    bicholeskisolve(dpAy+l,A,l0,l1);
    for (i=0;i<dpAy[l].r;i++) dAy[l].V[i]= -dpAy[l].V[i];
    for (i=dpAy[l].r;i<dAy[l].r;i++) dAy[l].V[i]=0.0;
    dAy[l].r= dpAy[l].r;
    OrthoMult(U,dAy+l,1,T->r-2,0,1,0);
    dAy[l].r=Wy->r;
    OrthoMult(Q,dAy+l,0,Q->r,1,1,1);
    for (i=0;i<dAy[l].r;i++) dAy[l].V[i]*=rho;
  }
  /* now finish Ay.... */
  OrthoMult(U,Ay,1,T->r-2,0,1,0);
  Ay->r=Wy->r;for (i=T->r;i<Wy->r;i++) Ay->V[i]=0.0;
  OrthoMult(Q,Ay,0,Q->r,1,1,1);
  for (i=0;i<Ay->r;i++) Ay->V[i]*=rho;
  /* form a, da[] & d2a[][]..... */
  for (l=0;l<m;l++) for (k=0;k<=l;k++)  /* starting with d2a[][]... */
  { matmult(*A,ULZSZLU[k],dpAy[l],0,0); /* forming (A=) U'L'Z'S_kZLU(I*r+T)^{-1}U'L'Z'S_lZLU(I*r+T)^{-1}U'[I,0]Qy */
    c->r=T->r;
    bicholeskisolve(c,A,l0,l1);  /* forming (c=) (I*r+T)^{-1}U'L'Z'S_kZLU(I*r+T)^{-1}U'L'Z'S_lZLU(I*r+T)^{-1}U'[I,0]Qy */
    matmult(*A,ULZSZLU[l],dpAy[k],0,0);  /* This line and next 3 are a bug */
    d->r=T->r;                            /* fix for incorrect original */
    bicholeskisolve(d,A,l0,l1);     /* derivation - 18/8/99 */
    for (i=0;i<c->r;i++) c->V[i]+=d->V[i];
    OrthoMult(U,c,1,T->r-2,0,1,0);  /* forming (c=) U(I*r+T)^{-1}U'L'Z'S_kZLU(I*r+T)^{-1}U'L'Z'S_lZLU(I*r+T)^{-1}U'[I,0]Qy */
    c->r=Wy->r;
    for (i=T->r;i<Wy->r;i++) c->V[i]=0.0; /* and premutiplying result by (0',I')' */
    OrthoMult(Q,c,0,Q->r,1,1,1);      /* premultiplying by Q' */
    for (i=0;i<c->r;i++) c->V[i]*=rho; /* c=d^2A/dt_idt_j y */
    if (l==k) /* then operator needs additional term dA/dt_i y / e^eta_i*/
    { for (i=0;i<c->r;i++)
      c->V[i]+=dAy[l].V[i]/exp(eta[l]);
    }

    x=0.0;
    for (i=0;i<Wy->r;i++) x+= dAy[l].V[i]*dAy[k].V[i]+(Ay->V[i]-Wy->V[i])*c->V[i];
    x*=2.0/n;d2a[l][k]=d2a[k][l]=exp(eta[l]+eta[k])*x;
  }  /* form da[] */
  for (l=0;l<m;l++)
  { x=0.0;
    for (i=0;i<Wy->r;i++) x+=(Ay->V[i]-Wy->V[i])*dAy[l].V[i];
    x*=2.0/n;
    da[l]=x*exp(eta[l]);
  }
  a=0.0;
  for (i=0;i<Wy->r;i++) a+=Wy->V[i]*Wy->V[i]+(Ay->V[i]-2*Wy->V[i])*Ay->V[i];
  a/=n;
  /* with luck and a fair wind we now have the ingredients for the gradient
     and Hessian */
  for (i=0;i<m;i++)
  { if (ubre)
    { g->V[i]=da[i]- sig2/sqrt(b)*db[i];
      for (j=0;j<=i;j++)
      Hess->M[j][i]=Hess->M[i][j]=d2a[i][j]- sig2/sqrt(b)*d2b[i][j] +
                                    sig2/(2*sqrt(b)*b)*db[i]*db[j];
    } else /* it's GCV */
      { g->V[i]=da[i]/b - a*db[i]/(b*b);
        for (j=0;j<=i;j++)
        Hess->M[j][i]=Hess->M[i][j]=d2a[i][j]/b-(da[i]*db[j]+da[j]*db[i])/(b*b)
                              +2*a*db[i]*db[j]/(b*b*b)-a*d2b[i][j]/(b*b);
      }
   }
  
}

void MSsetup(int m,long n,long np,long nz, long *off,matrix *A,matrix *c,matrix *d,matrix *Ay,matrix *Hess,matrix *g,
matrix *Wy,matrix *R,matrix *J,matrix *z,matrix *Z,matrix *Q,matrix **LZrS,matrix **LZSZL,matrix **ULZSZLU,matrix **H,
matrix **ULZrS,matrix *w,matrix *y,matrix *S,matrix *T,matrix *U,matrix *l0,matrix *l1,matrix **dAy,matrix **dpAy,double **trdA,
double **da,double **db,double ***trd2A,double ***d2a,double ***d2b,double **ninf,double **pinf,double **rw)

/* Routine to do various initialization tasks for MultiSmooth */ 

{ int i,j,k,l;
  double *pp,x,**RM;
  matrix ZC,C;
  *A=initmat(np,np); /* workspace matrix */
  *c=initmat(n,1L); /*     "     vector  */
  *d=initmat(n,1L);
  *Ay=initmat(n,1L);
  *Hess=initmat((long)m,(long)m);*g=initmat((long)m,1L);
  *trdA=(double *)calloc((size_t)m,sizeof(double));
  *da=(double *)calloc((size_t)m,sizeof(double));
  *db=(double *)calloc((size_t)m,sizeof(double));
  *trd2A=(double **)calloc((size_t)m,sizeof(double));
  *d2a=(double **)calloc((size_t)m,sizeof(double));
  *d2b=(double **)calloc((size_t)m,sizeof(double));
  *ninf=(double *)calloc((size_t)m,sizeof(double));
  *pinf=(double *)calloc((size_t)m,sizeof(double));
  for (k=0;k<m;k++)
  { (*trd2A)[k]=(double *)calloc((size_t)m,sizeof(double));
    (*d2a)[k]=(double *)calloc((size_t)m,sizeof(double));
    (*d2b)[k]=(double *)calloc((size_t)m,sizeof(double));
  }
  /* Get Q'R=W^{0.5} J Z */
  *rw=(double *)calloc((size_t)n,sizeof(double));
  for (i=0;i<n;i++) (*rw)[i]=sqrt(w->V[i]);   /* rw contains l.d. of W^{0.5} */
  *Wy=initmat(n,1L);
  for (i=0;i<n;i++) Wy->V[i] = (*rw)[i] * y->V[i];
  if (Z->r)
  { *R=initmat(n,np);mcopy(J,R);
    HQmult(*R,*Z,0,0);R->c=nz;
    /* matmult(R,*J,*Z,0,0); FZ */ /* R=JZ - up to O(n^3) */
  } else
  { *R=initmat(n,np);mcopy(J,R);}          /* case when Z=I */
  RM=R->M; 
  
  for (i=0;i<n;i++)
  { x=(*rw)[i];for (pp=RM[i];pp<RM[i]+R->c;pp++) *pp *= x;}  /* R=W^{0.5} JZ */
  *Q=initmat(np,n);/* altered for efficient storage */
  QR(Q,R);  /* done in up to O(n^3) */
  /* invert R - it is this step that requires n>=nz*/
  R->r=R->c; /* getting rid of zero part */
  /*  InvertTriangular(R);*/ /* DEBUG ONLY */
  /* Form the matrices L'Z'S^{0.5} and L'Z'S_iZL */
  ZC=initmat(np,np);
  *LZrS=(matrix *)calloc((size_t)m,sizeof(matrix));
  *LZSZL=(matrix *)calloc((size_t)m,sizeof(matrix));
  *ULZSZLU=(matrix *)calloc((size_t)m,sizeof(matrix));
  *H= (matrix *)calloc((size_t)m,sizeof(matrix));
  *ULZrS=(matrix *)calloc((size_t)m,sizeof(matrix));
  for (l=0;l<m;l++)   /* Initialization Loop */
  { root(S+l,&C,8*DOUBLE_EPS);    /* S[l]=CC' - initializes C */
    if (Z->r)
    { ZC.c=C.c;ZC.r=np;
      /* set ZC = [0,C',0]' */
      for (i=0;i<off[l];i++) for (j=0;j<C.c;j++) ZC.M[i][j]=0.0;
      for (i=off[l];i<off[l]+C.r;i++)
      for (j=0;j<C.c;j++) ZC.M[i][j]=C.M[i-off[l]][j];
      for (i=off[l]+C.r;i<np;i++)
      for (j=0;j<C.c;j++) ZC.M[i][j]=0.0;
      /* ...and apply Z efficiently, to get Z'C... */
      HQmult(ZC,*Z,1,1);ZC.r=nz;  /* bug fixed here 17/8/99 - Z was untransposed! */
    } else
    { ZC.c=C.c;ZC.r=np;
      for (i=0;i<ZC.r;i++) for (j=0;j<ZC.c;j++) ZC.M[i][j]=0.0;
      for (i=0;i<C.r;i++) for (j=0;j<C.c;j++) ZC.M[i+off[l]][j]=C.M[i][j];
    }
    freemat(C);
    (*LZrS)[l]=initmat(R->c,ZC.c); 
    
    /*  MM= (*LZrS)[l].M;M1M=R->M;M2M=ZC.M;                     
    for (i=0;i<(*LZrS)[l].r;i++) for (j=0;j<(*LZrS)[l].c;j++)
    for (k=0;k<=i;k++) MM[i][j]+=M1M[k][i]*M2M[k][j];*/   /* last 3 lines explicit R^{-1} version */
    
    Rsolv(R,*LZrS+l,&ZC,1); /* non-explicit version */
    (*LZSZL)[l]=initmat(R->c,R->c); /* this memory requirement could be halved */
    matmult((*LZSZL)[l],(*LZrS)[l],(*LZrS)[l],0,1);
    if (R->c>=ZC.c) (*H)[l]=initmat(R->c,R->c);    /* need to make sure that matrix is big enough for storage sharing with ULZrS */
    else { (*H)[l]=initmat(R->c,ZC.c);(*H)[l].c=R->c;}
    (*ULZSZLU)[l]=initmat(R->c,R->c); /* memory requirement could be halved, but would need own choleskisolve */
    (*ULZrS)[l].M=(*H)[l].M;(*ULZrS)[l].r=R->c;(*ULZrS)[l].c=ZC.c; /* sharing memory with H[l] */
  }
  freemat(ZC); 
 
  *T=initmat(R->c,R->c);
  *U=initmat(T->r,T->r);
  *z=initmat(n,1L);
  *l0=initmat(T->r,1L);*l1=initmat(T->r-1,1L);
  *dAy=(matrix *)calloc((size_t)m,sizeof(matrix));
  *dpAy=(matrix *)calloc((size_t)m,sizeof(matrix));
  for (l=0;l<m;l++) { (*dAy)[l]=initmat(n,1L);(*dpAy)[l]=initmat(T->r,1L);}
}


void init_msrep(msrep_type *msrep,int m, int direct_mesh)
/* Initialize the memory for a MultiSmooth diagnostic report object */
{ msrep->g=(double *)calloc((size_t)m,sizeof(double));
  msrep->h=(double *)calloc((size_t)m,sizeof(double));
  msrep->e=(double *)calloc((size_t)m,sizeof(double));
  msrep->edf=(double *)calloc((size_t)direct_mesh,sizeof(double));
  msrep->score=(double *)calloc((size_t)direct_mesh,sizeof(double));
}


void free_msrep(msrep_type *msrep)
/* free the memory for a MultiSmooth diagnostic report object */
{ free(msrep->g);
  free(msrep->h);
  free(msrep->e);
  free(msrep->edf);
  free(msrep->score);
}



double MultiSmooth(matrix *y,matrix *J,matrix *Z,matrix *w,matrix *S,matrix *p,
                 double *theta,long *off,int m,double *sig2,msctrl_type *msctrl,
				 msrep_type *msrep, int direct_mesh,double *trA)

/* routine for multiple smoothing parameter problems, with influence matrix:
   A=r*JZ(Z'J'WJZ*r + \sum_{i=1}^m \theta_i Z'S_iZ)^{-1} Z'J'W
   The routine uses Gu and Wahba's approach of alternating exact searches
   for overall smoothing parameter r, with Newton updates of the theta_i's
   The actual calculations are rather different because the method doesn't
   assume the special structure needed by Gu and Wahba's method.
   The S_i matrices should be supplied in their smallest possible form:
   if p is the total parameter vector then in principle a smoothness measure
   will be of the form p'S_ip, but much of S_i will be zeroes. Suppose that S_i
   contains an s by s non-zero sub matrix S[i], and that q is the s-vector
   consisting of the s elements of p starting from element off[i]. Then the
   measure is given by q'S[i]q. It is the S[i] matrices and array off[] that
   should be supplied to the routine. This way of proceeding means that the
   smoothing parameter vector updates require only around n^3 operations not
   m n^3.
   Problem must have datapoints >= to null space dimension (parameter vector
   length, or columns of Z) - I'm not sure if this restriction is fundamental.

   If any of the m elements of theta[] are <=0 then automatic initialisation
   of smoothing parameters takes place. Otherwise the supplied theta[i]s are
   used as initial estimates. On exit theta[i] contains best estimates of
   theta[i]/rho.
   
   Autoinitialization is performed as follows:

   1. Initialize theta[i] to 1/(tr(S[i])) and estimate rho and p.
   2. re-initilize theta[i] to rank(S[i])/(p'S[i]p)

   Step 2. is heuristic. Suppose that we were to fit model as least squares problem
   min ||y* - X* p||^2 where X*=[X',(la[0]*S[0])^0.5,(la[1]*S[1])^0.5,...]' and
   y*=[y',0,0 ... ]', where la[i] are absolute smoothing parameters. If the implied 
   constant variance assumtion were true then all la[i]*p'S[i]p/rank(S[i]) should be 
   equal (to the resiudal ||y-Xp||^2, in fact).

   The minimisation strategy alternates exact searches for an overall s.p. with trial
   steps for the log relative parameters eta[]. The trial steps are determined as follows:
   
   If the Hessian is +ve definite use a Newton step, but scale the step length so that 
   no component of the step is greater than 5. Half the step length up to 4 times if the 
   step is not successful, if the direction still doesn't work, switch to steepest descent.

   If the Hessian is not +ve definite then use steepest descent, scaling the direction
   so that the largest component has unit length. Half the direction while it's failing.

   Steps are *never* expanded, even if this would decrease the score further: this guards
   against stepping into regions of the score that are flat w.r.t. some eta[i].


   Z is actually supplied as a matrix containing a series of householder
   transformations, as produced by QT when fullQ==0... this improves efficiency
   considerably.... old, full Z code in brackets labelled FZ.
   To obtain Z for Cp=0: call QT(Q,C,0). Then set Q.r=C.r; Q is now used as Z.

   Supplying *sig2 as a positive number signals the routine to use UBRE, rather
   than GCV, since *sig2 is known. Supplying *sig2 as zero or negative causes
   the routine to use GCV. (added 15/1/99)


   msctrl->conv_tol controls the convergence tolerence (1e-6 usually ok)
   msctrl->max_step_half gives the number of step halvings to try during Newton 
                         updates (15 is usually ok)
   msctrl->min_edf is the minimum possible edf for the whole model. Useful for setting
                   rho (overall s.p.) limits in EasySmooth. <=0 to ignore.
 
   Returns gcv/ubre score. 

   msrep is a structure that is loaded with various convergence diagnostics (assuming m>1).

   msrep.g is an array of GCV/UBRE score gradients w.r.t. smoothing params.
   msrep.h is array of GCV/UBRE second derivatives w.r.t. s.p.s (leading diagonal of Hessian)
   msrep.e is array of eigenvalues of Hessian [all should be +ve at convergence]
   msrep.iter is number of iterations taken
   msrep.inok is 1 if second guess was ok (or not auto-initialized) 0 otherwise.
   msrep.m is number of s.p.s
   
*/

{ double *rw,tr,*eta,*del,*trial,*trdA,**trd2A,*db,**d2b,*da,**d2a,
          rho,v,vmin=0.0,x,**MM,**M1M,tdf,xx1,tol,
          *pinf,*ninf,*ldt,best_trA=-1.0,best_sig2=-1.0;
  long i,j,l,n,np,nz;
  int iter,reject,ok=1,autoinit=0,op=0,
      ubre=0,accept=0,steepest=0,gwstart=1;
  matrix z,l0,l1,Q,R,*LZrS,*LZSZL,T,U,A,Wy,Hess,g,c,c_copy,*H,*ULZSZLU,
         *ULZrS,*dAy,*dpAy,d,Ay,dum;
  if (*sig2>0.0) ubre=1; /* use UBRE rather than GCV */
  n=y->r;  /* number of datapoints */
  np=J->c; /* number of parameters */
  for (i=0;i<m;i++) if (theta[i]<=0.0) autoinit=1;

  if (Z->r) nz=np-Z->r;else nz=np; /* dimension of null space */
  
  MSsetup(m,n,np,nz,off,&A,&c,&d,&Ay,&Hess,&g,&Wy,&R,J,&z,Z,&Q,&LZrS,&LZSZL,&ULZSZLU,&H,
	   &ULZrS,w,y,S,&T,&U,&l0,&l1,&dAy,&dpAy,&trdA,&da,&db,&trd2A,&d2a,&d2b,&ninf,&pinf,&rw);
  c_copy=initmat(c.r,1L);
  ldt=(double *)calloc((size_t)T.r,sizeof(double)); /* used for storing leading diagonal of T */
  dum=initmat((long)m,1L); /* dummy matrix for use in Newton step */
  eta=(double *)calloc((size_t)m,sizeof(double));
  del=(double *)calloc((size_t)m,sizeof(double)); /* change in s.p.s */
  trial=(double *)calloc((size_t)m,sizeof(double));
  /* get initial estimates for theta_i and eta_i=log(theta_i) */
  msrep->step_fail=0;
  msrep->inok=1;
  if (autoinit)
  for (l=0;l<m;l++)
  { tr=0.0;for (i=0;i<LZSZL[l].r;i++) tr+=LZSZL[l].M[i][i];
  /* eta[l]=log(n/tr);*/
    eta[l]=log(1.0/(n*tr));
    rho=1.0;
  } else
  { x=0.0;for (i=0;i<m;i++) { eta[i]=log(theta[i]);x+=eta[i];}
    x/=m;rho=1.0;
    for (i=0;i<m;i++) { ninf[i]=eta[i]-x-300.0;pinf[i]=ninf[i]+600.0;}
  }
  iter=0;  /* iteration counter */
  while (ok) /* The main loop */
  { /* form combined smoothness measure */
    reject=1;
    while (reject) /* try current search direction (unless first step) */
    { x=0.0;for (i=0;i<m;i++) { trial[i]=eta[i]+del[i];x+=trial[i];}
      x/=m;
      for (i=0;i<m;i++) {trial[i] -= x;} /* normalising smooths */
      if ((iter>1)||(!autoinit))   /* check smoothing parameters won't lead to overflow */
      { for (i=0;i<m;i++)
        if (trial[i]<ninf[i])
	    { trial[i]=ninf[i];ErrorMessage(_("resetting -ve inf"),0);}
        else if (trial[i]>pinf[i])
	    { trial[i]=pinf[i];ErrorMessage(_("resetting +ve inf"),0);}
      } 
      /* form S the combined smooth measure */
      x=exp(trial[0]);MM=LZSZL[0].M;M1M=T.M;
      for (i=0;i<T.r;i++) for (j=0;j<T.c;j++)
      M1M[i][j]=x*MM[i][j];
      for (l=1;l<m;l++) 
      { x=exp(trial[l]);MM=LZSZL[l].M;
        for (i=0;i<T.r;i++) for (j=0;j<T.c;j++)
        M1M[i][j] += x*MM[i][j];
      }
      UTU(&T,&U);    /* Form S=UTU' */
      z.r=n;
      for (i=0;i<n;i++) z.V[i]=rw[i]*y->V[i]; /* z=W^{1/2}y */
      OrthoMult(&Q,&z,0,Q.r,0,1,1);           /* z=QW^{1/2}y */
      z.r=R.r;                                /* z=[I,0]QW^{1/2}y */
      OrthoMult(&U,&z,1,T.r-2,1,1,0);         /* z=U'[I,0]QW^{1/2}y */
      z.r=n;                                  /* z->[z,x_1]' */
      if (!ubre) *sig2=-1.0; /* setting to signal GCV rather than ubre */
      rho=EasySmooth(&T,&z,&v,&tdf,n,sig2,DOUBLE_EPS*100,direct_mesh,msrep->edf,msrep->score,msctrl->min_edf,
                        msctrl->target_edf,0.0);    /* do a cheap minimisation in rho */
      z.r=R.r;
      if ((!iter||v<vmin)&&(reject!=msctrl->max_step_half-1)) /* accept current step - last condition avoids prob. with bad guess 2 */
      { if (autoinit) { if (iter>1) accept++;} else accept++; /* counting successful updates */
        if (m==1) ok=0; /* there is only one smooth term, so terminate immediately */
	else
	if (iter<=3) ok=1; /* never terminate too soon */ 
	else /* test for convergence - don't look at trial step length - can be long at valid convergence if score flat */
        { tol=msctrl->conv_tol;ok=0;
          if (vmin-v>tol*(1+v)) ok=1;
          xx1=0.0;for (i=0;i<m;i++) xx1+=g.V[i]*g.V[i];xx1=sqrt(xx1);
          if (xx1>pow(tol,1.0/3)*(1+v)) ok=1;
        }  
	for (i=0;i<m;i++) eta[i]=trial[i];reject=0;
	vmin=v;best_trA= tdf;best_sig2= *sig2; /* record V, TrA and sig2 estimates at best minimum so far */ 
      } else   /* contract step */
      { reject++;
   	if (iter>1&&reject==4&&steepest==0) /* switch from Newton to steepest descent */
        { steepest=1;
          x=0.0;for (i=0;i<m;i++) { xx1=fabs(g.V[i]); if (xx1>x) x=xx1;}
		  for (i=0;i<m;i++) del[i] = - g.V[i]/x; /* reset step to steepest descent - length such that biggest component is 1 */
        } else for (i=0;i<m;i++) del[i]*=0.5;
        if (reject==msctrl->max_step_half-1) for (i=0;i<m;i++) del[i]=0.0;
        if (reject==msctrl->max_step_half) reject=0; 
        if (!reject)
	{ if (iter==1&&autoinit) msrep->inok=0; /* report that second guess failed */
	  else if (iter>3) /* terminate as step has failed */
	  { ok=0; 
	    msrep->step_fail=1;
	  }
        }
      }
      if (op==1) 
      { Rprintf("\nv=%12.6g  %12.6g  %d  %d r=%12.6g,tdf=%12.6g",v,vmin,iter,steepest,rho,tdf);
        for (i=0;i<m;i++) Rprintf(" %9.3g",del[i]);
      } 
    }
    
    /* get choleski decomposition of (I*rho+T) */
    for (i=0;i<T.r;i++) {ldt[i]=T.M[i][i];T.M[i][i] += rho;}
    tricholeski(&T,&l0,&l1);
    for (i=0;i<T.r;i++) T.M[i][i] = ldt[i]; /* note this way avoids serious rounding error */
    if ((!iter&&autoinit)||!ok) /* do second update guess at start parameters, or finish (ok==0) */
    { /* get the current best parameter vector ZLU(I*r+T)^{-1}z */
      c_copy.r=c.r=z.r;
      bicholeskisolve(&c,&z,&l0,&l1);
      OrthoMult(&U,&c,1,T.r-2,0,1,0);
      mcopy(&c,&c_copy);

      Rsolv(&R,&c,&c_copy,0);
      for (i=0;i<c.r;i++) c.V[i]*=rho; /* c = r*LU(I*r+T)^{-1}z */

      /*   for (i=0;i<c.r;i++) { x=0.0;for (j=i;j<c.r;j++) x+=c.V[j]*R.M[i][j];c.V[i]=x*rho;}*/ /* explicit R^{-1} version */

      if (ok) /* then it's the second parameter guess */
      { for (i=0;i<c.r;i++) p->V[i]=c.V[i]; /* use p_z not Zp_z */
        c.r=np;
        for (l=0;l<m;l++)
        { for (i=0;i<LZrS[l].c;i++)
          { c.V[i]=0.0;for (j=0;j<LZrS[l].r;j++) c.V[i]+=p->V[j]*LZrS[l].M[j][i];
          }
          tr=0.0;for (i=0;i<LZrS[l].c;i++) tr+=c.V[i]*c.V[i];
	  if (gwstart)
          { if (tr>0.0) trial[l]=log(1/(exp(eta[l])*exp(eta[l])*n*tr)); else trial[l]=0.0;} /* this  version from Wahba 1990 - should work */ 
          else { if (tr>0.0) trial[l]=log(LZrS[l].c/tr); else trial[l]=0.0;}
          del[l]=trial[l]-eta[l];
        }
        /* now estimate effective -ve and +ve infinities */
        x=0.0;for (l=0;l<m;l++) x+=trial[l];x/=m;
        for (l=0;l<m;l++) ninf[l]=trial[l]-x-300.0;
        for (l=0;l<m;l++) pinf[l]=trial[l]-x+300.0;

      }  else /* it's the final calculation */
      { if (Z->r)
        { p->r=np;for (i=0;i<nz;i++) p->V[i]=c.V[i];
          for (i=nz;i<np;i++) p->V[i]=0.0;
          HQmult(*p,*Z,1,0);  /* Q [c,0]'= [Z,Y][c,0]' = Zc */
        } else
        { p->r=np;for (i=0;i<np;i++) p->V[i]=c.V[i];}
        for (l=0;l<m;l++) theta[l]=exp(eta[l])/rho; /* return smoothing parameters */
        if (best_sig2>0.0) *sig2=best_sig2;
        if (best_trA>0.0) *trA=best_trA; else *trA=tdf;
      }
    } else /* full  Newton update */
    { /* call routine to do gradient and Hessian calculations */
      
      MSgH(ubre,m,LZrS,ULZrS,ULZSZLU,&U,&T,&l0,&l1,&A,H,&d,&c,&Wy,&Ay,dAy,dpAy,&Q,&g,&Hess,rho,*sig2,trdA,trd2A,eta,da,d2a,db,d2b);
      /* boringHg(R,Q,LZSZL,y,rw,eta,rho,m,ubre*(*sig2),1e-4); - this can be used for f.d. checking */

      /* and finally work out the update direction........ */
      c.r=A.c=A.r=Hess.r;
      for (i=0;i<c.r;i++) { msrep->g[i]=g.V[i];msrep->h[i]=Hess.M[i][i];} /* fill in diagnostic structure */
      specd(Hess,c); /* get the spectral decomposition of H */
      for (i=0;i<c.r;i++) msrep->e[i]=c.V[i]; /* fill in diagnostic structure */

      if (c.V[c.r-1]<=0.0) /* saddle point approx => Newton's method unhelpful => use steepest descent */
      { x=0.0;for (i=0;i<g.r;i++) if (fabs(g.V[i])>x) x=fabs(g.V[i]);
	for (i=0;i<g.r;i++) del[i]= -g.V[i]/x; /* scale so that longest component of trial step is length 1 */
	steepest=1;
        if (op==1) Rprintf("\nAt saddle, using steepest.");
      } else /* Newton should work  */
      { for (i=0;i<c.r;i++) c.V[i]=1/(c.V[i]);
        matmult(dum,Hess,g,1,0);
        for (i=0;i<c.r;i++) dum.V[i]*=c.V[i];
        matmult(c,Hess,dum,0,0); /* -c is search direction */
		x=5.0;
		for (i=0;i<c.r;i++) if (fabs(c.V[i])>x) x=fabs(c.V[i]);
		x=5.0/x; /* multiplier ensuring step is short enough that no component is larger than 5 */
		for (i=0;i<c.r;i++) del[i] = -c.V[i]*x;
                if (op==1&&x!=1.0) Rprintf("Hess step multiplier = %g",x);
		steepest=0;
      }
    }
    iter++;
  }
  msrep->iter=iter; /* filling in diagnostic structure */
  if (!accept&&m>1) 
      { if (autoinit) ErrorMessage(_("Multiple GCV didn't improve autoinitialized relative smoothing parameters"),0); 
  } 
  freemat(A);freemat(c);freemat(Ay);freemat(d);
  freemat(Wy);freemat(Q);freemat(R);freemat(c_copy);
  freemat(T);freemat(U);freemat(z);freemat(l0);
  freemat(l1);freemat(g);freemat(Hess);freemat(dum);
  for (i=0;i<m;i++)
  { freemat(LZrS[i]);freemat(LZSZL[i]);
    freemat(H[i]);
    /*freemat(ULZrS[i]); not freed - memory shared with H[i] */
    freemat(ULZSZLU[i]);
    freemat(dAy[i]);freemat(dpAy[i]);
    free(trd2A[i]);free(d2b[i]);free(d2a[i]);
  }
  free(ldt);free(LZrS);free(LZSZL);free(H);free(ULZrS);free(ULZSZLU);
  free(dAy);free(dpAy);free(ninf);free(pinf);
  free(trd2A);free(d2b);free(d2a);free(trdA);free(db);free(da);
  free(rw);free(eta);free(del);free(trial);
  return(vmin);
}


void MSmooth(double ft(int,int,int,double*,double*,int,int,int),
             matrix *y,matrix *J,matrix *Z,matrix *w,matrix *S,matrix *p,
             double *theta,long *off,int m,int mp,double *sig2,int transform)

/* This routine is a generalization of multismooth(). The generalization is that
   the smoothing parameters applying to each penalty (eta) can be functions of a
   smaller set of smoothing parameters (lam).
   m is the number of penalties and mp is the number of smoothing parameters.
   ft(int e,int m,int mp, double *eta, double *lam,int i,int j,int k)
        is the function defining the transformation from log true smoothing
        parameters to log parameters multiplying each penalty. Note the log
        transformation. e is a control code: -1 to transform initial eta
        guesses to lam guesses; 0 to transform lam to eta; 1 first partial
        derivative of eta_i w.r.t. lam_j; 2 for 2nd partial of eta_i wrt
        lam_j and lam_k; -2 is used to transform lam so as to achieve a
        uniform reduction in the etas - i.e. the transform of lam required to
        reduce all the eta values by a fixed amount. Works by passing in the
        transformed eta values and the original lam values, passing out
        transformed lam.  

   To reproduce Multismooth ft() would apply identity transforms, return 1
   for 1st partials and zero for second.

   routine for multiple smoothing parameter problems, with influence matrix:
   A=r*JZ(Z'J'WJZ*r + \sum_{i=1}^m \theta_i Z'S_iZ)^{-1} Z'J'W
   The routine is more or less Gu and Wahba's method (although it should
   work with alot less structure).
   The S_i matrices should be supplied in their smallest possible form:
   if p is the total parameter vector then in principle a smoothness measure
   will be of the form p'S_ip, but much of S_i will be zeroes. Suppose that S_i
   contains an s by s non-zero sub matrix S[i], and that q is the s-vector
   consisting of the s elements of p starting from element off[i]. Then the
   measure is given by q'S[i]q. It is the S[i] matrices and array off[] that
   should be supplied to the routine. This way of proceeding means that the
   smoothing parameter vector updates require only around n^3 operations not
   m n^3.
   Problem must have datapoints >= to null space dimension (parameter vector
   length, or columns of Z) - I'm not sure if this restriction is fundamental.

   If any of the m elements of theta[] are -ve then automatic initialisation
   of smoothing parameters takes place. Otherwise the supplied theta[i]s are
   used as initial estimates. On exit theta[i] contains best estimates of
   theta[i]/rho.

   NOTE that for some problems it may make sense to initialise by setting
   running first with very low s.p.s and then using the Wahba suggested 2nd
   guesses - not implemented yet.

   Z is actually supplied as a matrix containing a series of householder
   transformations, as produced by QT when fullQ==0... this improves efficiency
   considerably.... old, full Z code in brackets labelled FZ.
   To obtain Z for Cp=0: call QT(Q,C,0). Then set Q.r=C.r; Q is now used as Z.

   If *sig2>0.0 on entry then UBRE replaces GCV as the s.p. selection criterion.
*/

{ double *rw,tr,*eta,*del,*trial,trA,a,b,*trdA,**trd2A,*db,**d2b,*da,**d2a,
          rho,v,vmin=0.0,x,**RM,*pp,**MM,**M1M,**M2M,*pp1,tdf,xx1,xx2,tol,
          *pinf,*ninf,*lam,score[100],edf[100];
  long i,j,k,l,n,np,nz;
  int iter,reject,ok=1,autoinit=0,op=0,ubre=0;
  matrix z,l0,l1,Q,R,C,ZC,*LZrS,*LZSZL,T,U,A,Wy,Hess,g,c,*H,*ULZSZLU,
         *ULZrS,*dAy,*dpAy,d,Ay,Ht;
  if (*sig2>0.0) ubre=1; /* signals UBRE rather than GCV */
  n=y->r;  /* number of datapoints */
  np=J->c; /* number of parameters */
  for (i=0;i<mp;i++) if (theta[i]<=0.0) autoinit=1; /*** m ->mp */
  if (Z->r) /*nz=Z->c FZ */ nz=np-Z->r;else nz=np; /* dimension of null space */
  A=initmat(np,np); /* workspace matrix */
  c=initmat(n,1L); /*     "     vector  */
  d=initmat(n,1L);
  Ay=initmat(n,1L);
  Hess=initmat((long)m,(long)m);g=initmat((long)m,1L);
  /*for (l=0;l<m;l++) ... appears to be totally redundant matrix allocation */
  { trdA=(double *)calloc((size_t)m,sizeof(double));
    da=(double *)calloc((size_t)m,sizeof(double));
    db=(double *)calloc((size_t)m,sizeof(double));
    trd2A=(double **)calloc((size_t)m,sizeof(double));
    d2a=(double **)calloc((size_t)m,sizeof(double));
    d2b=(double **)calloc((size_t)m,sizeof(double));
    ninf=(double *)calloc((size_t)m,sizeof(double));
    pinf=(double *)calloc((size_t)m,sizeof(double));
    for (k=0;k<m;k++)
    { trd2A[k]=(double *)calloc((size_t)m,sizeof(double));
      d2a[k]=(double *)calloc((size_t)m,sizeof(double));
      d2b[k]=(double *)calloc((size_t)m,sizeof(double));
    }
  }
  /* Get Q'R=W^{0.5} J Z */
  rw=(double *)calloc((size_t)n,sizeof(double));
  for (i=0;i<n;i++) rw[i]=sqrt(w->V[i]);   /* rw contains l.d. of W^{0.5} */
  Wy=initmat(n,1L);
  for (i=0;i<n;i++) Wy.V[i]=rw[i]*y->V[i];
  if (Z->r)
  { R=initmat(n,np);mcopy(J,&R);
    HQmult(R,*Z,0,0);R.c=nz;
    /* matmult(R,*J,*Z,0,0); FZ */ /* R=JZ - up to O(n^3) */
  } else
  { R=initmat(n,np);mcopy(J,&R);}          /* case when Z=I */
  RM=R.M;
  for (i=0;i<n;i++)
  { x=rw[i];for (pp=RM[i];pp<RM[i]+R.c;pp++) *pp *= x;}  /* R=W^{0.5} JZ */
  Q=initmat(np,n);/* altered for efficient storage */
  QR(&Q,&R);  /* done in up to O(n^3) */
  /* invert R - it is this step that requires n>=nz*/
  R.r=R.c; /* getting rid of zero part */
  InvertTriangular(&R);   /* R now contains L */
  /* Form the matrices L'Z'S^{0.5} and L'Z'S_iZL */
  ZC=initmat(np,np);
  LZrS=(matrix *)calloc((size_t)m,sizeof(matrix));
  LZSZL=(matrix *)calloc((size_t)m,sizeof(matrix));
  ULZSZLU=(matrix *)calloc((size_t)m,sizeof(matrix));
  H= (matrix *)calloc((size_t)m,sizeof(matrix));
  ULZrS=(matrix *)calloc((size_t)m,sizeof(matrix));
  for (l=0;l<m;l++)
  { root(S+l,&C,1e-14);    /* S[l]=CC' */
    if (Z->r)
    { ZC.c=C.c;ZC.r=np;
      /* set ZC = [0,C',0]' */
      for (i=0;i<off[l];i++) for (j=0;j<C.c;j++) ZC.M[i][j]=0.0;
      for (i=off[l];i<off[l]+C.r;i++)
      for (j=0;j<C.c;j++) ZC.M[i][j]=C.M[i-off[l]][j];
      for (i=off[l]+C.r;i<np;i++)
      for (j=0;j<C.c;j++) ZC.M[i][j]=0.0;
      /* ...and apply Z efficiently... */
      HQmult(ZC,*Z,1,1);ZC.r=nz; /* bug here fixed 17/8/99 - should be Z'C but was attempting ZC*/ 
    } else
    { ZC.c=C.c;ZC.r=np;
      for (i=0;i<ZC.r;i++) for (j=0;j<ZC.c;j++) ZC.M[i][j]=0.0;
      for (i=0;i<C.r;i++) for (j=0;j<C.c;j++) ZC.M[i+off[l]][j]=C.M[i][j];
    }
    freemat(C);
    LZrS[l]=initmat(R.c,ZC.c);
    MM=LZrS[l].M;M1M=R.M;M2M=ZC.M;
    for (i=0;i<LZrS[l].r;i++) for (j=0;j<LZrS[l].c;j++)
    for (k=0;k<=i;k++) MM[i][j]+=M1M[k][i]*M2M[k][j];
    LZSZL[l]=initmat(R.c,R.c); /* this memory requirement could be halved */
    matmult(LZSZL[l],LZrS[l],LZrS[l],0,1);
    if (R.c>=ZC.c) H[l]=initmat(R.c,R.c);    /* need to make sure that matrix is big enough for storage sharing with ULZrS */
    else { H[l]=initmat(R.c,ZC.c);H[l].c=R.c;}
    ULZSZLU[l]=initmat(R.c,R.c); /* memory requirement could be halved, but would need own choleskisolve */
    /*ULZrS[l]=initmat(R.c,ZC.c); */
    ULZrS[l].M=H[l].M;ULZrS[l].r=R.c;ULZrS[l].c=ZC.c; /* sharing memory with H[l] */
  }
  /* Start the main loop */
  freemat(ZC);
  eta=(double *)calloc((size_t)m,sizeof(double));
  lam=(double *)calloc((size_t)mp,sizeof(double)); /*** added */
  del=(double *)calloc((size_t)mp,sizeof(double)); /* change in s.p.s */
  trial=(double *)calloc((size_t)mp,sizeof(double));
  /* get initial estimates for theta_i and eta_i=log(theta_i) */
  if (autoinit)
  for (l=0;l<m;l++)
  { tr=0.0;for (i=0;i<LZSZL[l].r;i++) tr+=LZSZL[l].M[i][i];
    eta[l]=log(1.0/(n*tr));/*eta[l]= -40.0; */
  } else
  { for (i=0;i<mp;i++) theta[i]=log(theta[i]);
    if (transform) ft(0,m,mp,eta,theta,0,0,0);else
    for (i=0;i<m;i++) eta[i]=theta[i];
    x=0.0;for (i=0;i<m;i++) { x+=eta[i];}x/=m;
    for (i=0;i<m;i++) { ninf[i]=eta[i]-x-300.0;pinf[i]=ninf[i]+600.0;}
  }
  if (transform)               /*** */
  { ft(-1,m,mp,eta,lam,0,0,0); /* get initial lam estimates */
    ft(0,m,mp,eta,lam,0,0,0);  /* transform these back to initial eta estimates */
  } else
  for (i=0;i<m;i++) lam[i]=eta[i];
  T=initmat(R.c,R.c);
  U=initmat(T.r,T.r);
  z=initmat(n,1L);
  l0=initmat(T.r,1L);l1=initmat(T.r-1,1L);
  dAy=(matrix *)calloc((size_t)m,sizeof(matrix));
  dpAy=(matrix *)calloc((size_t)m,sizeof(matrix));
  for (l=0;l<m;l++) { dAy[l]=initmat(n,1L);dpAy[l]=initmat(T.r,1L);}
  iter=0;  /* iteration counter */
  while (ok)
  { /* form combined smoothness measure */
    reject=1;
    while (reject) /* try current search direction (unless first step) */
    { for (i=0;i<mp;i++) { trial[i]=lam[i]+del[i];x+=trial[i];}
      if (transform) ft(0,m,mp,eta,trial,0,0,0); else
      for (i=0;i<m;i++) eta[i]=trial[i];
      x=0.0;for (i=0;i<m;i++) x+=eta[i];x/=m;
      for (i=0;i<m;i++) eta[i] -= x; /* normalising smooths */
      if (transform) ft(-2,m,mp,eta,trial,0,0,0); /* making trial consistent with eta */
      else for (i=0;i<m;i++) trial[i] -= x;
      if ((iter>1)||(!autoinit))   /* check smoothing parameters won't lead to overflow */
      { for (i=0;i<m;i++)
        if (eta[i]<ninf[i])
        eta[i]=ninf[i];
        else if (eta[i]>pinf[i])
        eta[i]=pinf[i];
      } 
      /* form S the combined smooth measure */
      for (i=0;i<T.r;i++) for (j=0;j<T.c;j++)
      T.M[i][j]=exp(eta[0])*LZSZL[0].M[i][j];
      for (l=1;l<m;l++) for (i=0;i<T.r;i++) for (j=0;j<T.c;j++)
      T.M[i][j] += exp(eta[l])*LZSZL[l].M[i][j];
      UTU(&T,&U);    /* Form S=UTU' */
      z.r=n;
      for (i=0;i<n;i++) z.V[i]=rw[i]*y->V[i]; /* z=W^{1/2}y */
     /* matrixintegritycheck(); */
      OrthoMult(&Q,&z,0,Q.r,0,1,1);           /* z=QW^{1/2}y */
      z.r=R.r;                                /* z=[I,0]QW^{1/2}y */
      OrthoMult(&U,&z,1,T.r-2,1,1,0);         /* z=U'[I,0]QW^{1/2}y */
      z.r=n;                                  /* z->[z,x_1]' */
      if (!ubre) *sig2=-1.0; /* signalling use of GCV */
      rho=EasySmooth(&T,&z,&v,&tdf,n,sig2,1e-6,100,edf,score,0.0,-1.0,0.0);    /* do a cheap minimisation in rho */
      z.r=R.r;
      if (!iter||v<vmin) /* accept current step */
      { reject=0;
        /* test for convergence */
        tol=1e-4;ok=0;
        if (vmin-v>tol*(1+v)) ok=1;
        xx1=0.0;for (i=0;i<mp;i++) { xx2=lam[i]-trial[i];xx1+=xx2*xx2;}
        xx1=sqrt(xx1);
        xx2=0.0;for (i=0;i<mp;i++) xx2+=trial[i]*trial[i];
        xx2=sqrt(xx2);xx2=(1+xx2)*sqrt(tol);
        if (xx1>xx2) ok=1;
        xx1=0.0;for (i=0;i<mp;i++) xx1+=g.V[i]*g.V[i];xx1=sqrt(xx1);
        if (xx1>pow(tol,1.0/3)*(1+v)) ok=1;
        for (i=0;i<mp;i++) lam[i]=trial[i];
        vmin=v;
      } else   /* contract step */
      { reject++;
        for (i=0;i<mp;i++) del[i]*=0.5;
        if (reject==8) for (i=0;i<mp;i++) del[i]=0.0;
        if (reject==9) reject=0;
        if (!reject&&iter>3) ok=0;
      }
      if (op) Rprintf("\n%12.6g  %12.6g",v,vmin);
    }
    /* get choleski decomposition of (I*rho+T) */
    for (i=0;i<T.r;i++) T.M[i][i] += rho;
    tricholeski(&T,&l0,&l1);
    for (i=0;i<T.r;i++) T.M[i][i] -= rho;
    if ((!iter&&autoinit)||!ok) /* do second update guess at start parameters */
    { /* get the current best parameter vector ZLU(I*r+T)^{-1}z */
      c.r=z.r;
      bicholeskisolve(&c,&z,&l0,&l1);
      OrthoMult(&U,&c,1,T.r-2,0,1,0);
      for (i=0;i<c.r;i++)
      { x=0.0;for (j=i;j<c.r;j++) x+=c.V[j]*R.M[i][j];c.V[i]=x*rho;}
      if (ok) /* then it's the second parameter guess */
      { for (i=0;i<c.r;i++) p->V[i]=c.V[i]; /* use p_z not Zp_z */
        c.r=np;
        for (l=0;l<m;l++)
        { for (i=0;i<LZrS[l].c;i++)
          { c.V[i]=0.0;for (j=0;j<LZrS[l].r;j++) c.V[i]+=p->V[j]*LZrS[l].M[j][i];
          }
          tr=0.0;for (i=0;i<LZrS[l].c;i++) tr+=c.V[i]*c.V[i];
          eta[l]=log(exp(eta[l])*exp(eta[l])*n*tr);
        }
        if (transform)
        { ft(-1,m,mp,eta,trial,0,0,0);
          ft(0,m,mp,eta,trial,0,0,0);
        } else
        for (i=0;i<mp;i++) trial[i]=eta[i];
        for (l=0;l<mp;l++) del[l]=trial[l]-lam[l];
        /* now estimate effective -ve and +ve infinities */
        x=0.0;for (l=0;l<m;l++) x+=eta[l];x/=m;
        for (l=0;l<m;l++) ninf[l]=eta[l]-x-300.0;
        for (l=0;l<m;l++) pinf[l]=eta[l]-x+300.0;
      }  else /* it's the final calculation */
      { if (Z->r)
        { p->r=np;for (i=0;i<nz;i++) p->V[i]=c.V[i];
          for (i=nz;i<np;i++) p->V[i]=0.0;
          HQmult(*p,*Z,1,0);  /* Q [c,0]'= [Z,Y][c,0]' = Zc */
         /* p->r=Z->r;matmult(*p,*Z,c,0,0); FZ */
        } else
        { p->r=np;for (i=0;i<np;i++) p->V[i]=c.V[i];}
        for (l=0;l<m;l++) eta[l]-=log(rho);
        if (transform) ft(-2,m,mp,eta,lam,0,0,0);
        else for (i=0;i<m;i++) lam[i]=eta[i];
        for (l=0;l<mp;l++) theta[l]=exp(lam[l]); /* return smoothing parameters */
      }
    } else /* full  Newton update */
    { /* form U'L'Z'S_i^{0.5} and U'L'Z'S_iZLU - This is the expensive part -
         <= O(n^3) worth further optimization of address calculation later.... */
      for (l=0;l<m;l++)
      { mcopy(LZrS+l,ULZrS+l);
        OrthoMult(&U,&ULZrS[l],1,T.r-2,1,1,0);
        MM=ULZrS[l].M;M1M=ULZSZLU[l].M;
        for (i=0;i<ULZrS[l].r;i++) for (j=0;j<=i;j++)
        { x=0.0;pp=MM[i];pp1=MM[j];
          for (k=0;k<ULZrS[l].c;k++) {x+= *pp * *pp1;pp++;pp1++;}
          M1M[i][j]=M1M[j][i]=x;
        }
      }
      /* now calculate the various traces needed in the calculation.
         These will be stored in trA, trdA[], trd2A[][]. These calculations
         are up to O(n^2)*/
      trA=triTrInvLL(&l0,&l1)*rho;
      for (l=0;l<m;l++)
      { A.r=ULZrS[l].r;A.c=ULZrS[l].c;
        bicholeskisolve(&A,ULZrS+l,&l0,&l1);
        trdA[l]=0.0;
        for (i=0;i<A.r;i++) for (j=0;j<A.c;j++)
        trdA[l] -= A.M[i][j]*A.M[i][j];
        trdA[l]*=rho;
      }
      /* first form (Ir+T)^{-1}U'L'Z'S_iZLU(Ir+T)^{-0.5} for all i */
      for (l=0;l<m;l++)
      { A.r=ULZSZLU[l].r;A.c=ULZSZLU[l].c;
        bicholeskisolve(&A,ULZSZLU+l,&l0,&l1);
        for (i=0;i<A.r;i++) H[l].M[i][0]=A.M[i][0]/l0.V[0];
        for (j=1;j<A.c;j++) for (i=0;i<A.r;i++)
        H[l].M[i][j]=(A.M[i][j]-H[l].M[i][j-1]*l1.V[j-1])/l0.V[j];
        /* H algorithm checked directly - ok */
      }
      for (l=0;l<m;l++) for (k=0;k<=l;k++)
      { x=0.0;
        for (i=0;i<H[l].r;i++) for (j=0;j<H[k].c;j++)
        x+=H[l].M[i][j]*H[k].M[i][j];
        trd2A[l][k]=trd2A[k][l]=2*x*rho;
        if (l==k) trd2A[l][k]+=trdA[l]/exp(eta[l]);
      }
      /* Now form b, db[], d2b[][] ...... */
      b=1-trA/n;b=b*b;
      for (l=0;l<m;l++) db[l]= exp(eta[l])*2.0/n*(trA/n-1)*trdA[l];
      for (l=0;l<m;l++) for (k=0;k<=l;k++)
      d2b[k][l]=d2b[l][k]=
      exp(eta[l]+eta[k])*2.0/n*(trdA[l]*trdA[k]/n-(1-trA/n)*trd2A[l][k]);
      /* Need the derivatives of the rss term next. Use W^{0.5}y in place of y.
         Form and store vector Ay = Q'[I,0]'U(I*r+T)^{-1}U'[I,0]Qy */
      d.r=n;for (i=0;i<n;i++) d.V[i]=Wy.V[i];  /* W^{0.5}y */
      OrthoMult(&Q,&d,0,Q.r,0,1,1);
      Ay.r=d.r=T.r;
      OrthoMult(&U,&d,1,T.r-2,1,1,0);
      bicholeskisolve(&Ay,&d,&l0,&l1); /* Ay = (I*r+T)^{-1}U'[I,0]Qy */
      /* now for the dAy[] and dpAy[] terms, before finishing off Ay :
         dpAy[i] = (I*r+T)^{-1}U'L'Z'S_iZLU(I*r+T)^{-1}U'[I,0]Qy
          dAy[i] = -Q'[I,0]'U dpAy[i]
         the dpAy[] terms save time in calculating d^2A/dt_idt_j
      */
      for (l=0;l<m;l++)
      { A.r=T.r;A.c=1L;
        matmult(A,ULZSZLU[l],Ay,0,0);
        bicholeskisolve(dpAy+l,&A,&l0,&l1);
        for (i=0;i<dpAy[l].r;i++) dAy[l].V[i]= -dpAy[l].V[i];
        for (i=dpAy[l].r;i<dAy[l].r;i++) dAy[l].V[i]=0.0;
        dAy[l].r= dpAy[l].r;
        OrthoMult(&U,dAy+l,1,T.r-2,0,1,0);
        dAy[l].r=y->r;
        OrthoMult(&Q,dAy+l,0,Q.r,1,1,1);
        for (i=0;i<dAy[l].r;i++) dAy[l].V[i]*=rho;
      }
      /* now finish Ay.... */
      OrthoMult(&U,&Ay,1,T.r-2,0,1,0);
      Ay.r=y->r;for (i=T.r;i<y->r;i++) Ay.V[i]=0.0;
      OrthoMult(&Q,&Ay,0,Q.r,1,1,1);
      for (i=0;i<Ay.r;i++) Ay.V[i]*=rho;
      /* form a, da[] & d2a[][]..... */
      for (l=0;l<m;l++) for (k=0;k<=l;k++)
      { matmult(A,ULZSZLU[k],dpAy[l],0,0);
        c.r=T.r;
        bicholeskisolve(&c,&A,&l0,&l1);
        matmult(A,ULZSZLU[l],dpAy[k],0,0);
        d.r=T.r;
        bicholeskisolve(&d,&A,&l0,&l1);
        for (i=0;i<c.r;i++) c.V[i]+=d.V[i];
        OrthoMult(&U,&c,1,T.r-2,0,1,0);
        c.r=y->r;
        for (i=T.r;i<y->r;i++) c.V[i]=0.0;
        OrthoMult(&Q,&c,0,Q.r,1,1,1);
        for (i=0;i<c.r;i++) c.V[i]*=rho; /* b=d^2A/dt_idt_j y */
        if (l==k) /* then operator needs additional term dA/dt_i y / e^eta_i*/
        { for (i=0;i<c.r;i++)
          c.V[i]+=dAy[l].V[i]/exp(eta[l]);
        }

        x=0.0;
        for (i=0;i<n;i++)
        x+= dAy[l].V[i]*dAy[k].V[i]+(Ay.V[i]-Wy.V[i])*c.V[i];
        x*=2.0/n;d2a[l][k]=d2a[k][l]=exp(eta[l]+eta[k])*x;
      }
      for (l=0;l<m;l++)
      { x=0.0;
        for (i=0;i<n;i++) x+=(Ay.V[i]-Wy.V[i])*dAy[l].V[i];
        x*=2.0/n;
        da[l]=x*exp(eta[l]);
      }
      a=0.0;
      for (i=0;i<n;i++) a+=Wy.V[i]*Wy.V[i]+(Ay.V[i]-2*Wy.V[i])*Ay.V[i];
      a/=n;
      /* with luck and a fair wind we now have the ingredients for the gradient
         and Hessian */
      Hess.r=Hess.c=g.r=(long)m;
      for (i=0;i<m;i++)
      { if (ubre)
        { g.V[i]=da[i]- *sig2/sqrt(b)*db[i];
          for (j=0;j<=i;j++)
          Hess.M[j][i]=Hess.M[i][j]=d2a[i][j]- *sig2/sqrt(b)*d2b[i][j] +
                                    *sig2/(2*sqrt(b)*b)*db[i]*db[j];
        } else /* it's GCV */
        { g.V[i]=da[i]/b - a*db[i]/(b*b);
          for (j=0;j<=i;j++)
          Hess.M[j][i]=Hess.M[i][j]=d2a[i][j]/b-(da[i]*db[j]+da[j]*db[i])/(b*b)
                              +2*a*db[i]*db[j]/(b*b*b)-a*d2b[i][j]/(b*b);
        }
      }
    /*  boringHg(R,Q,LZSZL,y,rw,trial,rho,m);*/
      if (op)
      { Rprintf("\n");
        for (i=0;i<m;i++)
        { for (j=0;j<=i;j++) Rprintf("%8.4g  ",Hess.M[i][j]);Rprintf("\n");}
        for (i=0;i<m;i++) Rprintf("\n%g",g.V[i]);
      }
      /* Now transform the Hessian and gradient if necessary */
      if (transform)
      { Ht=initmat((long)mp,(long)mp);
        for (i=0;i<mp;i++) for (j=0;j<mp;j++)
        for (k=0;k<m;k++)
        { Ht.M[i][j]+=g.V[k]*ft(2,m,mp,eta,lam,k,i,j);
          for (l=0;l<m;l++)
          Ht.M[i][j]+=Hess.M[k][l]*ft(1,m,mp,eta,lam,k,i,0)*ft(1,m,mp,eta,lam,l,j,0);
        }
        for (i=0;i<mp;i++) for (j=0;j<mp;j++) Hess.M[i][j]=Ht.M[i][j];
        freemat(Ht);
        Ht=initmat((long)mp,1L);
        for (i=0;i<mp;i++) for (k=0;k<m;k++)
        Ht.V[i]+=g.V[k]*ft(1,m,mp,eta,lam,k,i,0);
        for (i=0;i<mp;i++) g.V[i]=Ht.V[i];
        freemat(Ht);
        Hess.r=Hess.c=g.r=(long)mp;
      }

      /* and finally the update ........ */

      A.c=A.r=Hess.r;
      x=0.0;for (i=0;i<mp;i++) x+=Hess.M[i][i];x/=m;x*=0.0001;
      x=fabs(x);
      while(!chol(Hess,A,0,0))
      { for (i=0;i<mp;i++) Hess.M[i][i]+=x;
        x*=2.0;
      }
      c.r=g.r;
      choleskisolve(A,c,g);
      for (i=0;i<mp;i++) del[i]= -c.V[i];
    }
    iter++;
  }
  freemat(A);freemat(c);freemat(Ay);freemat(d);
  freemat(Wy);freemat(Q);freemat(R);
  freemat(T);freemat(U);freemat(z);freemat(l0);
  freemat(l1);freemat(g);freemat(Hess);
  for (i=0;i<m;i++)
  { freemat(LZrS[i]);freemat(LZSZL[i]);
    freemat(H[i]);
    /*freemat(ULZrS[i]); not freed - memory shared with H[i] */
    freemat(ULZSZLU[i]);
    freemat(dAy[i]);freemat(dpAy[i]);
    free(trd2A[i]);free(d2b[i]);free(d2a[i]);
  }
  free(LZrS);free(LZSZL);free(H);free(ULZrS);free(ULZSZLU);
  free(dAy);free(dpAy);free(ninf);free(pinf);
  free(trd2A);free(d2b);free(d2a);free(trdA);free(db);free(da);
  free(rw);
  free(eta);
  free(del);
  free(lam);
  free(trial);
}



/*******************************************************************************************************/
/*

Bug fix log:

21/2/2001 - Multismooth/Msmooth: H[l] and ULZrS[l] share memory space for their .M - under highly unusual
            circumstances it was possible for H[l].M to have fewer columns allocated than were needed
            by ULZrS[l].M. This caused a matrix out of bound write error. This is now fixed.

24/2/2001 - Multismooth now returns gcv/ubre score

31/10/2001 - Warning message given from EasySmooth if there is a failure to bracket the GCV minimum
             w.r.t. overall smoothing parameter.
31/10/2001 - MultiSmooth() now warns if it doesn't manage to update smoothing parameters beyond initial
             values. It also now takes a control structure to allow tightening/loosening of tolerences.
14/11/2001 - Fixed a bug whereby if data are all zero, MultiSmooth never returns - problem was that
             second s.p. guesses in this case would all be zero, since parameters are all zero.

2/1/2002   - OUCH! following problems uncovered in Multismooth:
             i) Routine can terminate if 2nd GUESS is poor!! [FIXED] 
             ii) In EScv and multismooth I set T<-T+rI and then T<-T-rI, leading to drastic precision loss.
                 [FIXED]
             iii) default range in Easysmooth too narrow - sometimes failed to trap minimum - altered and
                  checks/warnings added [FIXED]
             iv) Newton steps often fail to find exact optimum: but steepest descent works. Suggest 
                 modifying Multismooth to a Hybrid Newton/steepest - will need step length selection. 
             v) Hessian/gradient calculations double checked and are good, and results of *really* minimising gcv 
                score are also good! 
9/1/2002 - Major overhaul of Multismooth() method:
           Problems occurr if we step into a region that is flat w.r.t. one of the smoothing parameters. 
		   The following steps help greatly:
		   i) Second guesses taken from Wahba 1990 are so poor that they almost never improve the GCV score:
		      have altered to second guesses - they work for every example I've tried so far.
           ii) Method now uses steepest descent if the Hessian is not positive definite - this makes more sense that
		       simply regularizing - if the Hessian is not positive definite then we know that the Newton
			   model is poor, so it's silly to use it.
           iii) Method switches to steepest descent if a Newton direction is still failing after 4 step length
		        reductions, since by then we again know that the Newton model is very poor.
           iv) Newton steps are scaled if necessary so that no component is increased by more than 5.
		   v) Steepest descent steps are initially set to the length that makes the largest component have
		      length 1. 
           vi) If a trial step fails, it's length is halved (halving repeated to some limit). Successful steps 
		       are never extended - this is to avoid the danger of pushing one component erroneously into
			   a flat region. 
           vii) Multismooth now returns a convergence diagnostics object.
		   viii) Quite extensive testing on the standard problems in mgcv package suggests that convergence is 
		         now very reliable.
           ix) It is important to avoid initial guesses that will lead to wildly different element sizes in combined
		       wiggliness matrix - better to use auto-initialization in such cases.

25/1/2002 - rho search range in EasySmooth was too high - could lead to s.p. estimates so high that numerical 
            problems lead to zero parameter estimates for the unpenalized terms and zero edf estimates.
            Also in EasySmooth a warning was generated whenever the best rho was really at "zero" or "infinity" 
            - this warning is now only generated if the gradient at the boundary is non-zero as judged relative
            to the biggest drop seen in the score. 
1/5/2002 - R compiles with options that break IEEE fp compliance on some platforms. This means that optimization can 
           use floating point registers to store variables with excess precision. This in turn breaks equality 
           testing of doubles (can also break inequality testing). This is pretty odd, since e.g. Watkins (1991),
           uses constructions that rely on sensible performance of equality testing (according to Brian Ripley, 
           so does linpack!). The Intel site has a worryingly unconvinving explanation of their reasons for 
           carrying extra precision (basically that it allows the programmer to be sloppy). This means that it 
           is not possible to guarantee exact cross platform or cross compiler compatibility. Effectively every 
           floating point operation is possibly subject to a random perturbation beyond the range of the mantissa, 
           which can feed back into the mantissa. The benefits are tiny speed improvements. 
           EasySmooth() is the main victim of this - it's search was stopping prematurely becuase of incorrect 
           f.p. equality testing.     
14/5/02  - Explicit formation of L=R^{-1} and formation of B=LA or B=L'A replaced by calls to Rsolve which solves
           RB=A or R'B=A for B.
23/10/02 - Because of occasional ill-conditioning problems when calculating term-wise degrees of freedom, tr(A)
           is now exported from Multismooth, to allow checking.

*/
/*******************************************************************************************************/
































