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

/* Routines for quadratic programming and other constrained optimization. */


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include "matrix.h"
#include "qp.h"
#include "general.h"

#define DELMAX 35L

void ErrorMessage(char *msg,int fatal);

#define max(a,b)    (((a) > (b)) ? (a) : (b))
#define min(a,b)    (((a) < (b)) ? (a) : (b))

#define round(a) ((a)-floor(a) <0.5 ? (int)floor(a):(int) floor(a)+1)




matrix addconQT(Q,T,a,u) matrix *Q,T,a,*u;

/* A constraint, a (a row vector), is added to the QT factorization of
   the working set. T must have been initialised square, and then had T.r
   set to correct length. */

{ long q,i,j;
  double la,ra=0.0,*cV,*bV,*T1V;
  matrix b,c;
  c=initmat(Q->r,1L);b=initmat(Q->r,1L);(*u)=initmat(Q->r,1L);
  for (i=0;i<c.r;i++) for (j=0;j<a.c;j++) c.V[i]+=a.V[j]*Q->M[j][i];
  la=dot(c,c);
  cV=c.V;bV=b.V;
  q=T.c-T.r-1;
  if (q!=0L)
  { for (i=q+1;i<a.c;i++) { ra+=cV[i]*cV[i];bV[i]=cV[i];}
    if ((la-ra)<0.0)
    { ErrorMessage(_("ERROR in addconQT."),1);}
    else
    bV[q]=sqrt(la-ra);
    if (cV[q]>0.0) bV[q]= -bV[q];
    householder(u,c,b,q);
    Hmult((*Q),(*u));
  } else
  for (i=0;i<a.c;i++) bV[i]=cV[i];
  T1V=T.M[T.r];T.r++;
  for (j=0;j<T.c;j++) T1V[j]=bV[j];
  freemat(b);
  freemat(c);
  return(T);
}

void GivensAddconQT(matrix *Q,matrix *T,matrix *a,matrix *s,matrix *c)

/* A constraint, a (a row vector), is added to the QT factorization of
   the working set, the QT factorisation is updated in a manner that
   allows easy update of the choleski factors of Z'GZ (R). The Givens
   transformations used to do this are stored in s and c. there are s.r
   of them and when applied from the right they zero successive elements
   of a by rotation into the next element.
   T must have been initialised square (and then had T.r reset)
   s and c need T->c-T->r-1 rows to store the Givens rotations and must be
   initialized outside the routine.
   */

{ long q,i,j;
  double Qi,r,cc,ss,*bV,*sV,*cV,**QM,*QV,bb,bb1;
  matrix b;
  b.V=T->M[T->r]; b.r=Q->r;b.c=1L;
  for (i=0;i<T->c;i++) b.V[i]=0.0;
  for (i=0;i<b.r;i++) for (j=0;j<Q->r;j++) b.V[i]+=Q->M[j][i]*a->V[j];
  /* now calculate a series of Givens rotations that will rotate the null basis
     so that it is orthogonal to new constraint a */
  bV=b.V;cV=c->V;sV=s->V;QM=Q->M;
  q=T->c-T->r-1; /* number of Givens transformations needed */
  for (i=0;i<q;i++)
  { /* first calculate the Givens transformation */
    bb=bV[i];bb1=bV[i+1];
    r=bb*bb+bb1*bb1;r=sqrt(r);
    if (r==0.0) { ss=sV[i]=0.0;cc=cV[i]=1.0;} else
    { ss=sV[i]=bb/r;cc=cV[i]= -bb1/r;
      bV[i]=0.0; /* non-essential */
      bV[i+1]=r;
    }
    /* now apply it to Q */
    for (j=0;j<Q->r;j++)
    { QV=QM[j];
      Qi=QV[i];
      QV[i]=cc*Qi + ss*QV[i+1];
      QV[i+1]=ss*Qi - cc*QV[i+1];
    }
  }
  T->r++;
}



void LSQPaddcon(matrix *Ain,matrix *Q,matrix *T,matrix *Rf,matrix *Py,matrix *PX,
                matrix *s,matrix *c,int sth)
/* Adds the sth row of Ain to the avtive set, updates Q and T using a sequence
   of T->c-T->r-1 Givens rotations from the right, coefficients of which are
   stored in s and c. The ith rotation acts on elements (i,i+1) (i=0,1,...).
   Updates the upper triangular (lower left 0) matrix Rf = PXQ, by applying the
   above Givens rotations from the right (updating Q) which introduces elements
   on the sub diagonal of Rf; these subdiaogonal elements are then zeroed using
   Givens rotations from the left, by way of updating P. Hence Py and PX can
   be updated at the same time. */

{ matrix a;
  double RfMji,*RfV,*RfV1,ss,cc,r,x1,x2;
  int i,j,k;
  a.V=Ain->M[sth];a.r=Ain->c;a.c=1L; /* vector containing sth constraint */
  s->r=T->c-T->r-1;  /* number of Givens rotations about to be returned */
  /* Update Q and T and return Givens rotations required to do so ....*/
  GivensAddconQT(Q,T,&a,s,c);
  /* Now apply the rotations from the right to Rf....*/
  for (i=0;i<s->r;i++)
  { cc=c->V[i];ss=s->V[i];
    k=i+2;if (k>Rf->r) k--;
    for (j=0;j<k;j++)
    { RfV=Rf->M[j];
      RfMji=RfV[i];
      RfV[i]=cc*RfMji+ss*RfV[i+1];
      RfV[i+1]=ss*RfMji - cc*RfV[i+1];
    }
  }
  /* Now zero the subdiagonal elements that have just been introduced, and apply
     the Givens rotations from the left, used to do this, to Py and PX */
  for (i=0;i<s->r;i++) /* work through the extra subdiagonal elements */
  { /* this will act on rows i and i+1, zeroing i+1,i - work out coefficients */
    RfV=Rf->M[i];RfV1=Rf->M[i+1];
    x1=RfV[i];x2=RfV1[i];
    r=sqrt(x1*x1+x2*x2);ss=x2/r;cc=x1/r;
    Rf->M[i][i]=r;Rf->M[i+1][i]=0.0;
    for (j=i+1;j<Rf->c;j++) /* apply rotation along the rows */
    { x1=RfV[j];x2=RfV1[j];
      RfV[j]=cc*x1+ss*x2;
      RfV1[j]=ss*x1-cc*x2;
    }
    /* Apply this rotation to  Py */
    x1=Py->V[i];x2=Py->V[i+1];
    Py->V[i]=cc*x1+ss*x2;
    Py->V[i+1]=ss*x1-cc*x2;
    /* and apply the same rotation to PX */
    for (j=0;j<PX->c;j++) /* work along the rows */
    { x1=PX->M[i][j];x2=PX->M[i+1][j];
      PX->M[i][j]=cc*x1+ss*x2;
      PX->M[i+1][j]=ss*x1-cc*x2;
    }
  }
}


int LSQPstep(int *ignore,matrix *Ain,matrix *b,matrix *p1,matrix *p,matrix *pk)

/* This is the stepping routine for the constrained least squares fitting
   routine. It should be faster than step, but more or less does the same thing.
   The return value is -1 for a minimum, otherwise the row of Ain containing the
   constraint to add is returned.
   ignore[i] should be set to 1 to ignore row i of Ain, to 0 to include it.

   Starting from p a step is taken to p+pk, if this would violate any
   constraints in the working set, then a step is taken from p along pk, to the
   closest constraint. The constraints are Ain p >= b.

   On exit: p1 contains the new parameter vector; the return value is -1 for a
            minimum, otherwise the constraint that needs to be added (i.e. the
            row of Ain)
*/
{ double Ap1,ap,apk,alpha,alphamin,*AV,*pV,*p1V,*pkV;
  int imin,i,j;
  alphamin=1.0;imin= -1;
  p1V=p1->V;pV=p->V;pkV=pk->V;
  for (i=0;i<p->r;i++) p1V[i]=pV[i]+pkV[i]; /* step all the way to minimum */
  for (i=0;i<Ain->r;i++)         /* work through the constraints */
  { AV=Ain->M[i];
    if (!ignore[i])     /* skip any already in working set */
    { Ap1=0.0;
      for (j=0;j<Ain->c;j++) Ap1+=AV[j]*p1V[j]; /* form  A p1 = A(p+pk) */
      if ((b->V[i]-Ap1)>0.0) /* does p+pk violate the ith constraint? */
      { ap=0.0;apk=0.0;        /* working out quantities needed to find distance to constraint from p */
	for (j=0;j<Ain->c;j++)
	{ ap+=AV[j]*pV[j];
	  apk+=AV[j]*pkV[j];
	}
	if (fabs(apk)>0.0)
	{ alpha=(b->V[i]-ap)/apk; /* p + alpha*pk is on the ith constraint */
          if (alpha<alphamin)    /* if this is the closest constraint to p, record the fact */
	  { alphamin=max(0.0,alpha);imin=i;
            for (j=0;j<p->r;j++) p1V[j]=pV[j]+alphamin*pkV[j]; /* 2/2/97 - avoids distance calc for all that would violate full step */
          }
        }
      }
    }
  }
  return(imin);

}


void LSQPdelcon(matrix *Q,matrix *T,matrix *Rf,matrix *Py,matrix *PX,int sth)

/* This routine deletes row s from the active set matrix, A, say, where
   AQ=[0,T] and T is reverse lower triangular (upper left is zero). It updates
   Q and T using Givens rotations from the right. These rotations induce
   subdiagonal elements in Rf=PXQ from column Rf->c-T->r to column Rf->c-s+2,
   where T->r is the number of active constraints before deletion.
   Note however that the Givens rotations that update Q and T, have to be
   applied in an order that works back through the columns of Rf=PXQ - this has
   the potential to produce a triangular block of elements below the diagonal,
   if they are all applied before applying the update rotations for P. Hence the
   appropriate thing to do is to apply each rotation from the left to Rf, as it
   is obtained and then work out the Givens rotation from the left that will
   immediately zero the unwanted subdiagonal element - this being an update of
   P, which should immediately be applied to PX and Py.
*/

{ int i,j,colj,coli,k,Tr,Tc,Qr,T1r,T1c;
  double r,s,c,xi,xj,**TM,**QM,*TV,*QV,*T1V,*RfV,*RfV1;
  Tr=T->r;TM=T->M;QM=Q->M;Tc=T->c;Qr=Q->r;
  for (i=sth+1;i<Tr;i++)   /* work down the rows from the deletion point (row not removed yet) */
  { coli=Tc-i-1;colj=Tc-i;    /* coli is zeroed - colj=coli+1 */
    xi=TM[i][coli];xj=TM[i][colj];
    r=xi*xi+xj*xj;
    r=sqrt(r);
    s=xi/r;c=xj/r;         /* Givens coefficients */
    for (j=i;j<Tr;j++)     /* Apply rotation to T */
    { TV=TM[j];
      xi=TV[coli];
      TV[coli]= -c*xi+s*TV[colj];
      TV[colj]=s*xi+c*TV[colj];
    }
    for (j=0;j<Qr;j++)   /* Apply rotation to Q */
    { QV=QM[j];
      xi=QV[coli];
      QV[coli]= -c*xi+s*QV[colj];
      QV[colj]=s*xi+c*QV[colj];
    }
    /* Now the awkward bit - the rotation must be applied to Rf=PXQ */
    for (j=0;j<=colj;j++) /* working down to the diagonal (and just below!) */
    { RfV=Rf->M[j];       /* row to apply rotation to */
      xi=RfV[coli];
      RfV[coli]= -c*xi+s*RfV[colj];
      RfV[colj]=s*xi+c*RfV[colj];
    } /* There is now an unwanted element at row colj, column coli */
    /* Calculate a rotation from the right that will zero the extra element */
    xi=Rf->M[coli][coli];xj=Rf->M[colj][coli]; /* xj to be zeroed */
    r=sqrt(xi*xi+xj*xj);
    s=xj/r;c=xi/r;         /* Givens coefficients to zero xj into xi */
    Rf->M[coli][coli]=r;Rf->M[colj][coli]=0.0;
    /* Now apply to rest of row from column colj (column coli already done) */
    RfV=Rf->M[coli];RfV1=Rf->M[colj];
    for (j=colj;j<Rf->c;j++)
    { xi=RfV[j];xj=RfV1[j];
      RfV[j]=c*xi+s*xj;
      RfV1[j]=s*xi-c*xj;
    }
    /* And apply this rotation from the right to Py and PX */
    /* Apply this rotation to  Py */
    xi=Py->V[coli];xj=Py->V[colj];
    Py->V[coli]=c*xi+s*xj;
    Py->V[colj]=s*xi-c*xj;
    /* and apply the same rotation to PX */
    for (j=0;j<PX->c;j++) /* work along the rows */
    { xi=PX->M[coli][j];xj=PX->M[colj][j];
      PX->M[coli][j]=c*xi+s*xj;
      PX->M[colj][j]=s*xi-c*xj;
    }
  }
  /* Now actually remove the extra row from T - this could be done awefully efficiently */
  /* by shuffling the pointers to rows, but it would probably end in tears, so I haven't */
  T->r--;T1r=T->r;T1c=T->c;
  for (k=0;k<T1r;k++)
  { T1V=TM[k];TV=TM[k];
    for (j=0;j<T1c-k-1;j++) T1V[j]=0.0;
    for (j=T1c-k-1;j<T1c;j++)
    if (k<sth) T1V[j]=TV[j];
    else T1V[j]=TM[k+1][j];
  }
}



int LSQPlagrange(matrix *X,matrix *Q,matrix *T,matrix *p,matrix *Xy,matrix *p1,
                 matrix *y1,int *fixed, int fixed_cons)

/* This routine attempts to find the lagrange multipliers associated with the
   currently active constraints (assuming that we're at a minimum in the current
   null space). If the Active constraint matrix is A then Ap=b where b is a set
   of constants. Furthermore AQ=[0,T] where T is reverse lower triangular
   (zero at upper left). The Lagrange multipliers, l, should satisfy: A'l=g
   where g is the gradient of the quadratic form at p, i.e. X'Xp-X'y.
   (Unfortunately I can't figure out a way of avoiding explicit formation of
   X'Xp....) So, l'A=g' => l'[0,T]=g'Q, and to find l, solve l'T=x, where x is
   the last tk=T->r rows of g'Q - this also yields the minimum of ||A'l-g||,
   which is appropriate.

   Note that T passed to the routine actually contains [0,T] and the first
   fixed_cons rows of T relate to the fixed constraints (if any).

   p1 and y1 are workspace matrices of length p->r and X->r respectively

   The routine returns -1 if there are no -ve multiplier estimates, otherwise it
   returns the index of *Inequlity* constraint with the most negative one.

   fixed[i] is set to 1 if the corresponding inequlity constraint is to be
   left in the active set regardless of lagrange multiplier - this is part
   of a strategy to avoid repeatedly deleting constraints wrongly.
*/

{ int i,j,tk;
  double x;
  tk=T->r;
  vmult(X,p,y1,0);  /* form y1= Xp */
  vmult(X,y1,p1,1); /* form p1 = X'Xp */
  for (i=0;i<p1->r;i++) p1->V[i]+= -Xy->V[i]; /* form p1 = g = X'Xp - X'y */
  /* now create the last tk=T->r elements of g'Q and store in y1 */
  for (i=0;i<tk;i++)
  { y1->V[i]=0.0;
    for (j=0;j<Q->r;j++) y1->V[i]+=p1->V[j]*Q->M[j][Q->c-tk+i];
  }
  /* Now solve l'T=g'Q (where first tk rows of y1 contain g'Q).... */
  for (i=tk-1;i>=fixed_cons;i--) /* work down through the the lagrange multipliers */
  { x=0.0;for (j=i+1;j<tk;j++) x+=p1->V[j]*T->M[j][T->c-i-1];
    if (T->M[i][T->c-i-1]!=0.0) p1->V[i]=(y1->V[tk-i-1]-x)/T->M[i][T->c-i-1];else p1->V[i]=0.0;
  }
  /* Now look for the most negative multiplier for an inequlity constraint */
  x=0.0;j=-1;
  for (i=fixed_cons;i<tk;i++)
  if ((!fixed[i-fixed_cons])&&(p1->V[i]<x)) { j=i;x=p1->V[i];}
/*  if (j==-1) if (p1->V[i]<x) { j=i;x=p1->V[i];} */ /* only delete last constraint added if it has only -ve multiplier */
  if (j!=-1) j -= fixed_cons;
  return(j); /* returns index of inequality constraint to delete */
}


/***************************************************************************/
/* Main Public Routines.                                                   */
/***************************************************************************/

void QPCLS(matrix *Z,matrix *X, matrix *p, matrix *y,matrix *Ain,matrix *b,matrix *Af,int *active)

/* This routine aims to fit linearly constrained least squares problems of the
   form:
           min ||Xp-y||^2    subject to Ain p>=b and Af p = constant

   *without* forming X'X directly.
   By suitable redefinition of X and y it's easy to perform weighted and/or
   penalized regressions using this routine......

   The routine uses working matrices T, Q, Rf, PX
   and working vectors Py, Xy, pz, pk, Pd
   In addition the routine creates workspace for the various service routines
   called by it, in order to avoid excessive memory allocation and deallocation.

   The Algorithm is as follows...
   1. Form the QT factorisation of Af: Af Q = [0,T] T reverse lower triangular
      (i.e top left 0). Q contains column bases for the null and range spaces of
      Af: Q=[Z,Y]. Apply Q to X to get XQ(=[XZ,XY]). Form Q explicitly to give
      ready access to the null space basis Z.
   2. Perform QR decomposition: XQ = P'Rf where P is orthogonal and Rf is upper
      triangular (lower left 0). Hence Rf= PXQ=[PXZ,PXY], as required.
      Apply P to y to get Py. Apply P to X to get PX.
   3. Form Pd = Py-PXp, and solve: minimise || R pz - Pd ||^2, where R is the
      first p->r-tk-Af->r rows and columns of Rf. Solution occurs when R pz=x and
      x is the first p->r - tk - Af->r rows of Pd. (Note that Gill et al. get
      the sign wrong for Pd.)
   4. Evaluate pk=Z pz, and step along it to minimum (goto 6.) or constraint.
   5. Add constraint to working set: update QT factorisation; update Rf; update
      Py and PX. Return to 3.
   6. Evaluate Lagrange multipliers l where Ac'l=g and g=X'Xp-X'y - Ac is the
      active constraint matrix. Clearly g involves X'X, which is unfortunate,
      but I can't figure out a way around it - however, it is only the signs of
      l that matter, so hopefully this is not critical. If multipliers are all
      +ve goto 8. otherwise proceed....
   7. Delete the constraint with the most -ve multiplier, updating Q, T, Rf, Py
      and PX at the same time. Return to 3.
   8. Convergence! A minimum has been achieved. Free the workspace matrices and
      vectors and the indexing arrays, obtain Z, and return.


   On exit active[] contains the number of active inequlity constraints in active[0], 
   and the row number of these constraints in Ain in the remaining elements of
   active[], active must be initialized to length p.r+1 on entry.

   See documentation in service routines:
   LSQPlagrange(); LSQPaddcon(); LSQPdelcon(); (above)
   Rsolv() (in matrix.c)
   for further details on steps 6, 5, 7 and 3.
   The approach is taken from Gill, Murray and Wright (1981) Practical
   Optimization page 180-181 Section 5.3.3. (But note wrong signs on p181
   first display equation and definition of d_k)

   Routine has been tested against less numerically stable alternative using QP().

   20/11/99

*/

{ matrix Q,T,Rf,PX,Py,a,P,p1,s,c,Xy,y1,u,Pd,pz,pk;
  int k,i,j,tk,*I,*ignore,iter=0,*fixed,*delog,maxdel=100;
  double x;
  I=(int *)R_chk_calloc((size_t) p->r,sizeof(int)); /* I[i] is the row of Ain containing ith active constraint */
  fixed=(int *)R_chk_calloc((size_t) p->r,sizeof(int)); /* fixed[i] is set to 1 when the corresponding inequality constraint is to be left in regardless of l.m. estimate */
  ignore=(int *)R_chk_calloc((size_t) Ain->r,sizeof(int)); /* ignore[i] is 1 if ith row of Ain is in active set, 0 otherwise */
  delog=(int *)R_chk_calloc((size_t) Ain->r,sizeof(int)); /* counts up number of times a constraint is deleted */
  p1=initmat(p->r,1L);    /* a working space vector for stepping & lagrange */
  y1=initmat(y->r,1L);    /* a work space vector for lagrange */
  s=initmat(p->r,1L);c=initmat(p->r,1L); /* working space vectors for Givens rotation */
  Xy=initmat(p->r,1L);     /* vector storing X'y for use in lagrange multiplier calculation */
  vmult(X,y,&Xy,1);      /* form X'y */
  Rf=initmat(X->r,X->c);  /* Rf=PXQ, where P and Q are orthogonal */
  mcopy(X,&Rf);          /* initialize Rf while P and Q are identity matrices */
  T=initmat(p->r,p->r);   /* initialised to max possible size */
  Q=initmat(p->r,p->r);   /* required for access to Z for null space to full space transform */
  /* initialize Q, T and Rf using fixed constraints (if any) .... */
  for (i=0;i<p->r;i++) for (j=0;j<p->r;j++) Q.M[i][j]=0.0;
  for (i=0;i<p->r;i++) Q.M[i][i]=1.0;
  T.r=0L;a.r=1L;a.c=Af->c;
  for (i=0;i<Af->r;i++)
  { a.V=Af->M[i];
    T=addconQT(&Q,T,a,&u); /* adding constraint from Af to working set */
    Hmult(Rf,u);           /* updating Rf (=XQ, at present) */
    freemat(u);            /* freeing u created by addconQT() */
  }
  /* Now Form Rf, proper. i.e. PXQ, using QR factorization */
  P=initmat(Rf.c,Rf.r);
  QR(&P,&Rf);   /* Rf now contains Rf=PXQ   (on entry it contained XQ) */
  Py=initmat(y->r,1L);mcopy(y,&Py);
  OrthoMult(&P,&Py,0,(int)P.r,0,1,1); /* Form Py */
  PX=initmat(X->r,X->c);mcopy(X,&PX);
  OrthoMult(&P,&PX,0,(int)P.r,0,1,1); /* Form PX */
  freemat(P); /* no longer needed */
  P=initmat(b->r,1L); /* used solely for feasibility checking */
  Pd=initmat(y->r,1L);pz=initmat(p->r,1L);pk=initmat(p->r,1L);
  tk=0;             /* The number of inequality constraints currently active */
  /*printf("\nLSQ");*/
  while(1)
  { iter++;
    /* Form Pd=Py-PXp and minimize ||R pz - Pd|| */
    vmult(&PX,p,&Pd,0); /* Pd = PXp */
    for (i=0;i<Pd.r;i++) Pd.V[i] = Py.V[i]-Pd.V[i]; /* Pd=P(y-Xp) */
    Rf.c=Rf.r=p->r-tk-Af->r; /* Restrict attention to QR factor of PXZ */
    for (i=0;i<Rf.c;i++) if (Rf.M[i][i]==0.0) ErrorMessage(_("QPCLS - Rank deficiency in model"),1);
    Rsolv(&Rf,&pz,&Pd,0);  /* solve R pz= Pd for pz - search direction in null space */
    Rf.r=X->r;Rf.c=X->c; /* Restore Rf */
    pz.r=p->r-tk-Af->r;
    /* Find pk = Z pz, the search direction */
    for (i=0;i<pk.r;i++)
    { pk.V[i]=0.0; for (j=0;j<pz.r;j++) pk.V[i]+=Q.M[i][j]*pz.V[j];}
    /* Take a step from p along pk to minimum or a constraint ... */
    k=LSQPstep(ignore,Ain,b,&p1,p,&pk);   /* s is the constraint to include or -1 */
    mcopy(&p1,p); /* updating the parameter vector */
    if (k>-1) /* add a constraint to the working set and update Rf, Py and PX */
    { I[tk]=k;ignore[k]=1; /* keeping track of what's in working set */
      LSQPaddcon(Ain,&Q,&T,&Rf,&Py,&PX,&s,&c,k);tk++;
      if (delog[k]>maxdel)
      fixed[tk-1]=1;
      /*Rprintf("+");*/
    } else   /* it's a minimum - check lagrange multipliers */
    { k=LSQPlagrange(X,&Q,&T,p,&Xy,&p1,&y1,fixed,(int)Af->r);
      if (k>-1) /* then a constraint must be deleted */
      { LSQPdelcon(&Q,&T,&Rf,&Py,&PX,k+(int)Af->r);  /* the Af.r added to k ensures that correct row of T deleted */
        /*Rprintf("-");*/
        /* update the fixed constraint list */
        { for (i=k;i<tk-1;i++)
          fixed[i]=fixed[i+1];
        }
        tk--;
        if (k>-1) /* updating indexing arrays */
        { ignore[I[k]]=0;
          delog[I[k]]++;
          for (i=k;i<tk;i++) I[i]=I[i+1];
        }
      } else  /* routine has arrived at a minimum */
      { /* feasibility check..... */
        matmult(P,*Ain,*p,0,0);
        x=0.0;for (i=0;i<c.r;i++) if (P.V[i]-b->V[i]<x) x=P.V[i]-b->V[i];
        /*printf("P\n Worst feasibility violation %g",x);*/
        /* create Z - this version is a full null space matrix, rather than sequence of rotations */
        *Z=Q; Z->c -= tk;
        /* copy active constraint information to active  */
        active[0]=tk;
        for (i=0;i<tk;i++) active[i+1]=I[i]; 
        /* free memory */
        freemat(T);freemat(Rf);freemat(PX);freemat(Py);freemat(p1);freemat(y1);
        freemat(s);freemat(c);freemat(Xy);freemat(Pd);freemat(pz);freemat(pk);
        R_chk_free(I);R_chk_free(ignore);freemat(P);R_chk_free(fixed);R_chk_free(delog);
        /* return */
        return;
      }
    }
  }
}


void PCLS(matrix *X,matrix *p,matrix *y,matrix *w,matrix *Ain,matrix *b,
          matrix *Af,matrix *H,matrix *S,int *off,double *theta,int m,int *active)

/* Routine for Penalized Constrained Least Squares problems.
   PCLS() is an interface routine for QPCLS for solving the general problem class:

             minimise    ||W^0.5(Xp-y)||^2 + p'Bp
             subject to   Ain p >=b  &  Af p = "a constant vector"

   ...where B is a sum of m S[i] matrices multiplied by smoothing parameters
   theta[i]. The S[i]'s may be smaller than B (p->r by p->r) so S[i] is
   added to B starting at row and column off[i]. B must be non-negative
   definite, which means that the S[k]'s must be. W is the diagnoal matrix
   having w on the leading diagonal. In many applications the ith element of w
   will be the reciprocal of the variance associated with the ith element of i.

   The routine uses the fact that the problem can be re-written as....

     minimise || Fp - z ||^2 Subject to Ain p >= b Af p = constant

   ... where F = [ X'W^0.5, B^0.5']'  and z = [y'W^0.5, 0]'. This rewrite is
   performed and then QPCLS is called to obtain the solution.

   If H->r==y->r on entry, then an influence (or "hat") matrix is returned in H.
   At present the calculation of H is inefficient and none too stable.

   On exit active[] contains a list of the active inequlity constraints in elements 
   1->active[0]. This array should be initialized to length p.r+1 on entry.

   20/11/99

*/

{ int i,j,k;
  matrix z,F,W,Z,B,C;
  double x,xx;
 
  /* form transformed data vector z */
  if (m>0) z=initmat(y->r+p->r,1L);else z=initmat(y->r,1L);
  W=initmat(w->r,1L);
  for (i=0;i<y->r;i++) { W.V[i]=sqrt(w->V[i]);z.V[i]=W.V[i]*y->V[i];}
  /* form transformed design matrix X */
  F=initmat(z.r,p->r);
  /* first put in W^0.5X */
  for (i=0;i<X->r;i++) for (j=0;j<X->c;j++) F.M[i][j]=W.V[i]*X->M[i][j];
  /* add up the Penalties */
 
  if (m>0)
  { B=initmat(p->r,p->r);
    for (k=0;k<m;k++) for (i=0;i<S[k].r;i++) for (j=0;j<S[k].c;j++)
    B.M[i+off[k]][j+off[k]]+=theta[k]*S[k].M[i][j];
    /* and find a square root of B..... */

    root(&B,&C,8*DOUBLE_EPS);

    /* copy C' into the last p->r rows of F */
    for (i=0;i<C.r;i++) for (j=0;j<C.c;j++) F.M[j+X->r][i]=C.M[i][j];
    freemat(B);freemat(C);
  }
  /*  printf("\ncond(F)=%g",condition(F));*/
  /* Which means that the problem is now in a form where QPCLS can solve it.... */
  QPCLS(&Z,&F,p,&z,Ain,b,Af,active); /* note that at present Z is full not HH */
  if (H->r==y->r) /* then calculate the influence matrix XZ(Z'F'FZ)^{-1}Z'X'W */
  { freemat(W);W=initmat(Z.c,Z.c);
    multi(4,W,Z,F,F,Z,1,1,0,0);invert(&W); /* Wildly inefficient!! */
    multi(5,*H,*X,Z,W,Z,*X,0,0,0,1,1);      /* ditto */
    for (i=0;i<H->r;i++) for (j=0;j<H->c;j++) H->M[i][j]*=w->V[j];
  }
  /* working out value of objective at minimum */
  B=initmat(z.r,1L);matmult(B,F,*p,0,0);
  xx=0.0;for (i=0;i<z.r;i++) { x=B.V[i]-z.V[i];xx+=x*x;}
  /*printf("\nObjective at Minimum = %g\n",xx);*/ freemat(B);
  /* freeing storage .... */
  freemat(F);freemat(z);freemat(W);freemat(Z);
}



/***************************************************************************/
/* Update and bug fix notes.                                               */
/***************************************************************************/
/* 9/11/01 - This version cut down for use with mgcv
   21/5/02 - root finding in pcls() used zero for tolerance! fixed.
*/









