/* Copyright (C) 2008-2013 Simon N. Wood  simon.wood@r-project.org

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
USA. */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <R.h>
#include "mgcv.h"

/* Compute reproducing kernel for spline on the sphere */

void rksos(double *x,int *n,double *eps) {
/* Function to compute reproducing kernel for spline on the sphere,
   based on Jim Wendelberger's (1981) thesis. 
   Returns evaluated kernel rk(x) in n vector x.  
*/
  double dl1,xi,rk,xk,xx;
  int i,k;
  dl1 = acos(0)*2;
  dl1 = dl1*dl1/6; /* dilog(1) = pi^2/6, dilog(0)=0 */
  for (i=0;i< *n;i++) {
    xi = x[i];
    if (xi <= 0) {
      if (xi < -1) xi = -1;
      rk = 1.0 - dl1;
      xk = xi = xi/2 + 0.5;
      for (k=1;k<1000;k++) {
        xx = xk/(k*k);
        rk += xx;
        xk *= xi;
	if (xx < *eps) break;
      }
    } else {
      if (xi>1) xi=1;
      if (xi/2>=.5) rk=1.0; else
      rk = 1 - log(.5+xi/2)*log(.5-xi/2);
      xk = xi = .5 - xi/2;
      for (k=1;k<1000;k++) {
        xx = xk/(k*k);
        rk += -xx;
        xk *= xi;
	if (xk < *eps) break;
      } 
    }
    x[i] = rk;
  }
}


/* inside polygon tester.... */

void in_out(double *bx, double *by, double *break_code, double *x,double *y,int *in, int *nb, int *n)
/* finds out whether points in arrays x,y are inside boundary or outside, by counting boundary 
   crossings. The boundaries nodes are defined by bx, by.  bx[i] and by[i] less than or equal to 
   break_code signals a break in the boundary (e.g. between island and external boundary.) Each 
   section of boundary is assumed to be a closed loop. nb is dimenion of bx and by; n is dimension 
   of x and y. `in' will contain a 1 for an interior point and a 0 otherwise, on exit. 
   Both bx[i] and by[i] or neither must be less than the break_code.
*/ 
{ double xx,yy,dum,x0,x1,y0,y1;
  int i,j,count,start,swap;
  for (i=0;i<*n;i++) { /* loop through all test points */
      xx=x[i];yy=y[i]; /* the current test point */
      start=0; /* start of current boundary section */
      for (count=0,j=0;j<*nb;j++) { /* loop through entire boundary */
	x0 = bx[j]; /* start node */
        if (x0 <= *break_code) start=j+1; /* next segment start */
        else { /* not a new section start */
          if (j==*nb-1) x1=bx[start]; else x1 = bx[j+1];  /* end node */
          if (x1 <= *break_code) x1 = bx[start]; /* must join up segment end */
          if (x0!=x1) { /* x0==x1 => segment immaterial to decision */
	    if (x1<x0) { dum=x0;x0=x1;x1=dum;swap=1;} else swap=0; /* ordered */
            if (x0<xx&&x1>=xx) { /* might have a crossing */
	      y0 = by[j]; /* start node y co-ord */
              if (j==*nb-1) y1=by[start]; else
              y1 = by[j+1]; /* end node y co-ord */
              if (y1 <= *break_code) y1=by[start]; /* must join up */
              if (y0<=yy&&y1<=yy) count++; /* definite crossing */
              else { /* more detail needed to determine crossing */
		if (!(y0>yy&&y1>yy)) { /* could still be one */
                  if (swap) {dum=y0;y0=y1;y1=dum;}
                  dum = (xx-x0)*(y1-y0)/(x1-x0)+y0; /* at what y does vertical cross segment */
                  if (yy>=dum) count++; /* it's a crossing */
		} /* end - could still be one */
              } /* end - more detail */
            } /* end - might be a crossing */
          } /* end - does seg matter */
        } /* end - not skipped because break */
      } /* end boundary loop */
     if (count%2) in[i]=1;else in[i]=0; /* record result */
  } /* end x,y test loop */
} /* end of in_out */



/******************************/
/* Tweedie distribution stuff */
/******************************/

void psum(double *y, double *x,int *index,int *n) {
  /* y is of length max(index). x and index are of the same length, n.
     This routine fills y[index[i]-1] so that it contains the sum of 
     the x[i]'s sharing index[i]. It is assumed that y is cleared to zero
     on entry.
  */
  int i; 
  for (i=0;i< *n;i++) {
    y[index[i]-1] += x[i];
  }
}

double *forward_buf(double *buf,int *jal,int update)
/* extend buffer forward 1000 */
{ double *buf2,*p,*p1,*p2;
  int n=1000;
  buf2 = (double *)R_chk_calloc((size_t)*jal+n,sizeof(double));
  for (p=buf,p1=buf + *jal,p2=buf2;p<p1;p++,p2++) *p2 = *p;
  R_chk_free(buf);
  if (update) *jal += n;
  return(buf2);
}


double *backward_buf(double *buf,int *jal,int *j0,int *j_lo,int *j_hi,int update)
/* extend buffer backwards by 1000 elements, or to j=1 */
{ int n=1000;
  double *buf2,*p,*p1,*p2;
  if (n > *j0-1) n = *j0 - 1; /* only extend back to j=1 */
  if (n==0) return(buf);
  buf2 = (double *)R_chk_calloc((size_t)*jal+n,sizeof(double));
  for (p=buf,p1=buf + *jal,p2=buf2 + n;p<p1;p++,p2++) *p2 = *p;  
  if (update) {
    *jal += n;
    *j_lo += n;
    *j_hi += n;  
    *j0 -= n;
  }
  R_chk_free(buf);
  return(buf2);
}



void tweedious(double *w,double *w1,double *w2,double *y,double *phi,double *p,double *eps,int *n)
/* Routine to perform tedious series summation needed for Tweedie distribution
   evaluation, following Dunn & Smyth (2005) Statistics and Computing 15:267-280.
   Notation as in that paper. 
   
   log W returned in w. 
   d logW / dphi in w1, 
   d2 logW / d phi2 in w2.
 
   *Only* suitable for 1<p<2 (inequalities strict). 

   NOTE: still some redundancy for readability 
 
*/
{ int j_max,i,j_lo,j_hi,jb,jal,j0,j,ok;
  double x,ymax,ymin,alpha,*alogy,*p1,*p2,*p3,*wb,*wb1,*wb2,w_base,
    wmax,w1max,w2max,wmin,w1min,w2min,wi,w1i,w2i,
    log_eps,wj,w1j,w2j,jalogy;
  
  log_eps = log(*eps);
  alpha = (2 - *p)/(1 - *p);
  w_base = alpha * log(*p-1) - (1-alpha)*log(*phi) - log(2 - *p);

  /* initially establish the min and max y values, and hence the initial buffer range,
     at the same time produce the alpha log(y) vector. */ 
  
  alogy = (double *)R_chk_calloc((size_t)*n,sizeof(double));
  ymax = ymin = *y;
  *alogy = alpha * log(*y);
  for (p1=y+1,p2=y+ *n,p3=alogy+1;p1<p2;p1++,p3++) {
    *p3 = alpha * log(*p1); /* alogy[i] = alpha * log(y[i]) */
    if (*p1 > ymax) ymax = *p1; else if (*p1 < ymin) ymin = *p1;
  }
  
  x = pow(ymin,2 - *p)/(*phi * (2 - *p));
  j_lo = (int) floor(x);if (j_lo<1) j_lo = 1;
   
  x = pow(ymax,2 - *p)/(*phi * (2 - *p));
  j_hi = (int) ceil(x);if (j_hi<j_lo) j_hi = j_lo;
  
  j0 = j_lo - 1000;if (j0<1) j0=1;
  jal = j_hi + 1000;
  
  jal -= j0-1;
  j_lo -= j0;
  j_hi -= j0;

  wb = (double *)R_chk_calloc((size_t)jal,sizeof(double)); /* add -j*alogy[i] to get logW_j, for y[i] */
  wb1 = (double *)R_chk_calloc((size_t)jal,sizeof(double)); /* add -j*alogy[i] to get logW_j', for y[i] */
  wb2 = (double *)R_chk_calloc((size_t)jal,sizeof(double)); /* add -j*alogy[i] to get logW_j'', for y[i] */
  
  /* ... note that in the above it's log of derivative, not derivative of log */ 

  for (jb=j_lo,j=j_lo+j0;jb <= j_hi;jb++,j++) { /* jb is in buffer index, j is true index */ 
    wb[jb] = j * w_base - lgamma((double)j+1) - lgamma(-j * alpha);
    x = j*(alpha-1)/ *phi;
    wb1[jb] = wb[jb] + log(-x); /* note this is for log(-W_j')) */
    wb2[jb] = wb[jb]  + log(x*(x-1/ *phi));
  }

  /* Now j0 is the true j corresponding to buffer position 0. j starts at 1.
     jal is the number of buffer locations allocated. locations in the buffer between 
     j_lo and j_hi contain data. */



  for (i=0;i<*n;i++) { /* loop through y */
    /* first find the location of the series maximum... */
    x = pow(y[i],2 - *p)/(*phi * (2 - *p));
    j_max = (int) floor(x);
    if (x - j_max  > .5||j_max<1) j_max++; 
    j_max -= j0; /* converted to buffer index */
    
    j = j_max+j0;
    jalogy = j*alogy[i];
    wi=w1i=w2i=1.0;
    wmax = wb[j_max] - jalogy;wmin = wmax + log_eps; 
    w1max = wb1[j_max] - jalogy;w1min = w1max + log_eps;    
    w2max = wb2[j_max] - jalogy;w2min = w2max + log_eps;


    /* start upsweep to convergence or end of available buffered values */
    ok = 0;
    for (j=j_max+1+j0,jb=j_max+1;jb<=j_hi;jb++,j++) { 
      jalogy = j*alogy[i];
      wj = wb[jb] - jalogy;
      w1j = wb1[jb]  - jalogy;
      w2j = wb2[jb]  - jalogy;
      wi += exp(wj-wmax);
      w1i += exp(w1j-w1max);
      w2i += exp(w2j-w2max);
      if ((wj < wmin)&&(w1j < w1min)&&(w2j < w2min)) { ok=1;break;} /* converged on upsweep */
    } /* end of upsweep to buffer end */ 

    while (!ok) { /* while upsweep unconverged need to fill in more buffer */
      for (;jb<jal;jb++,j++) { /* fill buffers and calculate w terms */
        wb[jb] = j * w_base - lgamma((double)j+1) - lgamma(-j*alpha);
        x = j*(alpha-1)/ *phi;
        wb1[jb] = wb[jb] + log(-x);
        wb2[jb] = wb[jb] + log(x*(x-1/ *phi));
        jalogy = j*alogy[i];
        wj = wb[jb] - jalogy;
        w1j = wb1[jb]  - jalogy;
        w2j = wb2[jb]  - jalogy;
        wi += exp(wj-wmax);
        w1i += exp(w1j-w1max);
        w2i += exp(w2j-w2max);
        if ((wj < wmin)&&(w1j < w1min)&&(w2j < w2min)) { ok=1;break;} /* converged on upsweep */
        
      } 
      j_hi = jb; if (j_hi > jal-1) j_hi = jal-1; /* set j_hi to last element filled */
      if (!ok) { /* need to expand buffer storage*/
        wb = forward_buf(wb,&jal,0);
        wb1 = forward_buf(wb1,&jal,0);
        wb2 = forward_buf(wb2,&jal,1);
      }
    } /* finished upsweep and any buffer expansion */

    /* start downsweep to convergence or start of available buffered values */
    ok=0;
    for (j=j_max-1+j0,jb=j_max-1;jb>=j_lo;jb--,j--) { 
      jalogy = j*alogy[i];
      wj = wb[jb] - jalogy;
      w1j = wb1[jb]  - jalogy;
      w2j = wb2[jb]  - jalogy;
      wi += exp(wj-wmax);
      w1i += exp(w1j-w1max);
      w2i += exp(w2j-w2max);
      if ((wj < wmin)&&(w1j < w1min)&&(w2j < w2min)) { ok=1;break;} /* converged on downsweep */
    } /* end of downsweep to buffer end */ 
   
    if (j<=1&&j_lo==0) ok=1; /* don't care about element size if reached base */

    while (!ok) { /* while downsweep unconverged need to fill in more buffer */
      for (jb=j_lo-1;jb>=0;jb--,j--) { /* fill buffers and calculate w terms */
        wb[jb] = j * w_base - lgamma((double)j+1) - lgamma(-j*alpha);
        x = j*(alpha-1)/ *phi;
        wb1[jb] = wb[jb] + log(-x);
        wb2[jb] = wb[jb] + log(x*(x-1/ *phi));
        jalogy = j*alogy[i];
        wj = wb[jb] - jalogy;
        w1j = wb1[jb]  - jalogy;
        w2j = wb2[jb]  - jalogy;
        wi += exp(wj-wmax);
        w1i += exp(w1j-w1max);
        w2i += exp(w2j-w2max);
        if ((wj < wmin)&&(w1j < w1min)&&(w2j < w2min)) { ok=1;break;} /* converged on downsweep */
      } 
      if (j<=1) ok=1; /* don't care about element size if reached base */

      j_lo = jb; if (j_lo<0) j_lo=0; /* set j_lo to first element filled */
      if (!ok) { /* need to expand buffer storage*/
        wb = backward_buf(wb,&jal,&j0,&j_lo,&j_hi,0);
        wb1 = backward_buf(wb1,&jal,&j0,&j_lo,&j_hi,0);
        wb2 = backward_buf(wb2,&jal,&j0,&j_lo,&j_hi,1);
      }

    } /* finished downsweep and any buffer expansion */

    /* Summation now complete: need to do final transformations */
    w[i] = wmax + log(wi);    /* contains log W */
    w1i = w1max + log(w1i); /* contains log dW/dphi */
    w1[i] = -exp(w1i-w[i]);    /* d logW / d phi */
    w2[i] = w2max + log(w2i); /* contains log d2W/dphi2 */   
    w2[i] = exp(w2[i]-w[i]) - exp(2*w1i-2*w[i]); /* d2 logW / dphi2 */
 
  } /* end of looping through y */
  R_chk_free(alogy);R_chk_free(wb);R_chk_free(wb1);R_chk_free(wb2);
}


/* test code for tweedious...
library(mgcv);library(tweedie)
phi <- 2
p <- 1.1
mu <- .001
y <- c(1,1,2,1,3,0,0,30,67)
eps <- 1e-6
l0 <- colSums(mgcv:::ldTweedie(y,mu=mu,p=p,phi=phi))
l1 <- colSums(mgcv:::ldTweedie(y,mu=mu,p=p,phi=phi+eps))
  (l1-l0)/eps;l0

log(dtweedie(y,power=p,mu=mu,phi=phi))

j <- 1:100
alpha <- (2-p)/(1-p)
w <- -j*alpha*log(y)+alpha*j*log(p-1)-j*(1-alpha)*log(phi)-j*log(2-p)-lgamma(j+1) - lgamma(-j*alpha)
theta <- mu^(1-p)
k.theta <- mu*theta/(2-p)
theta <- theta/(1-p)  
(y*theta-k.theta)/phi - log(y) +  log(sum(exp(w)))

n <- 20
mu <- rep(1,n)
ml <- mgcv:::ldTweedie(1:n,mu,p=1.5,phi=1);ml
dl <- log(dtweedie.series(1:n,power=1.5,mu,phi=1));dl
x <- seq(.05,100,by=.1)
mu <- 1+x*0
sum(dtweedie(x,power=1.5,mu,phi=1))*.1 + dtweedie(0,power=1.5,1,phi=1)

sum(exp(mgcv:::ldTweedie(x,mu,p=1.5,phi=1)))*.1 + exp(mgcv:::ldTweedie(0,1,p=1.5,phi=1))


x <- rtweedie(10000,power=1.5,mu=1,phi=1)
  system.time(d1 <- dtweedie(x,power=1.5,mu=1,phi=1))
  system.time(d2 <- mgcv:::ldTweedie(x,mu=1,p=1.5,phi=1))
  range(d2-log(d1))

*/ 


/*******************************************************/
/** Fast re-weighting routines                         */
/*******************************************************/

void rwMatrix(int *stop,int *row,double *w,double *X,int *n,int *p) {
/* Function to recombine rows of n by p matrix X (column ordered).
   ith row of X' is made up of row[stop[i-1]...stop[i]], weighted by 
   w[stop[i-1]...stop[i]]. stop[-1]=0 by convention.
   stop is an n vector.     
   
   See rwMatrix in bam.r for call from R. 
*/
  int i,j,jump,start=0,end,off;
  double *X1p,*Xp,weight,*Xpe,*X1;
  /* create storage for output matrix, cleared to zero */
  X1 = (double *)R_chk_calloc((size_t)(*n * *p),sizeof(double));
  jump = *n;
  off = *n * *p;
  for (i=0;i<*n;i++) { /* loop through rows of output X1 */
    end = stop[i]+1;
    for (j=start;j<end;j++) { /* loop through the input rows */
      X1p = X1 + i;    /* pointer to start of row i of output */
      Xp = X + row[j]; /* pointer to start of source row */
      weight = w[j];   
      for (Xpe=Xp+off;Xp<Xpe;Xp+=jump,X1p+=jump) *X1p += weight * *Xp;
    }
    start = end;
  }
  /* coppy output to input for return...*/
  for (Xp=X,X1p=X1,Xpe=Xp+off;Xp<Xpe;Xp++,X1p++) *Xp = *X1p;
  R_chk_free(X1);
}

/* Example code for rwMatrix in R....
   n <- 10;p<-5
   X <- matrix(runif(n*p),n,p)
   ## create transform to take AR1(rho) to independence...
   stop <- c(1:(n-1)*2,2*n-1)
   row <- rep(1:n,rep(2,n))[-1]
   rho <- .7;ld <- 1/sqrt(1-rho^2);sd <- -rho*ld
   w <- c(rep(c(ld,sd),n-1),1)
   mgcv:::rwMatrix(stop,row,w,X)
   
*/

