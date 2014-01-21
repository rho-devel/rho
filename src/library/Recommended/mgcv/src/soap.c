/* Code for soap film smoothing. Copyright Simon Wood 2006-2012.

   R CMD SHLIB soap.c creates appropriate soap.so from this, can
   then be loaded by dyn.load("soap.so") and called with .C() */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <R.h>
#include "mgcv.h"

/****************************************************************************************************/
/* Boundary handling utilities from here on....                                                     */  
/****************************************************************************************************/


void boundary(int *G, double *d, double *dto, double *x0, double *y0, double *dx, double *dy,
              int *nx, int *ny, double *x, double *y,double *break_code, int *n, int *nb)

/* Function to create solution grid definition matrix G (nx by ny).
   Lower left cell centre is at x0, y0. cells are dx by dy. On entry 
   matrices d and dto are same dimension as G. 
   
   The boundary is supplied in n-arrays, `x' and `y'. Sub loops are separated by elements
   <= break_code. nb must have dimension of muber of loops.

   On exit:
   G[i,j] < - nx * ny is outside boundary, otherwise
   G[i,j] <= 0 is on boundary, and -G[i,j] indexes cell in d and g.
   G[i,j] > 0 indexes cell in g.

   On exit d contains the distances along the boundary, stored sequentially from 
   element 0 (i.e. d is a 1D array). nb contains the length of each boundary loop 
   in d (i.e. its cell count). 

   'g' refers to the solution grid itself, which will contain only interior and 
   boundary points.
 
   The boundary in x,y must be *strictly* within the outer grid cells. 

   `G' is stored column-wise (R default).

   `dto' is a working matrix containing distances from the boundary to the 
   cell centre. This is needed for judging which of multiple boundary segments 
   should supply the boundary value (the closest).

   The term `vertical' means parallel to y axis.

   The basic principle is that the boundaries between grid cells are 
   given by a set of evenly spaced horizontal and vertical lines. 
   It is easy to work out which lines are crossed by a boundary line 
   segment, and where this crossing occurs. Cells whose cell boundaries 
   are cut are treated as boundary cells.
 
*/ 
{ int segi,j,j0,j1,k,kk,i,reversed,*inb,*ip,*ip1,*ip2,bnd_count,ii,out_lim;
  double x1,y1,x2,y2,xb0,yb0,xl,yl,xc,yc,dist,dist_to,grad=0.0,b,len2,*p1,*p2;
 
  /* first step is to mark outside points in grid */
  
  p1 = d;p2 = dto;
  for (x1 = *x0,i=0;i<*nx;i++,x1 += *dx) {
    for (y1 = *y0,j=0;j<*ny;j++,y1 += *dy,p1++,p2++) {
      *p1 = x1;*p2 = y1; /* cell centres */
    }
  }
  k = *nx * *ny; /* total size of G, d, dto */
  out_lim = -k;
  inb = (int *)R_chk_calloc((size_t)k,sizeof(int));  
  in_out(x,y,break_code,d,dto,inb,n,&k); /* test all cell centres for in/out */
  j = -(k + 10);
  for (ip = inb,ip1 = G,p2 = dto,ip2=G+k;ip1<ip2;ip1++,p2++,ip++) {
    if (!*ip) *ip1 = j; else *ip1 = 1; /* set outside/inside in G */
    *p2 = -1.0; /* clear dto to -ve */
  }
  R_chk_free(inb);

  xb0 = *x0 - *dx/2;yb0 = *y0 - *dx/2; /* Refers to boundary lines lower left */ 
 
  dist = 0.0; /* distance along current boundary loop */
  bnd_count = 0; /* count number of boundary loops */
  nb[0] = 0; /* count cells in this boundary */
  ii = 0; /* counter for all boundary cells */
  for (segi=1;segi<*n;segi++) { /* loop through line segments */

    /* work through intersections with vertical boundary lines */
    if (x[segi-1]<x[segi]) {  /* get current segment */
      x1=x[segi-1];x2=x[segi]; 
      y1=y[segi-1];y2=y[segi];
      reversed = 0;
    } else {
      x1=x[segi];x2=x[segi-1]; 
      y1=y[segi];y2=y[segi-1]; 
      reversed = 1;    
    }
   
    j0 = (int) ceil( (x1 - xb0) / *dx); /* index of first vertical line after start */ 
    j1 = (int) floor( (x2 - xb0) / *dx);   /* index of last vertical line before end */
    
    if (x2-x1>0) grad = (y2-y1)/(x2-x1); else j1=j0-1;

    for (j=j0;j<=j1;j++) {      /* loop through intersected lines */
      xl = xb0 + j * *dx;       /* line location */
      yl = y1 + (xl - x1)*grad; /* y intersection location */
      k = (int) floor(( yl - yb0)/ *dy);
      /* so nodes j,k and (j-1),k are boundary nodes */
      kk = (j-1) * *ny + k;
      if (G[kk]>0||G[kk]< out_lim) { /* otherwise already a boundary cell */
        G[kk] = -ii;
        ii++;
        nb[bnd_count]++;
      }
      kk += *ny; /* j * *ny + k */
      if (G[kk]>0||G[kk]< out_lim) { /* otherwise already a boundary cell */
        G[kk] = -ii;
        ii++;
        nb[bnd_count]++;
      } 
   
      /* Now get the distance along/to the boundary */ 
     
      for (i=0;i<2;i++) { /* loop over the two cells concerned */
        xl = x2-x1;yl=y2-y1; 
        xc = (j-i) * *dx + *x0;
        yc = k * *dy + *y0;    
        xc -= x1;yc -= y1;   /* cell centre done */
        len2 = yl*yl + xl*xl;
        b = (xc*xl + yc*yl)/len2;
        xl = xl*b+x1;yl = yl * b + y1; /* location of projection from node to line */
        
        if (xl < x1) {xl = x1;yl = y1;}
        if (xl > x2) {xl = x2;yl = y2;} /* constrained to *within* segment */
         
        dist_to = sqrt((xl-xc)*(xl-xc) + (yl-yc)*(yl-yc));
        kk = (j-i) * *ny + k;
 
        if (dist_to < dto[kk] || dto[kk]<0) { 
          dto[kk] = dist_to;
          xl -= x1; yl -= y1;
          if (reversed) 
	    d[-G[kk]] = dist + sqrt(len2) - sqrt(xl*xl + yl*yl);
          else 
            d[-G[kk]] = dist + sqrt(xl*xl + yl*yl); /* distance along boundary */
	}
        
      }
    } /* end of vertical line processing */

    /* Now deal with horizontal lines */

     if (y[segi-1]<y[segi]) {  /* get current segment */
      x1=x[segi-1];x2=x[segi]; 
      y1=y[segi-1];y2=y[segi];
      reversed=0;
    } else {
      x1=x[segi];x2=x[segi-1]; 
      y1=y[segi];y2=y[segi-1];  
      reversed=1;   
    }
   
    j0 = (int) ceil( (y1 - yb0) / *dy); /* index of first horizontal line after start */ 
    j1 = (int) floor( (y2 - yb0) / *dy);   /* index of last horizontal line before end */

    if (y2-y1>0) grad = (x2-x1)/(y2-y1); else j1=j0-1;

    for (j=j0;j<=j1;j++) {      /* loop through intersected lines */
      yl = yb0 + j * *dy;       /* line location */
      xl = x1 + (yl - y1)*grad; /* y intersection location */
      k = (int) floor(( xl - xb0)/ *dx);
      /* so nodes k,j and k, (j-1) are boundary nodes */
      
      kk = k * *ny + j - 1;
      if (G[kk]>0||G[kk]< out_lim) {G[kk] = -ii;ii++;nb[bnd_count]++;} /* otherwise already a boundary cell */
      kk ++; /* k * *ny + j */
      if (G[kk]>0||G[kk]< out_lim) {G[kk] = -ii;ii++;nb[bnd_count]++;} /* otherwise already a boundary cell */
      
      /* Now get the distance along/to the boundary */ 
     
      for (i=0;i<2;i++) { /* loop over the two cells concerned */
        xl = x2-x1;yl=y2-y1;
        yc = (j-i) * *dy + *y0;
        xc = k * *dx + *x0;    
        xc -= x1;yc -= y1;   /* cell centre done */
        len2 = yl*yl + xl*xl;
        b = (xc*xl + yc*yl)/len2;
        xl = xl*b+x1;yl = yl * b + y1; /* location of projection from node to line */
        
        if (yl < y1) {xl = x1;yl = y1;}
        if (yl > y2) {xl = x2;yl = y2;} /* constrained to *within* segment */
         
        dist_to = sqrt((xl-xc)*(xl-xc) + (yl-yc)*(yl-yc));
        kk = k * *ny + j-i;
 
        if (dist_to < dto[kk] || dto[kk]<0) { 
          dto[kk] = dist_to;
          xl -= x1; yl -= y1;
          if (reversed) 
	    d[-G[kk]] = dist + sqrt(len2) - sqrt(xl*xl + yl*yl);
          else 
            d[-G[kk]] = dist + sqrt(xl*xl + yl*yl); /* distance along boundary */
	}
        
      }
    } /* end of horizontal line processing */

    /* update `dist' */
    x2 = x2-x1;y2=y2-y1;
    dist += sqrt(x2*x2+y2*y2);
    /* now look ahead to see if we are at the end of a sub-loop */
    if (segi < *n - 1 && x[segi+1] <= *break_code) { /* reached segment end */
      dist = 0.0; /* reset for new loop */
      segi++;segi++; /* move past the break */
      bnd_count++; /* loop counter */
      if (segi < *n) nb[bnd_count] = 0; /* set cell counter for this loop */
    }    
  } /* end of line segment loop */
  
  /* Clear the remainder of d to -ve */
  k = *nx * *ny;for (i=ii;i<k;i++) d[i] = -1;
  
  /* Set remaining elements of G */
  for (ip1 = G,ip2 = G + k;ip1<ip2;ip1++) if (*ip1 > 0) {*ip1 = ii;ii++;} 

} /* end of boundary */

void pde_coeffs(int *G,double *x,int *ii,int *jj,int *n,int *nx,int *ny,double *dx,double *dy) {
/* Takes nx by ny grid G produced by function boundary, and produces corresponding PDE
   coefficient matrix, for soap PDEs in sparse triplet form.
   On entry x, ii and jj should be of length 5 times the number of cells 
   within the boundary. On exit n will contain their exact required length.

*/ 
  int i,j,*ip,outside,Gk0,Gk1,k0,k1;
  double xc,dx2,dy2,thresh=0.0;
  thresh = dx2= 1.0/(*dx * *dx);dy2 = 1.0/(*dy * *dy);
  if (dy2 < thresh) thresh = dy2;thresh *= .5;
  outside = - *nx * *ny - 1;
  *n=0; 
  for (ip=G,i=0;i<*nx;i++) for (j=0;j<*ny;j++,ip++) if (*ip > outside){
    if (*ip <= 0) { /* boundary cell */
      *x=1.0;*jj = *ii= - *ip;
      x++;ii++;jj++;*n += 1;
    } else { /* interior */
      xc=0.0; /* diagonal coefficient */
      if (i>0&&i< *nx-1) { /* FD w.r.t. x may be possible */ 
         k0 = (i-1) * *ny + j; /* backwards diff */
         k1 = k0 + 2 * *ny;    /* forwards diff */
         Gk0 = G[k0];Gk1 = G[k1];
         if (Gk0 > outside && Gk1 > outside) { /* difference is possible */
           xc += 2*dx2;
           if (Gk0<0) Gk0 = -Gk0;
           *x = -dx2;*ii = *ip;*jj = Gk0;
           x++;ii++;jj++;*n += 1;
           if (Gk1<0) Gk1 = -Gk1; 
           *x = -dx2;*ii = *ip;*jj = Gk1;
           x++;ii++;jj++;*n += 1;
         }
      } /* FD in x direction finished */
      if (j>0&&j< *ny-1) { /* FD w.r.t. x may be possible */ 
         k0 = i * *ny + j - 1; /* backwards diff */
         k1 = k0 + 2;    /* forwards diff */
         Gk0 = G[k0];Gk1 = G[k1];
         if (Gk0 > outside && Gk1 > outside) { /* difference is possible */
           xc += 2*dy2;
           if (Gk0<0) Gk0 = -Gk0;
           *x = -dy2;*ii = *ip;*jj = Gk0;
           x++;ii++;jj++;*n += 1;
           if (Gk1<0) Gk1 = -Gk1; 
           *x = -dy2;*ii = *ip;*jj = Gk1;
           x++;ii++;jj++;*n += 1;
         }
         if (xc > thresh) { /* there is a difference for this cell */
           *x = xc;*ii = *jj = *ip;
           x++;ii++;jj++;*n += 1;
         }
      }
    } /* interior branch end*/
  } /* main loop end */ 
} /* end of pde_coeffs */


void gridder(double *z,double *x,double *y,int *n,double *g, int *G,int *nx, int *ny,double *x0, 
             double *y0,double *dx,double *dy,double NA_code) {
/* Takes solution g indexed by ny by nx matrix G. lower left cell of G is centred at x0, y0 and 
   cell sizes are dx by dy. 

   Interpolates solution to n locations in x, y, returning NA code for out of area. 
   Does not do strict boundary testing here, since this routine is often called several
   times with same geometry. 
*/
  int i,ix,iy,ok,Gthresh,Gk,k,ok00,ok01,ok10,ok11;
  double xx,yy,xx0,yy0,dmax,xa,ya,g00=0.0,g01=0.0,g10=0.0,g11=0.0,b0,b1,b2,b3,dist,d1;
  dmax = (*dx * *dx + *dy * *dy)*2;
  xx0 = *x0;yy0 = *y0;
  Gthresh = - *nx * *ny; /* G below with implies out of area */
  for (i=0;i < *n;i++) { /* loop through x,y locations */
    xx = x[i];yy = y[i];
    ix = (int) floor((xx - xx0) / *dx);
    iy = (int) floor((yy - yy0) / *dy);
    k = ix * *ny + iy;
    ok = 0; 
    /* node 00... */
    if (ix<0||ix>=*nx||iy<0||iy>=*ny) ok00 = 0; else { 
      Gk=G[k];
      if (Gk < Gthresh) ok00 = 0; else {
	ok00 = 1; ok++;
	if (Gk < 0) Gk = -Gk;
        g00 = g[Gk];
      }
    } /* end of node 00 */
    iy++;k++; /* node 01 */   
    if (ix<0||ix>=*nx||iy<0||iy>=*ny) ok01 = 0; else { 
      Gk=G[k];
      if (Gk < Gthresh) ok01 = 0; else {
	ok01 = 1; ok++;
	if (Gk < 0) Gk = -Gk;
        g01 = g[Gk];
      }
    } /* end of node 01 */
    ix++; k += *ny; /* node 11 */
    if (ix<0||ix>=*nx||iy<0||iy>=*ny) ok11 = 0; else { 
      Gk=G[k];
      if (Gk < Gthresh) ok11 = 0; else {
	ok11 = 1; ok++;
	if (Gk < 0) Gk = -Gk;
        g11 = g[Gk];
      }
    } /* end of node 11 */
    iy--;k--; /* node 10 */
    if (ix<0||ix>=*nx||iy<0||iy>=*ny) ok10 = 0; else { 
      Gk=G[k];
      if (Gk < Gthresh) ok10 = 0; else {
	ok10 = 1; ok++;
	if (Gk < 0) Gk = -Gk;
        g10 = g[Gk];
      }
    } /* end of node 10 */
    ix--;

    if (ok==4) { /* all nodes are ok, full bilinear */
      b0 = g00;
      b1 = (g10-g00) / *dx;
      b2 = (g01-g00) / *dy;
      b3 = (g11-g10-g01+g00)/( *dx * *dy);
      xx = xx - xx0 - ix * *dx;
      yy = yy - yy0 - iy * *dy;
      /* evaluate interpolating polynomial */
      z[i] = b0 + b1 * xx + b2 * yy + b3 * xx * yy; 
   
    } else if (!ok) { /* no good neighbours - NA */
      z[i] = NA_code;
    } else { /* resort to nearest neighbour */
      xa = xx - xx0 - ix * *dx;
      ya = yy - yy0 - iy * *dy;
      dist = dmax;
      if (ok00) {  
        dist = xa*xa + ya*ya;
        z[i] = g00;
      }
      if (ok01) {
        ya = *dy - ya;
	d1 = xa*xa + ya*ya;
        if (d1 < dist) {
          dist=d1;
          z[i] = g01;
        }
      }
      if (ok11) {
        xa = *dx - xa;
	d1 = xa*xa + ya*ya;
        if (d1 < dist) {
          dist=d1;
          z[i] = g11;
        }
      }
      if (ok10) {
        ya = *dy - ya;
	d1 = xa*xa + ya*ya;
        if (d1 < dist) {
          z[i] = g10;
        }
      }
    } /* end of nearest neighbour */
  }
} /* end of gridder */
