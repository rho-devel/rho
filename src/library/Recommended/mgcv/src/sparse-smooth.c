/* Copyright Simon N. Wood, 2011-13

   Code to implement kd tree based nearest neighbour routines in C.
   Based on approach of Press et al Numerical Recipes 3rd ed. 21.2, but 
   re-implemented in vanilla C. Design is based on principles from Press
   et al. but due to licensing, this is a re-write. So results need not
   correspond to Press et al. code in detail (e.g. exactly which point 
   is in which box of tree, and exact indexing details). Efficiency 
   should be same, however.

   R CMD SHLIB kd-tree.c 
   to build.
   dyn.load("kd-tree.so")
   to load into R.

*/

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <math.h>
#include <stdlib.h>
#include "mgcv.h"

/* 
  kd-tree tasks:
  1. Build and return kd tree.
  2. Find nearest neighbour of points x in given kd tree.
  3. Find k nearest neighbours of points x in given kd tree.
  4. Build kd tree, compute k nearest neighbours for all nodes,
     return these, and optionally tree.
  5. Find all points in given tree within r-ball of each point 
     in x. 


  key routines:
   * kd_tree and free_kdtree for creating and freeing kd trees.
   * closest - find closest point in kd tree to a new point x.
   * k_nn_work finds k nearest neighbours of each node in kd tree
   * k_nn forms kd tree and then obtains k nearest neighbours
  
   * kd_sizes, kd_dump, kd_read are concerned with encoding 
     kd tree in form suitable for storage in R and reading 
     from this format.
 
   needed: 
   * k_closest - find k nearest neighbours in kd tree to points not 
                 in kd tree.
   * r_ball - find points in kd tree within  r-balls around points 
              not in kd tree. 

*/

typedef struct { /* defines structure for kd-tree box */
  double *lo,*hi;    /* box defining co-ordinates */
  int parent,child1,child2, /* indices of parent and 2 offspring */
      p0,p1;         /* indices of first and last point in box */
} box_type; 



typedef struct {
  box_type *box;
  int *ind, /* index of points in coordinate matrix which tree relates to */
      *rind, /* where is ith row of X in ind? */
      n_box, /* number of boxes */
      d, /* dimension */
    n; /* number of points that tree relates to */
  double huge; /* number indicating an open boundary */
} kdtree_type;


void kd_sizes(kdtree_type kd,int *ni,int *nd) {
/* reports size of integer array and double array (ni and nd)
   required to hold full kd tree in packed storage for passing 
   back to R */
  *nd = 1 + kd.d * kd.n_box * 2; /* to hold huge, lo and hi data for boxes */
  *ni = 2 + /* n_box and d */
    2 * kd.n + /* ind, rind */ 
    5 * kd.n_box; /* parent,child1,child2,p0,p1*/        
}

void kd_dump(kdtree_type kd,int *idat,double *ddat) {
/* writes a kdtree structure to arrays idat and ddat, initialized
   to the sizes determined by kd_sizes for kd. The point is that
   these are suitable for passing to R, say. */
  int *p,*p0,*p1,i,nb,d,*pc1,*pc2,*pp,n;
  double *pd,*pd1;
  nb = idat[0] = kd.n_box; /* number of boxes */
  d = idat[1] = kd.d; /* dimension of boxes/points */
  n = idat[2] = kd.n; /* number of points tree relates to */
  *ddat = kd.huge;ddat++;
  /* copy kd.ind... */
  for (p=idat+3,p0=kd.ind,p1=p0+n;p0<p1;p++,p0++) *p = *p0;
  /* copy kd.rind... */
  for (p0=kd.rind,p1=p0+n;p0<p1;p++,p0++) *p = *p0;
  /* now work through boxes dumping contents */
  pp = idat + 3 + 2 * n; /* parents */
  pc1 = pp + nb; /* child1 */
  pc2 = pc1 + nb; /* child2 */
  p0 = pc2 + nb; /* p0 */
  p1 = p0 + nb; /* p1 */
  for (i=0;i<nb;i++) {
    /* copy d array kd.box[i].lo... */
    for (pd=kd.box[i].lo,pd1=pd+d;pd<pd1;pd++,ddat++) *ddat = *pd;
    /* copy  kd.box[i].hi... */
    for (pd=kd.box[i].hi,pd1=pd+d;pd<pd1;pd++,ddat++) *ddat = *pd;
    *pp = kd.box[i].parent;pp++;
    *pc1 = kd.box[i].child1;pc1++;
    *pc2 = kd.box[i].child2;pc2++;
    *p0 = kd.box[i].p0;p0++;
    *p1 = kd.box[i].p1;p1++;
  }
}

void kd_read(kdtree_type *kd,int *idat,double *ddat) {
/* creates a kd tree from the information packed into idat and ddat by
   kd_dump. Note that ind, rind, and kd.box[0].lo should not be freed 
   when freeing this structure, as no storage is allocated for these.
   Only kd.box, should be freed!!

   Point of this is that kd_dump can be used to export structure to R for 
   storage, and this routine then reads in again.
*/
  int nb,d,*pp,*pc1,*pc2,*p0,*p1,i,n;
  box_type *box;
  nb = kd->n_box = idat[0]; /* number of boxes */
  d = kd->d = idat[1]; /* dimensions of boxes etc. */
  n = kd->n = idat[2]; /* number of points tree relates to */
  kd->ind = idat + 3;
  kd->rind = idat + 3 + n;
  kd->huge = *ddat;ddat++;
  /* Now make an array of boxes (all cleared to zero)... */
  kd->box = (box_type *)R_chk_calloc((size_t)nb,sizeof(box_type));
  /* now work through boxes loading contents */
  pp = idat + 3 + 2*n; /* parents */
  pc1 = pp + nb; /* child1 */
  pc2 = pc1 + nb; /* child2 */
  p0 = pc2 + nb; /* p0 */
  p1 = p0 + nb; /* p1 */
  box = kd->box;
  for (i=0;i<nb;i++,box++) {
    box->lo = ddat;ddat += d;
    box->hi = ddat;ddat += d;
    box->parent = *pp;pp++;
    box->child1 = *pc1;pc1++;
    box->child2 = *pc2;pc2++;
    box->p0 = *p0;p0++;
    box->p1 = *p1;p1++;
  }
}


void kd_sanity(kdtree_type kd) {
  int ok=1,i,*count,n=0;
  for (i=0;i<kd.n_box;i++) if (kd.box[i].p1>n) n = kd.box[i].p1;
  count = (int *)R_chk_calloc((size_t)n,sizeof(int));
  for (i=0;i<kd.n_box;i++) if (!kd.box[i].child1) { /* terminal node */
    if (kd.box[i].p1-kd.box[i].p0>1) { Rprintf("More than 2 points in a box!!\n");ok=0;}
    count[kd.box[i].p0]++;
    if (kd.box[i].p1!=kd.box[i].p0) count[kd.box[i].p1]++;
  }
  for (i=0;i<n;i++) {
    if (count[i]!=1) { Rprintf("point %d in %d boxes!\n",i,count[i]);ok=0;}
  }
  if (ok) Rprintf("kd tree sanity checks\n");
  R_chk_free(count);
}

void k_order(int *k,int *ind,double *x,int *n) {
/* ind is of length n.
   x is of length max(ind)+1, at least.
   On exit x[ind[0..k-2]] <= x[ind[k-1]] <= x[ind[k..n-1]],
   by permuting ind, but not x. i.e. we split x into two 
   groups... those less than or equal to the kth largest, and 
   those greater than or equal to the kth largest.

   This works by picking an arbitrary pivot value from x, and arranging 
   ind so that x[ind] is partitioned about that value. Now we know 
   immediately which of the two partitions must contain the kth 
   largest value, so we repeat the process on this one. Eventually 
   the partition that must contain the kth largest value has only 1 
   or 2 elements in it... 

   R test code...
   dyn.load("kd-tree.so")
   n <- 20
   x <- runif(n)
   k <- 1
   oo <- .C("k_order",k=as.integer(k),ind=as.integer(0:19),x=as.double(x),n=as.integer(n))
   ind <- oo$ind+1
   sort(x)[k]
   x[ind[k]]
   ## following is TRUE if it's worked...
   sum(!c(x[ind[1:(k-1)]]<=x[ind[k]],x[ind[(k+1):n]]>=x[ind[k]]))==0

*/ 

  int l,r,m,ip,ri,li,dum;
  double xp;
  l = 0;   /* leftmost point of current partition */
  r = *n-1; /* rightmost point of current partitions */
  while (1) {
    if (r > l+1) { /* partition large enough to need work still */
      m = (l+r) / 2; /* pick a point from partition midpoint (by location not value) 
                        (Press et al say to do this to avoid poor behaviour on already 
                         sorted x).*/ 
      dum = ind[l+1];ind[l+1] = ind[m];ind[m] = dum; /* swap points m and l+1 */
      /* now re-arrange so that x[ind[l]] < x[ind[l+1]] < x[ind[r]]... */
      if (x[ind[l]] > x[ind[r]]) { /* swap r and l */
        dum = ind[r];ind[r] = ind[l];ind[l] = dum;
      }
      if (x[ind[l]] > x[ind[l+1]]) { /* swap l and l+1 */
        dum = ind[l];ind[l] = ind[l+1];ind[l+1] = dum;
      } 
      else if (x[ind[l+1]] > x[ind[r]]) { /* swap l+1 and r */
        dum = ind[l+1];ind[l+1] = ind[r];ind[r] = dum;
      }
      ip = ind[l+1]; /* index of pivot */
      xp = x[ip];    /* pivot value */
      /* so pivot is xp = x[ind[l+1]]. start proccess of shuffling array into
         two partitions containing all the values less than xp, and all 
         those larger than xp... */ 
      ri = r;  /* start searching down partition from here for wrongly located 
                    values (pos r above pivot already) */
      li = l+1; /* start searching up from here (pos l is already below pivot, l+1 is pivot)*/
      while (1) {
	/* BUG: can get stuck in here, when there are tied values, so that li and ri
	   stay unmodified, but ri > li... changing to <= >= allows ri and li to 
           move out of [0,n], which causes segfault!*/
        li++;ri--; /* always move by one, or you can get stuck */
        while(x[ind[li]] < xp) li++; /* move up until value on wrong side (or equal) found */
        while(x[ind[ri]] > xp) ri--; /* move down until value on wrong side (or equal) found */
        if (ri < 0) Rprintf("ri<0!!\n");
        if (li >= *n) Rprintf("li >= n!!\n");       
        if (ri<li) break; /* partitions correct now */
        dum = ind[ri];ind[ri] = ind[li];ind[li] = dum; /* swap ri and li (to correct sides) */
      } /* end of partitioning loop */
      /* now put pivot into right place (and value in that place in correct partition) */
      ind[l+1] = ind[ri];
      ind[ri] = ip;
      /* Now select the partition in which kth largest must lie, by setting new end points */
      if (ri >= *k ) r = ri - 1; /*else l=li;*//* if (ri <= *k + 1)  l = li;*/ /* had else l=li; here */
      if (ri <= *k ) l = li; 
   } else { /* the partition can only contain 1 or 2 points */
      if (r == l+1 && x[ind[r]] < x[ind[l]]) { /* contains two points, but in wrong order */
         dum = ind[r];ind[r] = ind[l];ind[l] = dum; /* so swap indices */
      } 
      return; /* x[ind[k]] is kth largest value in x */ 
    }
  } /* end while(1) - main loop */
}

void free_kdtree(kdtree_type kd) {
/* free a kdtree. Only use for tree created entirely from complied code,
   not one read from R. For R only versions R_chk_free(kd.box) is all 
   that is needed, as rest uses memory sent in from R.*/
  R_chk_free(kd.ind);R_chk_free(kd.rind);
  R_chk_free(kd.box[0].lo); /* storage for box coordinates */
  R_chk_free(kd.box);
}

void kd_tree(double *X,int *n, int *d,kdtree_type *kd) {
/* Create a kd tree for the points in n by d matrix X.
   X is in column order. Each row is one point. 
   At end of process... 
   * box[i] contains points indexed by 
     ind[box[i].p0..box[i].p1] 
   * box[i] has one parent and 2 children, unless it contains only one 
     or 2 points, in which case it has no children.

   
*/
  int *ind,*rind,*p,i,m,todo[50],todo_d[50],item,bi,nb,np,k,dim,b,p0,p1;
  box_type *box;
  double huge=1e100,*pd,*x,*dum1,*dum2,*dum3;
  /* create index for points... */
  ind = (int *)R_chk_calloc((size_t) *n,sizeof(int)); 
  for (i=0,p=ind;i < *n;i++,p++) *p = i; 
  /* Find the number of boxes in the tree */
  m=2;while (m < *n) m *= 2;
  nb = *n * 2 - m / 2 - 1;
  if (nb > m-1) nb = m - 1; 
  /* Now make an array of boxes (all cleared to zero)... */
  box = (box_type *)R_chk_calloc((size_t)nb,sizeof(box_type));
  /* allocate storage for box defining coordinates... */ 
  pd = (double *)R_chk_calloc((size_t)nb * (2 * *d),sizeof(double));
  for (i=0;i<nb;i++) {
    box[i].lo = pd;pd += *d;
    box[i].hi = pd;pd += *d;
  } /* note box[0].lo is now the thing to free when done (not p!) */

  /* set up box[0]... */
  for (i=0;i< *d;i++) { /* arbitrary bounding box */
    box[0].lo[i] = -huge;box[0].hi[i] = huge;
  }
  box[0].p1 = *n-1; /* last index item in this box (.p0 is first) */
  todo[0]=0;   /* put box[0] on todo list for processing */ 
  todo_d[0]=0; /* which dimension to start with */
  item=0;      /* which item of todo list to do next (item+1 is number of items on list) */
  /* each box contains 1 or 2 points, or it gets split into
     2 smaller boxes. If the smaller boxes contain 3 or more 
     points then they are added to the todo list. The todo 
     list is always worked on from the end (i.e by processing 
     box todo[item]) */
  bi=0; /* box index, counting number of boxes created so far */
  while (item >= 0) {   /* todo list still has items */
    b = todo[item];     /* current box */
    dim = todo_d[item]; /* dimension on which to split box */ 
    p0 = box[b].p0;p1=box[b].p1; 
    np = p1-p0+1;      /* number of points in box k */
    x = X + dim * *n;  /* array of co-ordinates for current dimension to sort on */
    k = (np-1)/2;          /* split the box around kth value in box */ 
    /* next line re-orders the point index for this box only.
       after reordering the index is split into two parts, indexing
       points below and above the kth largest value */  
 
    k_order(&k,ind+p0,x,&np); 
  
    /*... so the box is now split at a plane/line through x[ind[p0+k-1]] */
    item--; /* basically done that item */
    /* create the offspring boxes... */
    
    bi++; /* lower box first */ 
    
    if (bi>nb-1) Rprintf("too many boxes!!");
    box[b].child1=bi;/* record box relationships */
    /* copy box coordinates... */
    for (dum1=box[bi].lo,dum2=dum1 + *d,dum3=box[b].lo;dum1<dum2;dum1++,dum3++) *dum1 = *dum3;
    for (dum1=box[bi].hi,dum2=dum1 + *d,dum3=box[b].hi;dum1<dum2;dum1++,dum3++) *dum1 = *dum3;
    box[bi].hi[dim] = x[ind[p0+k]]; /* split location */
    box[bi].parent=b; /* record box relationships */
    box[bi].p0=box[b].p0;
    box[bi].p1=box[b].p0+k;
    if (k>1) { /* more than two points , so more work needed */ 
      item++;
      todo[item] = bi;
      todo_d[item] = dim+1;
      if (todo_d[item] == *d) todo_d[item] = 0;
    }
    bi++; /* now the higher box */ 
    if (bi>nb-1) Rprintf("too many boxes!!");
    box[b].child2=bi;/* record box relationships */
    /* copy box coordinates... */
    for (dum1=box[bi].lo,dum2=dum1 + *d,dum3=box[b].lo;dum1<dum2;dum1++,dum3++) *dum1 = *dum3;
    for (dum1=box[bi].hi,dum2=dum1 + *d,dum3=box[b].hi;dum1<dum2;dum1++,dum3++) *dum1 = *dum3;
    box[bi].lo[dim] = x[ind[p0+k]]; /* split location */
    box[bi].parent=b; 
    box[bi].p1=box[b].p1;
    box[bi].p0=box[b].p0+k+1;
    if (np-k>3) { /* more than two points , so more work needed */ 
      item++;
      todo[item] = bi;
      todo_d[item] = dim+1;
      if (todo_d[item] == *d) todo_d[item] = 0;
    }
  }  
  if (bi!=nb-1) Rprintf("bi not equal to nb-1 %d %d\n",bi,nb-1);
  rind = (int *)R_chk_calloc((size_t) *n,sizeof(int));
  /* now create index of where ith row of X is in ind */
  for (i=0;i<*n;i++) rind[ind[i]]=i; 
  /* now put tree into kd object */
  kd->box = box;kd->ind = ind;kd->rind = rind;kd->n_box = nb;kd->huge = huge;
  kd->d = *d;kd->n = *n;
} /* end of kd_tree */


void Rkdtree(double *X,int *n, int *d,int *idat,double *ddat) { 
/* Routine to export kdtree data to R 
     m <- 2;
     while (m<n) m <- m*2
     nb <- min(m-1,2*n-m/2-1)
     see rd_dump for structure of idat and ddat
     * input:
       X is n by d matrix, each row a point.
     * output
       ddat is an nb*2*d vector
       idat is a 2 + 7*nb vector
       - together they encode the kd tree 

*/
  kdtree_type kd;
  kd_tree(X,n,d,&kd); /* create kd tree */
  kd_dump(kd,idat,ddat); /* dump it to idat,ddat */
  free_kdtree(kd); /* free structure */
}


void update_heap(double *h,int *ind,int n) {
/* h contains n numbers, such that h[i] > h[2*i+1] and
   h[i] > h[2*i+2] (each applying whenever elements exist).
   The exception is that h[0], may not obey these conditions. 
   This function re-arranges h so that it does. It also 
   applies the same re-arrangement to ind. Figure 8.3.1
   of Press et al (2007) shows what's going on.
*/
  double h0;
  int i,i0,ind0;
  h0 = h[0]; /* h0 should be largest element, in properly ordered heap */
  ind0 = ind[0];  /* index vector to re-shuffle exactly as h vector */
  i0 = 0; /* current position of h0 */
  i = 1; /* index for first child node of i0 */
  while (i < n) { /* work through to end of heap */
    if (i < n-1&&h[i]<h[i+1]) i++; /* i indexes largest of i0's child nodes */ 
    if (h0 > h[i]) break; /* h0 should be at h[i0] */
    /* since h0 <= h[i], move h[i] 'up' heap into h[i0], and move i0, the nominal 
       position for h0 'down' heap to i */
    h[i0] = h[i];
    ind[i0] = ind[i];
    i0 = i;
    i = 2*i+1; /* now move on to first child of h[i]... */
  }
  h[i0] = h0; /* put h0 into location it should occupy in heap */
  ind[i0] = ind0;
}

double box_dist(box_type *box,double *x,int d) {
/* find distance from d dimensional box to point x */
  double d2 = 0.0,z,*bl,*bh,*xd;
  for (xd=x+d,bl=box->lo,bh=box->hi; x < xd;x++,bl++,bh++) {
    if (*x < *bl) { z = *x - *bl;d2 += z*z;}
    if (*x > *bh) { z = *x - *bh;d2 += z*z;}
  } 
  return(sqrt(d2));
}

 
int which_box(kdtree_type *kd,int j) {
/* Finds smallest box in kd tree containing jth point 
   from point set used to create tree */ 
  int i,bi,b1;
  i = kd->rind[j]; /* where jth point is in kd->ind */
  bi=0;
  
  while (kd->box[bi].child1) { /* still haven't reached smallest */
   
    b1 = kd->box[bi].child1;   /* index of first child */
    if (kd->box[b1].p1>=i) bi = b1; /* point is in child1 */
    else bi = kd->box[bi].child2; /* kd->box[bi].child1 must be in child2 */
  }
  return(bi); /* index of smallest box containing jth point */
}


int xbox(kdtree_type *kd,double *x) {
/* which box of the kd tree is point x located in? 
   For maximal efficiency use the fact that nested boxes are
   split along one dimension, and that the split dimensions are
   cycled through in the same order, while descending the tree. 
*/
  int bi,d,b1;
  box_type *box;
  bi=0; /* root of the tree - the big box */
  box = kd->box;
  d=0;  /* dimension for first split */
  while (box[bi].child1) { /* still not reached the outermost twig - smallest box*/
    b1 = box[bi].child1;
    if (box[b1].hi[d]!=box[box[bi].child2].lo[d]) Rprintf("child boundary problem\n");
    /* note that points on boundary are in lower box (child1) */
    if (x[d] <= box[b1].hi[d]) bi = b1; else 
    bi = box[bi].child2;
    d++; if (d == kd->d) d=0;
  }
  return(bi);
}


double ijdist(int i, int j, double *X,int n,int d) {
/* return Euclidian distance between ith and jth rows of n by d 
   matrix X */
  double *pi,*pj,*pil,dist=0.0,x;
  for (pi=X+i,pil=pi+n*d,pj=X+j;pi<pil;pi+=n,pj+=n) {x = *pi - *pj;dist += x*x;} 
  return(sqrt(dist));
} 

double xidist(double *x,double *X,int i,int d, int n) {
/* distance between point x and point in ith row of X */
  double dist=0.0,z;
  int j;
  for (j=0;j<d;j++) { 
    z = x[j] - X[j*n+i];
    dist += z*z;
  }
  return(sqrt(dist));
}


int closest(kdtree_type *kd, double *X,double *x,int n,int *ex,int nex) {
/* Find the point in the kd tree which is closest to the point
   with co-ordinates given by x. kd->d is dimension. n is number
   of rows in X. rows of X are points in tree. 
   if nex>0 then ex is a list of points to exclude.
*/
  int bx,ni,i,j,k,d,todo[100],bi,*ind,item,ok=0;
  double nd,d1,dix; 
  box_type *box;
  if (nex<0) nex=0;  
  nd = kd->huge; 
  bx = xbox(kd,x); /* box containing x */
  /* get closest point within that box */
  d = kd->d;
  box = kd->box;
  ind = kd->ind;
  ni = -1;
  while (ni<0) { /* open larger boxes until one contains a non-excluded neighbour */
    for (j=box[bx].p0;j<box[bx].p1;j++) { /* work through points in this box */
      k = ind[j]; /* candidate neighbour */
      /* is k on the exclusion list? */
      ok = 1;for (i=0;i<nex;i++) if (k == ex[i]) {ok = 0;break;}
      if (ok) { /* is it closest in this box? */
        d1 = xidist(x,X,k,d,n);
        if (d1<nd) { nd=d1;ni=k;}
      }
    }
    if (ni < 0&& bx!=0) bx = box[bx].parent; /* still need bigger box */
  } 
  /* now look for closer points in any box that could be better */
  todo[0] = 0; /* index of root box... first to check */
  item = 0; 
 
  while (item>=0) { /* items on the todo list */
    if (todo[item]==bx) { /* this is the initializing box - already dealt with */
       item--;
    } else {
      bi = todo[item]; /* box to deal with now */
      item--;
      if (box_dist(box+bi,x,d)<nd) { /* box edge is closer than existing best point 
                                             -- need to check further */
        if (box[bi].child1) { /* box has children --- add to todo list */
           item++;
           todo[item] = box[bi].child1;
           item++;
           todo[item] = box[bi].child2;
        } else { /* at smallest box end of tree */
          for (j=box[bi].p0;j<=box[bi].p1;j++) {
            k = ind[j];
            /* check if k on exclusion list */
            ok = 1;for (i=0;i<nex;i++) if (k == ex[i]) {ok = 0;break;}
            if (ok) { /* check distance if not an excluded point */
              dix = xidist(x,X,k,d,n);/* distance between points x and ind[j] */ 
              if (dix<nd) { /* point closer than existing best */
                nd = dix; /* best distance */
                ni = k; /* best distance index */  
              } /* end of point addition */
            } /* end if ok */
          } /* done the one or two points in this box */
        } /* finished with this small box */
      } /* finished with possible candiate box */
    } /* end of else branch */
  } /* todo list end */
  return(ni);
} /* closest */



void star(kdtree_type *kd, double *X,int n,int i0,int *ni,double dist) {
 /* REDUNDANT

   find indices of 5 points near points of a star centred on point i0
    and return these in ni. points are unique and do not include i0.
    start points are at dist from i0.
    kd is kd tree, relating to points stored in n rows of X.
 */
  double pi25,dx,dy,x0[2],x[2];
  int i,ex[6];
  if (kd->d!=2) Rprintf("\n star only useful in 2D\n");
  pi25 = asin(1)*4/5;
  x0[0] = X[i0];x0[1] = X[i0 + n];
  ex[0] = i0;
  for (i=0;i<5;i++) {
    dx = dist*sin(pi25*i);dy = dist*cos(pi25*i);
    x[0] = x0[0] + dx;x[1] = x0[1] + dy; /* current star point */
    /* find closest point in X/kd, not in exclusion list */
    ex[i+1] = ni[i] = closest(kd,X,x,n,ex,i+1);
  }
}

void p_area(double *a,double *X,kdtree_type kd,int n,int d) {
/* Associates the volume of its kd box with each point. If the point 
   shares a box then the volume is split. If the box has an open boundary 
   then that boundary is shrunk so that the point is enclosed in it.
   Results returned in a.
*/
  double *wa,*lo,*hi,*x0,*x1,min_w,x;
  int np,bi,i,j,k,ok=1,*count,check;

  wa = (double *)R_chk_calloc((size_t)d,sizeof(double));
  lo = (double *)R_chk_calloc((size_t)d,sizeof(double));
  hi = (double *)R_chk_calloc((size_t)d,sizeof(double));
  x0 = (double *)R_chk_calloc((size_t)d,sizeof(double));
  x1 = (double *)R_chk_calloc((size_t)d,sizeof(double));
  count = (int *)R_chk_calloc((size_t)d,sizeof(int));

  /* get average box widths, for fallback purposes */
  for (bi=0;bi<kd.n_box;bi++) {
    for (j=0;j<d;j++) {
      if (kd.box[bi].lo[j]!= - kd.huge && kd.box[bi].hi[j]!= kd.huge) {
        count[j]++; wa[j] += kd.box[bi].hi[j] - kd.box[bi].lo[j];
      }
    }
  }
  for (j=0;j<d;j++) wa[j] /= count[j];

  for (i=0;i<n;i++) {
    bi = which_box(&kd,i); /* locate smallest box containing point i */
    for (j=0;j<d;j++) { /* make copies of box boundaries to work on */
      lo[j] = kd.box[bi].lo[j];
      if (lo[j]==-kd.huge) ok = 0;
      hi[j] = kd.box[bi].hi[j];
      if (hi[j]==kd.huge) ok = 0;
    }
    np = kd.box[bi].p1-kd.box[bi].p0+1; /* number of points in box */
    if (!ok) { /* box is not finite */
      check = 0;
      k = kd.ind[kd.box[bi].p0]; /* row of X that first point is in */
      if (k==i) check=1;
      for (j=0;j<d;j++) x0[j] = X[k + j * n];
      if (np>1) { /* there is a second point to consider */
        k = kd.ind[kd.box[bi].p1];
        if (k==i) check=1;
        for (j=0;j<d;j++) x1[j] = X[k + j * n];
      }
      if (!check) Rprintf("indexing error in p_area!\n");
      /* now work through trying to shrink the limits */
      /* first shrink limits to points unless that collapses a dimension */
      ok=1; min_w = -1.0;
      for (j=0;j<d;j++) {
        if (lo[j] == -kd.huge) { /* attempt to shrink boundary to (lowest) point */
           x = x0[j]; if (np>1 && x1[j]<x) x = x1[j];  
           if (x < hi[j]) lo[j] = x; /* sorted! */
           else ok=0; /* not sorted! */
        } 
        if (hi[j] == kd.huge) { /* attempt to shrink boundary to (highest) point */
           x = x0[j]; if (np>1 && x1[j]>x) x = x1[j];  
           if (x > lo[j]) hi[j] = x; /* sorted! */
           else ok=0; /* not sorted! */
        } 
        if (lo[j] != -kd.huge && hi[j] != kd.huge) {
           x = hi[j]-lo[j];
           if (min_w < 0 || x < min_w) min_w = x;
        }
      } /* end of first pass through limits */
      if (!ok) { /* then there are unfixed limits left to deal with */
        for (j=0;j<d;j++) {
          if (lo[j] == -kd.huge) { /* attempt to shrink boundary to (lowest) point */
            x = x0[j]; if (np>1 && x1[j]<x) x = x1[j]; 
            if (min_w>0) x -= min_w; else x -= wa[j];
            lo[j] = x; /* sorted! */
          } 
          if (hi[j] == kd.huge) { /* attempt to shrink boundary to (highest) point */
            x = x0[j]; if (np>1 && x1[j]>x) x = x1[j];  
            if (min_w>0) x += min_w; else x += wa[j];
            hi[j] = x; /* sorted! */
          }
        } 
      } /* all limits now reset */
    } /* box is now finite */
    /* compute box volume */
    for (x=1.0,j=0;j<d;j++) x*= hi[j]-lo[j];
    x /= np; /* share it out */
    a[i] = x; /* store it */ 
  }
  R_chk_free(count);
  R_chk_free(x0);R_chk_free(x1);R_chk_free(lo);R_chk_free(hi);R_chk_free(wa);
} /* p_area */


void k_radius(double r, kdtree_type kd, double *X,double *x,int *list,int *nlist) {
/* Find all points in kd tree defined by kd, X within radius r of point x,
   and return these in list (initialized to length n.) on output nlist is
   number of points returned. Could be made more efficient by checking if
   boxes are completely inside r-ball, and simply adding, rather than opening, 
   if they are. 
*/
  int todo[100],item,bi,bi_old,dim,d,c1,c2,*ind,n,i;
  box_type *box;
  box = kd.box;
  d = kd.d;
  n = kd.n;
  ind = kd.ind;
  *nlist = 0; /* neighbour counter */
  bi = 0; /* box index */
  dim = 0; /* box dividing dimension (cycles as we move down tree) */
  /* find the index, bi, of the smallest box completely enclosing r-ball around x... */ 
  while (box[bi].child1) { /* box has children */
    bi_old = bi; 
    c1 = box[bi].child1;c2 = box[bi].child2;
    /* need to find out if r ball around x could be contained in one
       of the children. Idea is that we know it's in bi, so only need
       both children, if r-ball cuts the divider between children. */
    if (x[dim]+r <= box[c1].hi[dim]) bi = c1; /* r-ball is completely inside child 1 */     
    else if (x[dim]-2 >= box[c2].lo[dim]) bi = c2; /* r-ball completely in child 2 */
    dim++; if (dim==d) dim = 0;
    if (bi==bi_old) break; /* neither child contained whole r-ball, so use box[bi] */
  }
  /* box[bi] completely encloses the r-ball around x. Now check whether its points
     lie within r-ball around x... */  
  item=0;  /* index of end of task list */
  todo[0] = bi; /* initial task - box bi */
  while (item>=0) {
    bi = todo[item];item--;
    if (box_dist(box+bi,x,d) < r) { /* box could contain a point in r-ball so check */
      if (box[bi].child1) { /* box has children, so add them to todo list */
        item++;todo[item] = box[bi].child1;
        item++;todo[item] = box[bi].child2;
      } else { /* reached small end of tree - check actual points */
        for (i=box[bi].p0;i<=box[bi].p1;i++) {
          if (xidist(x,X,ind[i],d,n) < r) {
            list[*nlist] = ind[i]; (*nlist)++;             
          }
        }
      }
    }
  }
} /* k_radius */


void Rkradius(double *r,int *idat,double *ddat,double *X,double *x,int *m,int *off,int *ni,int *op) {
/* Given kd tree defined by idat, ddat and X, from R, this routine finds all points in  
   the tree less than distance r from each point in x. x contains the points stored end-to-end.
   Routine must be called twice. First with op==0, which does the work, but only returns the 
   length required for ni, in off[m+1].
   The second call must have op==1, and ni initialized to the correct length. Then neighbour
   information is returned in off, ni.
   neighbours of ith point are in ni[off[i]:(off[i+1]-1)], where off is an m+1 vector. All indexes
   0 based (C style). Add one to off and ni to get R style.
 */
  static int *nei,nn; 
  kdtree_type kd;
  double *xx;
  int d,i,j,n_buff=0,nlist,*list;
  if (*op) { /* output saved nei data */
    for (i=0;i<nn;i++) ni[i]=nei[i];
    R_chk_free(nei);nn=0;
    return;
  }
  kd_read(&kd,idat,ddat); /* unpack kd tree */
  d = kd.d; /* dimension */
  /* get the r-radius neighbour information... */
  list = (int *)R_chk_calloc((size_t)kd.n,sizeof(int)); /* list of neighbours of ith point */
  n_buff = kd.n*10;
  nei = (int *)R_chk_calloc((size_t)n_buff,sizeof(int)); /* global list of neighbours */
  xx=x;nn=0;off[0]=0;
  for (i=0;i<*m;i++) { /* work through points in x */
    k_radius(*r, kd, X,xx,list,&nlist);
    if (nn+nlist>n_buff) { /* expand nei */
      n_buff *= 2;
      nei = (int *)R_chk_realloc(nei,(size_t)n_buff*sizeof(int));
    }
    for (j=nn;j<nn+nlist;j++) nei[j] = list[j-nn];
    nn += nlist;
    off[i+1] = nn;
    xx += d; /* next point */
  }
  R_chk_free(list);
  R_chk_free(kd.box); /* free storage created by kd_read */
}

void k_newn_work(double *Xm,kdtree_type kd,double *X,double *dist,int *ni,int*m,int *n,int *d,int *k) {
/* Given a kd tree, this routine does the actual work of finding the nearest neighbours
   within the tree (defined by kd, X), to a new set of m points in x
   * inputs: 
     d is dimension.
     k is number of neighbours to find
     kd is a kd tree structure, for points stored in n by d matrix X
     Xm is m by d matrix of new points. 
   * outputs:
     ni is m by k matrix of indices of k nearest neighbours in X
     dist is m by k matrix of distances to nearest neighbours indexed in ni.
*/
  int i,j,bi,*ik,bii,todo[100],item,pcount,*ind;
  box_type *box;
  double *dk,huge,*p,*p1,*p2,dij,*x;
 
  huge = kd.huge;
  ind = kd.ind;
  box = kd.box;

  dk = (double *)R_chk_calloc((size_t)*k,sizeof(double)); /* distance k-array */
  ik = (int *)R_chk_calloc((size_t)*k,sizeof(int)); /* corresponding index array */
  x = (double *)R_chk_calloc((size_t)*d,sizeof(double)); /* array for current point */    
  pcount=0;

  for (i=0;i < *m;i++) { /* work through all the points in Xm */
    for (p=Xm+i,p1=x,p2=p1 + *d;p1<p2;p1++, p+= *m) *p1 = *p; /* copy ith point (ith row of Xm) to x */
    for (p=dk,p1=dk + *k;p<p1;p++) *p = huge; /* initialize distances to huge */
    /* here I have followed Press et al. in descending tree to smallest 
       box and then re-ascending to find box with enough points. This is
       probably more efficient than checking box size all the way down 
       if n >> k .... */
   
    /*bi = which_box(&kd,i);*/ /* bi is smallest box containing ith point */
    bi = xbox(&kd,x); /* bi is smallest box containing ith point, x */

    while (box[bi].p1-box[bi].p0 < *k) bi = box[bi].parent; /* note k does not include self */    
    
    /*  Rprintf("Initial box %d contains %d need %d\n",bi,kd.box[bi].p1-kd.box[bi].p0+1,*k); */
 
   /* now find k nearest points in the box and put in dk... */     
    for (j=box[bi].p0;j<=box[bi].p1;j++) { 
      pcount++;
      /*dij = ijdist(i,ind[j],X,*n,*d);*/ /* distance between points i and j */
      dij = xidist(x,X,ind[j],*d,*n); 
      if (dij<dk[0]) { /* distance smaller than top of heap */
        dk[0] = dij;       /* so replace top of distance heap */
        ik[0] = ind[j]; /* and put index on index heap */
        if (*k>1) update_heap(dk,ik,*k); /* update heap so it still obeys heap ordering */  
      } 
    } /* finished initialising heap (dk, ik) */
    
    /* Now search the rest of the tree. Basic idea is that if a box is further from the 
       ith point than dk[0] (the largest of the current neighbour distances), then we 
       can ignore all the points it contains (and hence its descendents) */ 
    todo[0] = 0; /* index of root box... first to check */
    item=0;
    bii = bi; /* index of initializing box */ 
    while (item>=0) { /* items on the todo list */
      if (todo[item]==bii) { /* this is the initializing box - already dealt with */
        item--;
      } else {
        bi = todo[item]; /* box to deal with now */
        item--;
        if (box_dist(box+bi,x,*d)<dk[0]) { /* box edge is closer than some of existing points 
                                              -- need to check further */
          if (box[bi].child1) { /* box has children --- add to todo list */
            item++;
            todo[item] = box[bi].child1;
            item++;
            todo[item] = box[bi].child2;
          } else { /* at smallest box end of tree */
            for (j=box[bi].p0;j<=box[bi].p1;j++) {
              pcount++;
              /*dij = ijdist(i,ind[j],X,*n,*d);*/ /* distance between points i and j */ 
              dij = xidist(x,X,ind[j],*d,*n);
              if (dij<dk[0]) { /* point closer than largest of current candidates -- add to heap */
                dk[0] = dij; /* add distance to heap */
                ik[0] = ind[j]; /* and corresponding index to heap index */
                if (*k>1) update_heap(dk,ik,*k); /* update heap so it still obeys heap ordering */  
              } /* end of point addition */
            } /* done the one or two points in this box */
          } /* finished with this small box */
        } /* finished with possible candiate box */
      } /* end of else branch */
    } /* todo list end */
    /* So now the dk, ik contain the distances and indices of the k nearest neighbours */
    for (j=0;j<*k;j++) { /* copy to output matrices */
      dist[i + j * *m] = dk[j];
      ni[i + j * *m] = ik[j];
    }     
  } /* end of points loop (i) */
 
  R_chk_free(dk);
  R_chk_free(ik);
  R_chk_free(x);
  *n = pcount;
} /* k_newn_work */

void Rkdnearest(double *X,int *idat,double *ddat,int *n,double *x, int *m, int *ni, double *dist,int *k) {
/* given points in n rows of X and a kd tree stored in idat, ddat in R, find the 
   k neares neighbours to each row of x m by d matrix x.
   * outputs 
     ni is m by k matrix of neighbour indices 
     dist is m by k matrix of neighbour distances
*/
  kdtree_type kd;
  int d;
  kd_read(&kd,idat,ddat); /* unpack kd tree */
  d = kd.d; /* dimension */
  /* get the nearest neighbour information... */
  k_newn_work(x,kd,X,dist,ni,m,n,&d,k);
  R_chk_free(kd.box); /* free storage created by kd_read */
}


void k_nn_work(kdtree_type kd,double *X,double *dist,int *ni,int *n,int *d,int *k) {
/* Given a kd tree, this routine does the actual work of finding the nearest neighbours.
*/
  int i,j,bi,*ik,bii,todo[100],item,pcount,*ind;
  box_type *box;
  double *dk,huge,*p,*p1,*p2,dij,*x;
 
  huge = kd.huge;
  ind = kd.ind;
  box = kd.box;

  dk = (double *)R_chk_calloc((size_t)*k,sizeof(double)); /* distance k-array */
  ik = (int *)R_chk_calloc((size_t)*k,sizeof(int)); /* corresponding index array */
  x = (double *)R_chk_calloc((size_t)*d,sizeof(double)); /* array for current point */    
  pcount=0;

  for (i=0;i < *n;i++) { /* work through all the points in X */
    for (p=X+i,p1=x,p2=p1 + *d;p1<p2;p1++, p+= *n) *p1 = *p; /* copy ith point (ith row of X) to x */
    for (p=dk,p1=dk + *k;p<p1;p++) *p = huge; /* initialize distances to huge */
    /* here I have followed Press et al. in descending tree to smallest 
       box and then re-ascending to find box with enough points. This is
       probably more efficient than checking box size all the way down 
       if n >> k .... */
   
    bi = which_box(&kd,i); /* bi is smallest box containing ith point */
 

 /*   for (j=0;j<*d;j++) 
    if (x[j]<kd.box[bi].lo[j]||x[j]>kd.box[bi].hi[j]) { 
      Rprintf("%d  ",i);
      for (j=0;j<*d;j++) Rprintf("%g  ",x[j]);
      for (j=0;j<*d;j++) Rprintf("%g  ",kd.box[bi].lo[j]);
      for (j=0;j<*d;j++) Rprintf("%g  ",kd.box[bi].hi[j]);
      Rprintf("\n");
    }

    Rprintf("%d  ",bi);*/

    while (box[bi].p1-box[bi].p0 < *k) bi = box[bi].parent; /* note k does not include self */    
    
    /*  Rprintf("Initial box %d contains %d need %d\n",bi,kd.box[bi].p1-kd.box[bi].p0+1,*k); */
    /* now find k nearest points in the box and put in dk... */    
 
    for (j=box[bi].p0;j<=box[bi].p1;j++) 
    if (ind[j]!=i) { /* avoid self! */
      pcount++;
      dij = ijdist(i,ind[j],X,*n,*d); /* distance between points i and j */ 
      if (dij<dk[0]) { /* distance smaller than top of heap */
        dk[0] = dij;       /* so replace top of distance heap */
        ik[0] = ind[j]; /* and put index on index heap */
        if (*k>1) update_heap(dk,ik,*k); /* update heap so it still obeys heap ordering */  
      } 
    } /* finished initialising heap (dk, ik) */
    
    /* Now search the rest of the tree. Basic idea is that if a box is further from the 
       ith point than dk[0] (the largest of the current neighbour distances), then we 
       can ignore all the points it contains (and hence its descendents) */ 
    todo[0] = 0; /* index of root box... first to check */
    item=0;
    bii = bi; /* index of initializing box */ 
    while (item>=0) { /* items on the todo list */
      if (todo[item]==bii) { /* this is the initializing box - already dealt with */
        item--;
      } else {
        bi = todo[item]; /* box to deal with now */
        item--;
        if (box_dist(box+bi,x,*d)<dk[0]) { /* box edge is closer than some of existing points 
                                              -- need to check further */
          if (box[bi].child1) { /* box has children --- add to todo list */
            item++;
            todo[item] = box[bi].child1;
            item++;
            todo[item] = box[bi].child2;
          } else { /* at smallest box end of tree */
            for (j=box[bi].p0;j<=box[bi].p1;j++) {
              pcount++;
              dij = ijdist(i,ind[j],X,*n,*d); /* distance between points i and j */ 
              if (dij<dk[0]) { /* point closer than largest of current candidates -- add to heap */
                dk[0] = dij; /* add distance to heap */
                ik[0] = ind[j]; /* and corresponding index to heap index */
                if (*k>1) update_heap(dk,ik,*k); /* update heap so it still obeys heap ordering */  
              } /* end of point addition */
            } /* done the one or two points in this box */
          } /* finished with this small box */
        } /* finished with possible candiate box */
      } /* end of else branch */
    } /* todo list end */
    /* So now the dk, ik contain the distances and indices of the k nearest neighbours */
    for (j=0;j<*k;j++) { /* copy to output matrices */
      dist[i + j * *n] = dk[j];
      ni[i + j * *n] = ik[j];
    }     
  } /* end of points loop (i) */
 
  R_chk_free(dk);
  R_chk_free(ik);
  R_chk_free(x);
  *n = pcount;
} /* k_nn_work */

void k_nn(double *X,double *dist,double *a,int *ni,int *n,int *d,int *k,int *get_a) {
/* NOTE: n modified on exit!!
         no tie handling... impractical without!
         
   X is an n by d matrix. Each row is the location of a point
   in some Euclidean d-space.
   Find k nearest neighbours in X of all points in X. 
   ni and dist are both n by k. each row of ni contains the neighbour list.
   Each row of dist is contains the corresponding distances. 
   if get_a is non zero, then volumes of kd boxes are associated with each point
   and returned in a.
   
   Some R test code...
   cd ~simon/mgcv-related/sparse-smooth
   R CMD SHLIB kd-tree.c  
   R 
   
   dyn.load("kd-tree.so")
   set.seed(2)
   n <- 100;d <- 2;k <- 5
   X <- matrix(runif(n*d),n,d)
   dist <- matrix(0,n,k)
   system.time(oo <- .C("k_nn",X=as.double(X),dist=as.double(dist),a=as.double(1:n),ni=as.integer(dist),
                   n=as.integer(n),d=as.integer(d),k=as.integer(k),get.a=as.integer(1)))
   oo$n/n^2 ## efficiency

   dist1 <- dist <- matrix(oo$dist,n,k)
   ni1 <- ni <- matrix(oo$ni+1,n,k)
   ## checking code...
   for (i in 1:n) {
     Xi <- t(t(X)-X[i,])
     di <- rowSums(Xi^2)^.5
     oi <- order(di)
     ni1[i,] <- (1:n)[oi[2:(k+1)]]
     dist1[i,] <- di[ni1[i,]]
     oi <- order(dist[i,])
     dist[i,] <- dist[i,oi]
     ni[i,] <- ni[i,oi]
   }
   range(ni-ni1)
   range(dist-dist1)

*/
  kdtree_type kd; 
  kd_tree(X,n,d,&kd); /* set up the tree */ 
  if (*get_a) p_area(a,X,kd,*n,*d);
  k_nn_work(kd,X,dist,ni,n,d,k);
  free_kdtree(kd);
}


void kba_nn(double *X,double *dist,double *a,int *ni,int *n,int *d,int *k,
            int *get_a,double *cut_off) {
/* REDUNDANT
   Obtains a roughly balanced set of 2d + k nearish neighbours. Idea is to take nearest neighbour 
   from kd box immediately above or below each point's box, in each dimension, and to add 
   k further points from the nearest neigbours not already included.  
   For each point:
   1. get 2d+k nearest neighbours.
   2. get 2d balanced neighbours.
   3. find k nearest neighbours in set 1 that are not in set 2.
   Step 3 can go through nearest looking for self and largest.  
*/
  int ii,i,j,nn,d2k,bi,bj,max_i,q,n1,n2,*count,method=1;
  double dx,*x,max_dist,d1,d2,maxnd,xj,*db,*p,*p1,d0;
  kdtree_type kd; 
  kd_tree(X,n,d,&kd); /* set up the tree */ 
  kd_sanity(kd); /* DEBUG only */
  if (*get_a) p_area(a,X,kd,*n,*d);
  d2k = 2 * *d + *k;
  nn = *n; /* following modifies n!!*/
  k_nn_work(kd,X,dist,ni,&nn,d,&d2k); /* get 2d+k nearest neighbours */
  
  /* d0 = average of distance to 2d+k nearest neighbours - a useful basic length scale */
  for (d0=0.0,p=dist,p1=dist+ *n * d2k;p<p1;p++) d0 += *p;d0 /= *n * d2k;

  x = (double *)R_chk_calloc((size_t) *d,sizeof(double));
  /* need to get typical box scale */ 
  db = (double *)R_chk_calloc((size_t)*d,sizeof(double));
  count = (int *)R_chk_calloc((size_t)*d,sizeof(int));
  for (bi=0;bi<kd.n_box;bi++) {
    for (j=0;j<*d;j++) 
    if (kd.box[bi].lo[j] > -kd.huge&&kd.box[bi].hi[j] < kd.huge) {
      db[j] += kd.box[bi].hi[j] - kd.box[bi].lo[j];
      count[j] ++;
    }
  }
  for (j=0;j<*d;j++) { 
    db[j] /= (count[j]+1);
    if (db[j]==0.0) db[j]=1.0;
  }

  for (i=0;i<*n;i++) { /* work through points */
    if (i==112) {
      Rprintf("hello\n");
    }

    bi = which_box(&kd,i);
    /* get centre of box containing i, if possible. This leads to fewer occasions on
       which same box turns up twice as a balanced neighbour. */
   if (method==0) {
     for (j=0;j<*d;j++) {
        if (kd.box[bi].hi[j] < kd.huge && kd.box[bi].lo[j] > -kd.huge) 
       	x[j] = (kd.box[bi].hi[j] + kd.huge && kd.box[bi].lo[j])*0.5; else
        x[j] = X[i + j * *n];
      }
   } else {
     for (j=0;j<*d;j++) x[j] = X[i + j * *n];
   }
 
    for (j=0;j<*d;j++) { /* get the balanced neighbours, j indexes dimension */
      xj = x[j]; 
      /* upper neighbour ... */
      if (kd.box[bi].hi[j]!=kd.huge) { /* then there is a neighbour in this direction */
        if (method==0) {
          if (kd.box[bi].lo[j] > -kd.huge) 
            dx = (kd.box[bi].hi[j] - kd.box[bi].lo[j])*1e-6;
          else dx = db[j]*1e-6;
          if (dx <=0) dx = db[j]*1e-6;
          x[j] = kd.box[bi].hi[j]+dx;
        } else { /* idea here is to avoid e.g. neighbours that have same co-ord in this direction */
          x[j] += d0;
          if (x[j] <= kd.box[bi].hi[j]) x[j] = kd.box[bi].hi[j] + d0;
        }
        bj = xbox(&kd,x); /* box above bi on axis j*/
        if (bj==bi) { 
          Rprintf("%d upper neighbour claimed to be self d=%d!\n",i,j);
          for (q=0;q<*d;q++) {
            Rprintf("%g  %g  %g\n",kd.box[bi].lo[q],x[q],kd.box[bi].hi[q]);
          }
          Rprintf("\n");
        }
        x[j] = xj;
        /* now get nearest point to i from box bj */
        n1 = kd.ind[kd.box[bj].p0];
        d1 = ijdist(i,n1,X,*n,*d);
        if (kd.box[bj].p1>kd.box[bj].p0) { 
          n2 = kd.ind[kd.box[bj].p1];
          d2 = ijdist(i,n2,X,*n,*d);
          if (d2<d1) { d1=d2;n1=n2;}
        }
        /* now put n1 into neighbour list in place of furthest neighbour not 
           itself an already computed balanced box point (later computed points 
           are no problem) */
      
        max_dist=0.0;
        max_i=0;
        maxnd=0.0; /* largest distance to neighbour */
        for (q=0;q < d2k;q++) {
          ii = i + *n * q; /* index of distance from i to qth neighbour */
          if (dist[ii] > maxnd) maxnd = dist[ii];
          if (ni[ii] == n1) { /* point is already in neighbour set */
            ni[ii] = -(n1+1);    /* signal to ignore for replacement */
            max_i = -1; /* signal that no replacement needed */
            break; 
          }
          /* it is not impossible for the same point to turn up twice as an upper
             and lower neighbour. This can happen when a point is right on the 
             box boundary, so that an upper neighbour is also detected as a side neighbour */
          if (n1== -ni[ii]-1) { /* point already in set and marked no replace*/
            max_i = -1;  /* signal that no replacement needed */
            break;
          } 
          /* find furthest point among replaceables */
          if (ni[ii]>=0&&dist[ii]>max_dist) { 
            max_dist = dist[ii];max_i=ii;
          }
        }
        if (max_i >= 0 && d1 < *cut_off * maxnd) { /* replace furthest replacable item with n1 */
          ni[max_i] = -(n1+1); /* signal not to replace later */
          dist[max_i] = d1;
        }
      } /* upper neighbour done */

     /* lower neighbour... */ 
     if (kd.box[bi].lo[j]!=-kd.huge) { /* then there is a neigbour in this direction */
        if (method==0) {       
          if (kd.box[bi].hi[j] < kd.huge) 
            dx = (kd.box[bi].hi[j] - kd.box[bi].lo[j])*1e-6;
          else dx = db[j]*1e-6;
          if (dx <=0) dx = db[j]*1e-6;
      
          x[j] = kd.box[bi].lo[j] - dx; 
        } else {
          x[j] -= d0;
          if (x[j] >= kd.box[bi].lo[j]) x[j] = kd.box[bi].lo[j] - d0;
        }
        bj = xbox(&kd,x); /* box below bi on axis j*/
        if (bj==bi) {
          Rprintf("lower neighbour claimed to be self!\n");
        }
        x[j] = xj;
        /* now find point closest to point i in box bj */
     
        n1 = kd.ind[kd.box[bj].p0];
    
        d1 = ijdist(i,n1,X,*n,*d);
        if (kd.box[bj].p1>kd.box[bj].p0) { 
          n2 = kd.ind[kd.box[bj].p1];
          d2 = ijdist(i,n2,X,*n,*d);
          if (d2<d1) { d1=d2;n1=n2;}
        }
        /* now put n1 into neighbour list in place of furthest neighbour not 
           itself an already computed balanced box point (later computed points 
            are no problem) */
       
        max_dist=0.0;
        max_i=0;
        maxnd=0.0;
        for (q=0;q < d2k;q++) {
          ii = i + *n * q;
          if (dist[ii]>maxnd) maxnd = dist[ii];
          if (ni[ii] == n1) { /* point is already in neighbour set */
            ni[ii] = -(n1+1);    /* signal to ignore for replacement */
            max_i = -1; /* signal that no replacement needed */
            break; 
          } 
          if (n1== -ni[ii]-1) { /* point already in set and marked no replace*/
            max_i = -1;  /* signal that no replacement needed */
            break;
          }  
          if (ni[ii]>=0&&dist[ii]>max_dist) { 
            max_dist = dist[ii];max_i=ii;
          }
        }
        if (max_i>=0 && d1 < *cut_off * maxnd) { /* replace furthest replacable item with n1 */
          ni[max_i] = -(n1+1); /* signal not to replace later */
          dist[max_i] = d1;
        }
      } /* lower neighbour done */

    } /* collected balanced neighbours */
    /* finally reset the negative indices to positive */    
    for (q=0;q < d2k;q++) {
       ii = i + *n * q;
       if (ni[ii]<0) ni[ii] = -ni[ii] - 1; 
    }
  }
  R_chk_free(x); free_kdtree(kd);R_chk_free(db);R_chk_free(count);
}


void tri2nei(int *t,int *nt,int *n,int *d,int *off) {
/* Takes a triangulation of n points in d dimensions, and turns this into a 
   neighbours list. t is nt by d+1 and contains the indices of triangle 
   vertices in its rows, on entry. The indices must run from 0 to n-1.  
   off is an n vector. On exit t[0..off[0]-1] contains the neighbours of point 0,
   and t[off[i-1] .. off[i]-1] contain the neigbours of point i if i>0.
   IMPORTANT: 
   t should be initialised to double its actual size (triangulation packed first).
*/
  int i,j,k,l,ii,jj,*p,*p1,*nn,k0,k1;
  /* count d times the number of triangles each point is part of... */
  for (p=off,p1=off + *n;p<p1;p++) *p = 0;
  for (p=t,p1=t + *nt * (*d+1);p<p1;p++) off[*p] += *d; /* at most *d neighbours */ 
  /* now turn off into an intial version of the final off vector */
  for (i=1;i< *n ;i++) off[i] += off[i-1]; 
  /* create oversized storage for neighbour lists */ 
  nn = (int *)R_chk_calloc((size_t)off[*n-1],sizeof(int));
  for (p=nn,p1 = nn + off[*n-1];p<p1;p++) *p = -1; /* -1 codes unused space */
  /* now work through triangles, adding vertices to relevant neighbour lists */
  for (i=0;i<*nt;i++) { /* triangle loop */
    for (j=0;j<*d+1;j++) { /* j indexes point of interest */
      ii = t[i + j * *nt]; /* focus point */
      if (ii==0) k0=0; else k0=off[ii-1];
      k1=off[ii];
      /* now check whether elements of current triangle should be added to 
         the neighbour list for point ii */
      for (l=0;l<*d+1;l++) if (l!=j) {
        jj = t[i + l * *nt]; /* neighbour */
	for (k=k0;k<k1;k++) { 
          if (nn[k]<0) { nn[k] = jj;break;} /* added to list */
          if (nn[k]==jj) break; /* already listed */
	} 
      }
    } /* finished triangle i */
  } /* end of triangle loop */
  /* At this stage nn includes all the nearest neighbours, but also a bunch of -1
     entries. Need to compress the storage */ 
  j= k0 = 0;
  for (i=0;i<*n;i++) { /* loop through points */
    k1=off[i];
    for (k=k0;k<k1;k++) {
      if (nn[k]<0) break;
      t[j] = nn[k];j++;
    }
    off[i]=j;
    k0 = k1;
  }
  /* so now neighbour lists are stored in t and indexed by off */
  R_chk_free(nn);
} /* end of tri2nei */

void ni_dist_filter(double *X,int *n,int *d,int *ni,int *off,double *mult) {
/* ni, off store the neighbour list for points stored in rows of X. 
    This routine strips out any neighbours more than mult times the 
    average neighbour distance.
    A revised ni, off is returned
*/
  int i,j,k,i0,i1;
  double *dist,z,z2,md=0.0;
  dist = (double *)R_chk_calloc((size_t) off[*n-1],sizeof(double)); /* interpoint distances */

  /* now find the average distance to neighbours */
  i0 = 0;
  for (j=0;j<*n;j++) {
    i1 = off[j];
  
    for (i=i0;i<i1;i++) {
      z2=0.0;
      for (k=0;k<*d;k++) {
        z = (X[j + *n * k] - X[ni[i] + *n * k]);
        z2 += z*z;
      }
      dist[i] = sqrt(z2);
      md += dist[i];
    }
    i0=i1;
  }
  md/=i0; /* average neighbour distance */
  
  /* now remove distant neighbours... */
  k = i0 = 0;
  for (j=0;j<*n;j++) { /* loop through points */
    i1=off[j];
    for (i=i0;i<i1;i++) {
      if (dist[i]< md * *mult) { /* near enough */
	ni[k] = ni[i];k++;
      }
    }
    off[j]=k; /* reset off[i] to how far we actually got */
    i0 = i1;
  }
  R_chk_free(dist);
}

void nei_penalty(double *X,int *n,int *d,double *D,int *ni,int *ii,int *off,
                                  int *m,int *a_weight,double *kappa) {
/* Creates the sqrt penalty matrix entries for a sparse smoother, given a 
   neighbourhood structure, specified in ni and off and point locations given 
   in X. 

   Each row of n by d matrix X is a point. 
   ni is a list indices of neighbours.
   off (length n) indicates where, in ne, the neighbours of each point lie.
   i.e. ni[0:(off[i]-1)] contains indices neighbours of point 0.
        ni[off[i-1]:(off[i]-1)] contains indices of neighbours of i>1.
   on exit ii[off[i-1]:(off[i]-1)] == i 

   X is n by d, and each row of X contains the location of a point. 
   There are no repeat points in X.

   D contains the finite difference approximation coefficients.
     D[i] is the coeff in row ii[i], col ni[i]

   This routine uses least squares/min norm solutions if there are 
   more/fewer points in neighbourhood than are required for FD approximation.

   Set up is general to allow for future extension of this routine, but currently 
   only the d==2, m=3, k=6 TPS like case is dealt with here. 

*/
  int i,j,k,true=1,kk,l,i0,i1,max_nn=0,jj,di,doff;
  double *M,*Mi,*Vt,*sv, /* matrix mapping derivatives to function values */
    x,z; 
  
  /* first strip out distant neighbours */
  z = 10.0;
  ni_dist_filter(X,n,d,ni,off,&z);

  /* now find the maximum number of neighbours */
  i0 = 0;
  for (j=0;j<*n;j++) {
    i1 = off[j];
    if (i1-i0>max_nn) max_nn = i1-i0; /* maximum number of neighbours */
    i0=i1;
  }
  max_nn++; /* self! */
  if (max_nn<6) max_nn=6;

  M = (double *)R_chk_calloc((size_t) 6 * max_nn,sizeof(double));
  Mi = (double *)R_chk_calloc((size_t) 6 * max_nn,sizeof(double)); 
  Vt = (double *)R_chk_calloc((size_t) 6 * 6,sizeof(double));
  sv = (double *)R_chk_calloc((size_t) 6,sizeof(double));

  /*  Rprintf("Starting main loop...\n");*/
  di = i0 = 0;
  doff = off[*n-1] + *n; /* total number of neighbours + selves */
  for (j=0;j<*n;j++) { /* work through all points */
    i1 = off[j]; /* neighbours of i are i0..i1-1 */
    k = kk = i1-i0 + 1; /* number of neighbours + self */
    if (kk<6) { /* will need to pack M with zero rows */ 
      kk=6;
      for (i=0;i<6*kk;i++) M[i]=0.0;
    }
    l=0; /* row index */
    /* NOTE: d= 2 hard coded! */ 
    M[0] = 1.0;for (i=1;i<6;i++) M[i*kk] = 0.0; /* self row */
    for (i=i0;i<i1;i++) {
      ii[i] = j;
      l++; /* up row index */    
      jj = ni[i]; /* current neighbour index */   
      x = X[jj] - X[j];     
      z = X[jj + *n] - X[j + *n];
      M[l] = 1.0; /* intercept */
      M[l + kk] = x;
      M[l + 2*kk] = z;
      M[l + 3*kk] = x*x/2;
      M[l + 4*kk] = z*z/2;
      M[l + 5*kk] = x*z;
    }  
    /* Let g = [f,f_x,f_z,f_xx,f_zz,f_xz], then f -> Mg as neighbours
       approach point i. Now pseudo invert M, to estimate g using g = M^{-}f */
     
    /* call mgcv_svd_full to pseudoinvert M */
    i = 6;
    mgcv_svd_full(M,Vt,sv,&kk,&i); 

    /* Rprintf("%d done svd...\n",i);*/ 
    jj = k; if (jj>6) jj=6;
    kappa[i] = sv[0]/sv[jj-1]; /* condition number */
    
    for (i=0;i<jj;i++) if (sv[i]>sv[0]*1e-10) sv[i] = 1/sv[i]; else sv[i]=0.0; 
    /* if k < kk, need to remove trailing rows of M */
    if (k<kk) {
      jj=0;
      for (i=0;i<6;i++) for (l=0;l<kk;l++)  if (l < k) { M[jj] = M[l + kk * i];jj++;}
      for (i=k;i<kk;i++) sv[i] = 0.0; /* set machine zeroes to zero */
    }

    /* Now form V diag(sv) M' */
    for (i=0;i<6;i++) { 
      x=sv[i];
      for (l=0;l<k;l++) M[l+i*k] *= x;
    }
    i=6;
    mgcv_mmult(Mi,Vt,M,&true,&true,&i,&k,&i);

    /* g = Mi f, where g = [f_0,f_x,f_z,f_zz,f_zz,f_xz] in obvious 
       notation, while f = [f_0,f_neighbours]. So first column of 
       Mi gives the dependence on the self node, f_0. */


    /*  Rprintf("done mmult...\n"); */
    /* Now read coefficients of second derivatives out into D matrix */
    /* if (*a_weight) x = sqrt(area[i]); else */ x = 1.0;
    
    /* issue here is that off and ii don't include references to self,
       don't want to handle this with an interleaving convention as
       it's a mess to code, and will increase memory footprint. Better 
       to do all self nodes at start and then work through nodes indexed 
       by off and ii */ 
    for (l=0;l<3;l++) 
      D[j + doff * l] = Mi[3 + l]; /* self nodes are first in each block */
 
    for (i=1;i<k;i++) {  /* now neighbours in off, ii order */ 
      for (l=0;l<3;l++) D[di + doff * l + *n] = Mi[3 + l + 6 * i];
      di++;
    }
    i0=i1;
  }
  /* free memory... */ 
  R_chk_free(M);
  R_chk_free(Mi);
  R_chk_free(Vt);
  R_chk_free(sv);
} /* end of tri_penalty */




/************************************************************/
/* Fast stable full rank cubic spline smoothing based on    
   deHoog and Hutchinson, 1987 and Hutchinson and deHoog,
   1985.... All O(n) by exploiting band structure. */ 
/************************************************************/

void QTz(int i,int j,double c,double s, double *z) { 
/* applies a Givens rotation to z */   
  double temp;
  temp=z[i]*c+z[j]*s;
  z[j]=z[j]*c-z[i]*s;
  z[i]=temp;
}

void givens(double a, double b,double *c,double *s) {
/* Obtain Givens rotation c and s to annihilate b (setting 
   a = sqrt(a^2 + b^2))*/
  double t;
  if (a==0.0) { *c=1.0;*s=0.0;} else
  if (fabs(a)<=fabs(b))
  { t=a/b;
    *s=1/sqrt(1+t*t);
    *c= (*s)*t;
  } else
  { t=b/a;
    *c=1/sqrt(1+t*t);
    *s=(*c)*t;
  }
}

void ss_setup(double *ub,double *lb,double *x,double *w, int *n) {
/* The upper and lower bands ,ub and lb of the matrix C of H&dH (3.2) are 
   set up the lower band coming from the Choleski decomposition of H (3.1)*/
  double *h,*hh,*hh1,*lb1,*ub2,*ub1;
  int i;
  h = (double *)R_chk_calloc((size_t) *n,sizeof(double));
  hh=(double *)R_chk_calloc((size_t) *n,sizeof(double));
  hh1=(double *)R_chk_calloc((size_t) *n,sizeof(double));
  for (i=0;i< *n-1;i++) h[i]=x[i+1]-x[i];
  for (i=0;i< *n-2;i++) hh[i]=2.0*(h[i]+h[i+1])/3.0;
  for (i=0;i< *n-3;i++) hh1[i]=h[i+1]/3.0;
 
  lb[0] = sqrt(hh[0]);
  lb1 = lb + *n; /* pointers to second band */

  for (i=1;i< *n-3;i++)
  { lb[i]= sqrt(hh[i]-lb1[i-1]*lb1[i-1]);
    lb1[i]= hh1[i]/lb[i];
  }
  lb[*n-3] = sqrt(hh[*n-3]-lb1[*n-4]*lb1[*n-4]);
  ub1 = ub + *n;ub2 = ub1 + *n; 
  for (i=0;i< *n-2;i++)
  { ub[i] = w[i]/h[i];
    ub1[i] = -w[i+1]*(1/h[i]+1/h[i+1]);
    ub2[i] = w[i+2]/h[i+1];
  }
  R_chk_free(h);R_chk_free(hh);R_chk_free(hh1);
}



void sspl_construct(double *lambda,double *x,double *w,double *U,double *V,
             double *diagA,double *lb,int *n,double *tol) {
/* function to set up smoothing spline of data at x weighted by w.
   x is ordered. dim(x) = n. lambda is the smoothing parameter. 

   w propto 1/stdev(y), where y is the response to be smoothed. This is the 
   usual convention, but H and deH actually use the reciprocal of this, so 
   routine converts.

   U and V will each contain n pairs of Givens rotations and are therefor of 
   length 4*n. They are packed in 4 vectors...
   [rotation 0 s, rotation 0 c, rotation 1 s, rotation 1 c] 
   
   lb is storage for the lower band in dH&H terms. It is needed for computing
   spline coefficients. Dimension is 2 * n;

   diagA will contain the diagonal elements of the influence matrix on exit. 

   duplicates are dealt with here by deleting duplicate x values and summing the 
   squared weights for the duplicate set. On exit *n is the number of unique x values.
   idea is that y's at duplicate x values will be averaged before smoothing.
  
   x values within tol of each other are treated as duplicates.
*/   
  double *ub,rho,*p,*p1,*ub1,*ub2,*lb1,c,s,*U0s,*U0c,*U1s,*U1c,
    *V0s,*V0c,*V1s,*V1c,upper,w2=0.0,
    L11=0.0,L12=0.0,L13,L21,L22,L23,L31,L32,L33,X1,X2,X3,Lt,temp;
  int i,k,ok;
  /* first check for duplicates */
  k=0;ok=1;
  for (i=1;i<*n;i++) if (x[k] + *tol < x[i]) { 
     if (!ok) { w[k] = sqrt(w2);}
     k++;
     x[k] = x[i]; 
     w[k] = w[i];ok=1;
   } else { /* a duplicate */
     if (ok) { w2 = w[k]*w[k];}
     w2 += w[i]*w[i];
     ok=0;
  }
  if (!ok) { w[k] = sqrt(w2);} 
  *n = k+1;

  for (i=0;i<*n;i++) w[i] = 1/w[i];  /* convert to H and de H convention */

  ub = (double *)R_chk_calloc((size_t)(*n * 3),sizeof(double));
  ss_setup(ub,lb,x,w,n);
  rho = sqrt(*lambda);
  /* multiply the upper band of the matrix by the smoothing parameter */
  for (p = ub,p1 = ub + *n * 3;p < p1;p++) *p *= rho;
  /* Now do the QR decomposition of double banded matrix C... */
  ub1 = ub + *n;ub2 = ub1 + *n;
  lb1 = lb + *n; 
  U0s = U;U0c = U + *n;
  U1s = U + *n * 2;U1c = U + *n *3;
  V0s = V;V0c = V + *n;
  V1s = V + *n * 2;V1c = V + *n *3;
  for (i=0;i<*n-3;i++)
  { givens(ub[i+1],lb1[i],&c,&s);
    temp = c*lb[i]-s*ub1[i];
    ub[i+1]=s*lb1[i]+c*ub[i+1];
    ub1[i]=s*lb[i]+c*ub1[i];
    lb[i]=temp;
    U1s[i] = -s; U1c[i] = c; 

    givens(ub[i],lb[i],&c,&s);
    ub[i]=c*ub[i]+s*lb[i];
    U0s[i] = -s; U0c[i] = c; 

    givens(ub[i],ub1[i],&c,&s);
    ub[i]=c*ub[i]+s*ub1[i];
    upper=s*ub[i+1];
    ub[i+1]=c*ub[i+1];
    V0s[i] = -s;V0c[i] = c;

    givens(ub[i],ub2[i],&c,&s);
    ub1[i+1]=c*ub1[i+1]-s*upper;
    if (i!=(*n-4)) ub[i+2]=c*ub[i+2];
    V1s[i] = -s; V1c[i] = c;

  }
  i = *n-3;
  givens(ub[i],lb[i],&c,&s);
  ub[i]=c*ub[i]+s*lb[i];
  U0s[i] = -s; U0c[i] = c; 

  givens(ub[i],ub1[i],&c,&s);
  ub[i]=c*ub[i]+s*ub1[i];
  V0s[i] = -s;V0c[i] = c;

  givens(ub[i],ub2[i],&c,&s);
 
  V1s[i] = -s; V1c[i] = c;

  /* Now compute the trace of the influence matrix */ 

  V0c += *n-3;V0s += *n-3;
  V1c += *n-3;V1s += *n-3;
  U0c += *n-3;U0s += *n-3;
  U1c += *n-3;U1s += *n-3;

  L31 = *V1c;L33 = - *V1s;
  L32 = -L31 * *V0s;L31 = L31 * *V0c;

  X3 = -L31 * *U0s;
  L31 = L31 * *U0c;

  diagA[*n-1] = L33*L33;

  L33 = L32;L32=L31;

  V0c--;V0s--;V1c--;V1s--;U0c--;U0s--;U1c--;U1s--;

  L21 = *V1c;L23 = - *V1s;
  L31 = L33 * *V1s;
  L33 = L33 * *V1c;
  L22 = -L21 * *V0s;
  L21 = L21 * *V0c;
  Lt = L31 * *V0c + L32 * *V0s;
  L32 = L32 * *V0c - L31 * *V0s;
  L31=Lt;

  X2 = -L21 * *U0s;L21 = L21 * *U0c;
  X3 = -L31 * *U0s;L31 = L31 * *U0c;
  L22 = L22 * *U1c + X2 * *U1s;
  L32 = L32 * *U1c + X3 * *U1s;

  diagA[*n-2]=(L33*L33+L23*L23);

  givens(L21,L31,&c,&s);        /** The succesive rotation  **/
  L21 = L21*c+L31*s;
  Lt=L22*c+L32*s;
  L32=L32*c-L22*s;L22=Lt;
  L33=L32;L23=L22;L22=L21;

 
  for (i = *n-5;i>=0;i--)
  { V0c--;V0s--;V1c--;V1s--;U0c--;U0s--;U1c--;U1s--;
    L13 = - *V1s;L11 = *V1c;
    L21 = L23 * *V1s;L23 *=  *V1c;
    L31 = L33 * *V1s;L33 *=  *V1c;
    givens(L11,L31,&c,&s);s = -s; /** Rotation to remove upper
		  element BEFORE it propagates **/
    L11 = L11*c - L31*s;
    L12 =- L11 * *V0s;
    L11 *= *V0c;
    Lt = L21 * *V0c + L22 * *V0s;
    L22 = L22 * *V0c - L21 * *V0s;
    L21=Lt;

    X1 = -L11 * *U0s;
    L11 *= *U0c;
    L12 = L12 * *U1c + X1 * *U1s;
 
    X2 = -L21 * *U0s;
    L21 *= *U0c;
    L22 = L22 * *U1c + X2 * *U1s;

    givens(L11,L21,&c,&s);      /** Second rotation removing
	     upper element **/
    L11 = L11*c+L21*s;
    Lt = L12*c+L22*s;
    L22 = L22*c-L12*s;L12=Lt;
    
    diagA[i+2]=L33*L33+L23*L23+L13*L13;
    if (i!=0)
    { L33=L22;L23=L12;L22=L11;
    }
  }
  diagA[1]=L22*L22+L12*L12;
  diagA[0]=L11*L11;
  for (i=0;i<*n;i++) diagA[i] = 1.0 - diagA[i];
  R_chk_free(ub);
}


void sspl_apply(double *y,double *x,double *w,double *U,double *V,int *n,int *nf,double *tol) {
/* Apply the smoothing spline stored in U and V to the data in y, 
   with weights w. The smoothed values are returned in y.
   
   x and w are also modified here.

   nf is length of y and x. n is the number of unique x values.

*/
  int i,k,ok;
  double *Wy,*U0s,*U0c,*U1s,*U1c,
    *V0s,*V0c,*V1s,*V1c,*p,*p1,*p2,w2,*xx;
  if (*nf > *n) { /* deal with duplicates */
   xx = (double *)R_chk_calloc((size_t)*nf,sizeof(double));
   for (p=x,p1=x + *nf,p2=xx;p<p1;p++,p2++) *p2 = *p;
   k=0;ok=1;
   for (i=1;i<*nf;i++) if (xx[k] + *tol < xx[i]) { 
     if (!ok) { w[k] = sqrt(w2);y[k] /= w2;}
     k++;
     xx[k] = xx[i];y[k] = y[i];
     w[k] = w[i];ok=1;
   } else { /* a duplicate */
     if (ok) { w2 = w[k]*w[k];y[k] *= w[k]*w[k];}
     w2 += w[i]*w[i];
     y[k] += y[i] * w[i] * w[i];
     ok=0;
   }
   if (!ok) { w[k] = sqrt(w2);y[k]/=w2;}
   R_chk_free(xx);
  }

  for (i=0;i<*n;i++) w[i] = 1/w[i];  /* convert to H and de H convention */

  /* ... y now contains weighted averages at unique x values, w corresponding weights */
  
  Wy = (double *)R_chk_calloc((size_t) 2 * *n,sizeof(double));
  for (i=0;i < *n;i++) Wy[i] = y[i]/w[i];
  /* set up pointers to various rotation pairs.. */
  U0s = U;U0c = U + *n;
  U1s = U + *n * 2;U1c = U + *n *3;
  V0s = V;V0c = V + *n;
  V1s = V + *n * 2;V1c = V + *n *3;
  for (i=0;i<*n-3;i++)
  { QTz(i+1,*n+i,U1c[i],-U1s[i],Wy);

    QTz(i,*n+i,U0c[i],-U0s[i],Wy);
   
    QTz(i,i+1,V0c[i],-V0s[i],Wy);
  
    QTz(i,i+2,V1c[i],-V1s[i],Wy);
  }
  i= *n-3;
  QTz(i,*n+i,U0c[i],-U0s[i],Wy);
 
  QTz(i,i+1,V0c[i],-V0s[i],Wy);
 
  QTz(i,i+2,V1c[i],-V1s[i],Wy);

  /* Calculates Weighted Residual */

  for (i = *n-2;i<2 * *n ;i++) Wy[i]=0.0;

  for (i= *n-3;i>=0;i--)
  { QTz(i,i+2,V1c[i],V1s[i],Wy);
    QTz(i,i+1,V0c[i],V0s[i],Wy);
    QTz(i,*n+i,U0c[i],U0s[i],Wy);
    if (i != *n-3) QTz(i+1,*n+i,U1c[i],U1s[i],Wy);
  }

  /* get fitted values... */  
  for (i=0;i<*n;i++) Wy[i] = y[i] - Wy[i]*w[i];

  if (*nf > *n) { /* deal with duplicates */
   k=0;ok=1;
   y[0] = Wy[0];
   for (i=1;i<*nf;i++) if (x[k] + *tol < x[i]) { /* distinct */
     k++;x[k] = x[i]; y[i] = Wy[k];
   } else { /* a duplicate */
     y[i] = Wy[k];
   }
  } else {
    for (i=0;i<*n;i++) y[i] = Wy[i];
  }
  R_chk_free(Wy);
}

void sspl_mapply(double *y,double *x,double *w,double *U,double *V,int *n,int *nf,double *tol,int *m) {
/* apply smoothing spline to the m columns of y */
  int i,xw_store=0;
  double *xx,*ww,*p,*p1,*p2;
  if (*m > 1 && *nf != *n) xw_store=1;
  if (xw_store) { /* must store original x and w */
    xx = (double *)R_chk_calloc((size_t)*nf,sizeof(double));
    ww = (double *)R_chk_calloc((size_t)*nf,sizeof(double));
    for (p=xx,p1=xx + *nf,p2=x;p<p1;p++,p2++) *p = *p2;
    for (p=ww,p1=ww + *nf,p2=w;p<p1;p++,p2++) *p = *p2;
  } 
  for (i=0;i < *m;i++) {
    if (xw_store) { /* sspl_apply modifies x and w... need to reset */
      for (p=xx,p1=xx + *nf,p2=x;p<p1;p++,p2++) *p2 = *p;
      for (p=ww,p1=ww + *nf,p2=w;p<p1;p++,p2++) *p2 = *p;
    }
    sspl_apply(y,x,w,U,V,n,nf,tol); /* smooth y */
    y += *nf;
  }
  if (xw_store) {R_chk_free(xx);R_chk_free(ww);}
}

void ss_coeffs(double *lb,double *a,double *b,double *c,double *d,double *x, int *n) { 
/* given smoothed values in a (as computed in ss_apply) and corresponding unique values in
   x, computes coefficients of piecewise cubics making up spline. Note that ubder duplication
   what is returned by ss_apply will contain duplicates, this routine requires the unduplicated 
   version of the smoothed values.
   if x[i] <= x <= x[i+1] then 
     f(x) = a[i] + b[i]*(x-x[i]) + c[i]*(x-x[i])^2 + d[i]*(x-x[i])^3
 */
  double *GTA,*z,*h,*lb1;
  int i;
  GTA=(double *)R_chk_calloc((size_t)*n,sizeof(double));
  z=(double *)R_chk_calloc((size_t)*n,sizeof(double));
  h=(double *)R_chk_calloc((size_t)*n-1,sizeof(double));
  for (i=0;i<*n-1;i++) h[i]=x[i+1]-x[i];
  for (i=0;i<*n-2;i++)
  GTA[i]=a[i]/h[i]-a[i+1]*(1/h[i]+1/h[i+1])+a[i+2]/h[i+1];
  lb1 = lb + *n;
  z[0]=GTA[0]/lb[0];
  for (i=1;i<*n-2;i++) z[i]=(GTA[i]-lb1[i-1]*z[i-1])/lb[i];
  c[*n-2]=z[*n-3]/lb[*n-3];c[*n-1]=0.0;c[0]=0.0;
  for (i=*n-4;i>=0;i--)
  c[i+1]=(z[i]-lb1[i]*c[i+2])/lb[i];
  b[*n-1]=d[*n-1]=0;
  for (i=0;i<*n-1;i++)
  { d[i]=(c[i+1]-c[i])/(3*h[i]);
    b[i]=(a[i+1]-a[i])/h[i]-c[i]*h[i]-d[i]*h[i]*h[i];
  }
  R_chk_free(GTA);R_chk_free(z);R_chk_free(h);
}




