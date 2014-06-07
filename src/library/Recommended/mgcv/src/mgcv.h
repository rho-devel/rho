/* main method routines */

void magic(double *y,double *X,double *sp0,double *def_sp,double *S,double *H,double *L,
	   double *lsp0,double *gamma,double *scale, int *control,int *cS,double *rank_tol,
	   double *tol,double *b,double *rV,double *norm_const,int *n_score,int *nt);

void gdi1(double *X,double *E,double *Es,double *rS,double *U1,
	  double *sp,double *z,double *w,double *wf,double *alpha,double *mu,double *eta, double *y,
	 double *p_weights,double *g1,double *g2,double *g3,double *g4,double *V0,
	 double *V1,double *V2,double *V3,double *beta,double *D1,double *D2,
         double *P0, double *P1,double *P2,double *trA,
         double *trA1,double *trA2,double *rV,double *rank_tol,double *conv_tol, int *rank_est,
	 int *n,int *q, int *M,int *Mp,int *Enrow,int *rSncol,int *deriv,
	  int *REML,int *fisher,int *fixed_penalty,int *nthreads);     

void pls_fit(double *y,double *X,double *w,double *E,int *n,int *q,int *cE,double *eta,
             double *penalty,double *rank_tol);
void pls_fit1(double *y,double *X,double *w,double *E,double *Es,int *n,int *q,int *rE,double *eta,
	      double *penalty,double *rank_tol,int *nt);

void get_detS2(double *sp,double *sqrtS, int *rSncol, int *q,int *M, int * deriv, 
               double *det, double *det1, double *det2, double *d_tol,
               double *r_tol,int *fixed_penalty); /* stable determinant of sum evaluation */

void get_stableS(double *S,double *Qf,double *sp,double *sqrtS, int *rSncol, int *q,int *M, int * deriv, 
               double *det, double *det1, double *det2, double *d_tol,
		 double *r_tol,int *fixed_penalty);

/* various service routines */
void  tweedious(double *w,double *w1,double *w2,double *y,double *phi,double *p,double *eps,int *n);
void psum(double *y, double *x,int *index,int *n);
void rwMatrix(int *stop,int *row,double *w,double *X,int *n,int *p);
void in_out(double *bx, double *by, double *break_code, double *x,double *y,int *in, int *nb, int *n);
void Rlanczos(double *A,double *U,double *D,int *n, int *m, int *lm,double *tol);
void RuniqueCombs(double *X,int *ind,int *r, int *c);
void  RPCLS(double *Xd,double *pd,double *yd, double *wd,double *Aind,double *bd,double *Afd,double *Hd,double *Sd,int *off,int *dim,double *theta, int *m,int *nar);
void RMonoCon(double *Ad,double *bd,double *xd,int *control,double *lower,double *upper,int *n);
void MinimumSeparation(double *gx,double *gy,int *gn,double *dx,double *dy, int *dn,double *dist);
void rksos(double *x,int *n,double *eps);
void pivoter(double *x,int *r,int *c,int *pivot, int *col, int *reverse);

/* Routines for linear algebra with direct access to linpack and lapack */

void mgcv_chol(double *a,int *pivot,int *n,int *rank);
void mgcv_svd(double *x,double *u, double *d,int *r,int *c);
void mgcv_qrqy(double *b,double *a,double *tau,int *r,int *c,int *k,int *left,int *tp);
void mgcv_backsolve(double *R,int *r,int *c,double *B,double *C, int *bc);
void mgcv_forwardsolve(double *R,int *r,int *c,double *B,double *C, int *bc);
void mgcv_qr(double *x, int *r, int *c,int *pivot,double *tau);
void mgcv_qr2(double *x, int *r, int *c,int *pivot,double *tau);
void update_qr(double *Q,double *R,int *n, int *q,double *lam, int *k);
extern void mgcv_mmult(double *A,double *B,double *C,int *bt,int *ct,int *r,int *c,int *n);
void mgcv_pmmult(double *A,double *B,double *C,int *bt,int *ct,int *r,int *c,int *n,int *nt);
void mgcv_mmult0(double *A,double *B,double *C,int *bt,int *ct,int *r,int *c,int *n);
void mgcv_svd_full(double *x,double *vt,double *d,int *r,int *c);
void mgcv_symeig(double *A,double *ev,int *n,int *use_dsyevd, int *get_vectors,int *descending);
void mroot(double *A,int *rank,int *n);
void R_cond(double *R,int *r,int *c,double *work,double *Rcondition);
void mgcv_td_qy(double *S,double *tau,int *m,int *n, double *B,int *left,int *transpose);
void mgcv_tri_diag(double *S,int *n,double *tau);
void mgcv_trisymeig(double *d,double *g,double *v,int *n,int getvec,int descending); 
void getXtWX(double *XtWX, double *X,double *w,int *r,int *c,double *work);
void getXtX(double *XtX,double *X,int *r,int *c);
void getXtMX(double *XtMX,double *X,double *M,int *r,int *c,double *work);
void getXXt(double *XXt,double *X,int *r,int *c);
void read_mat(double *M,int *r,int*c, char *path);
void row_block_reorder(double *x,int *r,int *c,int *nb,int *reverse);
void mgcv_pqr(double *x,int *r, int *c,int *pivot, double *tau, int *nt);
void getRpqr(double *R,double *x,int *r, int *c,int *rr,int *nt);
void mgcv_pqrqy(double *b,double *a,double *tau,int *r,int *c,int *cb,int *tp,int *nt);

/* basis constructor/prediction routines*/

void crspl(double *x,int *n,double *xk, int *nk,double *X,double *S, double *F,int *Fsupplied);
void predict_tprs(double *x, int *d,int *n,int *m,int *k,int *M,double *Xu,int *nXu,
                  double *UZ,double *by,int *by_exists,double *X);
void construct_tprs(double *x,int *d,int *n,double *knt,int *nk,int *m,int *k,double *X,double *S,
                    double *UZ,double *Xu,int *nXu,double *C);
void gen_tps_poly_powers(int *pi,int *M,int *m, int *d);
void boundary(int *G, double *d, double *dto, double *x0, double *y0, double *dx, double *dy,
              int *nx, int *ny, double *x, double *y,double *break_code, int *n, int *nb);
void gridder(double *z,double *x,double *y,int *n,double *g, int *G,int *nx, int *ny,double *x0, 
             double *y0,double *dx,double *dy,double NA_code);
void pde_coeffs(int *G,double *x,int *ii,int *jj,int *n,int *nx,int *ny,double *dx,double *dy);

/* sparse smooth related routines */
void k_nn(double *X,double *dist,double *a,int *ni,int *n,int *d,int *k,int *get_a);
void Rkdtree(double *X,int *n, int *d,int *idat,double *ddat);
void Rkdnearest(double *X,int *idat,double *ddat,int *n,double *x, int *m, int *ni, double *dist,int *k);
void Rkradius(double *r,int *idat,double *ddat,double *X,double *x,int *m,int *off,int *ni,int *op);

void tri2nei(int *t,int *nt,int *n,int *d,int *off);
void nei_penalty(double *X,int *n,int *d,double *D,int *ni,int *ii,int *off,
		 int *m,int *a_weight,double *kappa);
void sspl_construct(double *lambda,double *x,double *w,double *U,double *V,
             double *diagA,double *lb,int *n,double *tol);
void sspl_mapply(double *y,double *x,double *w,double *U,double *V,int *n,int *nf,double *tol,int *m);


