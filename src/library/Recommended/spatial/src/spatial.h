void
VR_alset(Sfloat *alph, Sint *nalph);

void
VR_gls(double *x, double *y, double *z, Sint *n, Sint *np,
       Sint *npar, double *f, double *l, double *r, double *bz,
       double *wz, double *yy, double *w, Sint *ifail, double *l1f);

void
VR_ls(double *x, double *y, double *z, Sint *n, Sint *np,
      Sint *npar, double *f, double *r, double *bz, double *wz,
      Sint *ifail);

void
VR_fmat(double *f, double *x, double *y, Sint *n, Sint *np);

void
VR_frset(Sfloat *xl, Sfloat *xu, Sfloat *yl, Sfloat *yu);

void
VR_valn(double *z, double *x, double *y, Sint *n, double *beta, Sint *np);

void
VR_krpred(double *z, double *xs, double *ys, double *x, double *y,
	  Sint *npt, Sint *n, double *yy);

void
VR_prvar(double *z, double *xp, double *yp, Sint *npt,
	 double *x, double *y, double *l, double *r, Sint *n,
	 Sint *np, Sint *npar, double *l1f);

void
VR_correlogram(Sfloat *xp, Sfloat *yp, Sint *nint, double *x,
	       double *y, double *z, Sint *n, Sint *cnt);

void
VR_variogram(Sfloat *xp, Sfloat *yp, Sint *nint, double *x,
	     double *y, double *z, Sint *n, Sint *cnt);

void
VR_ppset(Sfloat *area);

void
VR_ppget(Sfloat *xx);

void
VR_sp_pp2(Sfloat *x, Sfloat *y, Sint *npt, Sint *k,
	  Sfloat *h, Sfloat *dmin, Sfloat *lm, Sfloat *fs);

void
VR_pdata(Sint *npt, Sfloat *x, Sfloat *y);

void
VR_simpat(Sint *npt, Sfloat *x, Sfloat *y, Sfloat *c,
	  Sfloat *r, Sint *init);

void
VR_simmat(Sint *npt, Sfloat *x, Sfloat *y, Sfloat *r);

void
VR_plike(Sfloat *x, Sfloat *y, Sint *npt, Sfloat *c,
	 Sfloat *r, Sint *ng, Sfloat *target, Sfloat *res);

