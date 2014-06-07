#include "threeDplot.h"




static void 
calculate_angles(double *x, double *y, double *z,
		 double *ls, double *misc, 
		 double distance)
{

    double x1, x2, y1, y2, z1, z2, len;
    double xl, yl, zl, xe, ye, ze; /* light source and eye  */ 
    double xn, yn, zn, xr, yr, zr; /* normal and reflected ray */ 

    /* 
       We have:

       *   three points defining the plane (x, y, z)
       *   a light source
       *   an implicit viewing point (0, 0, 1/distance)


       * We want (to be put into misc):

       *   (1) angle between incident light and normal (measure of
               foreshortening). We will return the cosine of this
               angle, which will be 1 at its brightest and 0 when 90
	       degree, which is perfect as an irradiance measure

       *   (2) angle between reflected light and direction of
               viewing. Again, we will calculate the cosine of this
	       angle, which will be 1 for full reflectance and 0 at
	       minimum.


       * To start with, we will arbitrarily choose the first point as
       * the origin and shift everything by that amount.

    */

    x1 = x[1] - x[0];
    y1 = y[1] - y[0];
    z1 = z[1] - z[0];

    x2 = x[2] - x[0];
    y2 = y[2] - y[0];
    z2 = z[2] - z[0];
 
    xl = ls[0] - x[0];
    yl = ls[1] - y[0];
    zl = ls[2] - z[0];

    xe = -x[0];
    ye = -y[0];
    if (distance == 0)
	ze = 10000 - z[0];
    else 
	ze = 1/distance - z[0];


    /* 
       We now want the normal to the plane defined by the 3 points,
       i.e., by (0,0,0), (x1, y1, z1) and (x2, y2, z2). This is in the
       direction of a = (y1 z2 - y2 z1, z1 x2 - z2 x1, x1 y2 - x2 y1)
       -- to see this, just note that a'x = 0 for these two points and
       solve for a. any valid a lies along the normal.
    */
    
    xn = y1 * z2 - y2 * z1;
    yn = z1 * x2 - z2 * x1;
    zn = x1 * y2 - x2 * y1;

    /* 
       There's no question of any lighting if the light source any eye
       are not on the same side of the surface. This happens when
       a'x has the same sign for x = xe and x = xl (and a = xn)
    */

    if ((xe * xn + ye * yn + ze * zn) * (xl * xn + yl * yn + zl * zn) < 0) {
	/* light source and eye on opposite sides  */
	misc[0] = 0;
	misc[1] = 0;
	return;
    }

    /* 
       We also need to make sure that the normal vector we use is also
       in the same side as everything else
    */

    if (xl * xn + yl * yn + zl * zn < 0) { /* xe would also have worked */
	xn *= (-1);
	yn *= (-1);
	zn *= (-1);
    }

    /* 
       the cosine of the angle between incident light and normal is
       now trivial -- it's simply the normalized inner product
    */

    misc[0] = xl * xn + yl * yn + zl * zn;
    if (misc[0] != 0) 
	misc[0] /= sqrt((xl * xl + yl * yl + zl * zl) * (xn * xn + yn * yn + zn * zn));


    /* 
       for the cosine of the angle between the reflected light and the
       eye, we first need a point along the reflected ray. For this,
       we are going to calculate the reflection of the light source on
       the normal. For this, we first need the projection of the light
       source along the normal, which is given by 

       len . x_n (x_n being the normal)

       where len is a scalar given by <x_l, x_n> / ||x_n||^2.
    */

    len = (xl * xn + yl * yn + zl * zn);
    if (len != 0) len /= (xn * xn + yn * yn + zn * zn);

    /* 
       x_r, The reflection of x_l, is then

       x_r = x_l + 2 . (len . x_n - x_l)
           = 2 . len . x_n - x_l

    */ 

    xr = 2 * len * xn - xl;
    yr = 2 * len * yn - yl;
    zr = 2 * len * zn - zl;

    /* 
       And so, we can now calculate the cosine of the angle between
       the reflected light and the eye. 
    */

    misc[1] = xr * xe + yr * ye + zr * ze;

    if (misc[1] != 0) 
	misc[1] /= sqrt((xr * xr + yr * yr + zr * zr) * (xe * xe + ye * ye + ze * ze));

    /* 
       But that angle can range from 0 to 180, and we really want the
       result to be 1 at 0, going down to 0 at 180. So, let's
       calculate cosine of half this angle, and take cos again
    */

    misc[1] = cos(acos(misc[1]) / 2);

    if (misc[0] < 0 || misc[1] < 0) 
	error("bug in wireframe calculations: cosines supposed to be non-negative but not");

    return;
}






SEXP wireframePanelCalculations(SEXP xArg, SEXP yArg, SEXP zArg, SEXP rotArg, 
				SEXP distanceArg,
				SEXP nxArg, SEXP nyArg, SEXP ngArg,
				SEXP lsArg,
				SEXP env, 
				SEXP shadeArg,
				SEXP isParSurfArg)
{
     /* Arguments supplied */
     double *x;       /* increasing vector of unique x values                      */
     double *y;       /* increasing vector of unique y values                      */
     double *z;       /* long vector of z values, length = nx * ny * ng            */
     double *rot;     /* rotation matrix 4x4 (homogeneous coordinates, as vector)  */
     double distance; /* for perspective calculations                              */
     int nx, ny, ng;  /* length of x, y and number of groups (conceptually ncol(z))*/  
     double *ls;      /* light source coordinates (norm 1)                         */
     int shade;       /* whether shading is being done (1 : yes, 0 : no            */
     int isparsurf;   /* whether we are drawing a parametrized
			 surface, in which case x y z are all matrices
			 parametrized on a regular (unknown) 2-D grid              */

     /* Other variables */
     SEXP call, sHeights, sxx, syy, szz, smisc;
     double *heights, *pxx, *pyy, *pzz, *misc;
     int *horder;
     int nh, i, k;
     double xx[4], yy[4], zz[4];
     double uxx[4], uyy[4], uzz[4]; /* without perspective */

     x = REAL(xArg);
     y = REAL(yArg);
     z = REAL(zArg);
     rot = REAL(rotArg);

     distance = asReal(distanceArg);
     
     ls = REAL(lsArg);
     
     nx = asInteger(nxArg);
     ny = asInteger(nyArg);
     ng = asInteger(ngArg);
     shade = asInteger(shadeArg);
     isparsurf = asInteger(isParSurfArg);

     /* depths (called height here) correspond to all the rectangles
      * in the grid. The indexing is conceptually according to the
      * lower left corner of the rectangle. Thus, the length of
      * heights is (nx-1)*(ny-1)*ng. Given i, would later need to
      * figure out which rectangle this corresponds to  */

     nh = (nx-1) * (ny-1) * ng; /* number of quadrilaterals */
     sHeights = PROTECT(allocVector(REALSXP, nh));
     heights = REAL(sHeights);
     
     /* printf("\nStarting depth calculation..."); */

     /* Depth Calculation: we need to determine the order in which the
      * quadrilaterals are to be drawn. However, It is not clear what
      * would be a good criteria to do this. 
     */

     /* Depth of nearest and farthest corners both have
	problems. Changing to average depth. This still has problems,
	but less frequently
     */


     for (i = 0; i < nh; i++) {
         double tx, ty, tz, th;
	 int tgi, ti;
	 int txi00, tyi00, tzi00;
	 int txi01, tyi01, tzi01;
	 int txi11, tyi11, tzi11;
	 int txi10, tyi10, tzi10;

	 tgi = i / ((nx-1)*(ny-1)); /* group index             */
	 ti =  i % ((nx-1)*(ny-1)); /* height index w/in group */

	 tyi00 = ti % (ny-1);   /* y index - (0,0) corner  */
	 txi00 = ti / (ny-1);   /* x index - (0,0) corner  */

	 tyi10 = tyi00;       /* y index - (1,0) corner  */
	 txi10 = txi00 + 1;   /* x index - (1,0) corner  */
	 tyi11 = tyi00 + 1;   /* y index - (1,1) corner  */
	 txi11 = txi00 + 1;   /* x index - (1,1) corner  */
	 tyi01 = tyi00 + 1;   /* y index - (0,1) corner  */
	 txi01 = txi00;       /* x index - (0,1) corner  */

	 tzi00 = tgi * nx * ny + txi00 * ny + tyi00;  /* z index     */
	 tzi10 = tgi * nx * ny + txi10 * ny + tyi10;  /* z index     */
	 tzi11 = tgi * nx * ny + txi11 * ny + tyi11;  /* z index     */
	 tzi01 = tgi * nx * ny + txi01 * ny + tyi01;  /* z index     */

	 if (isparsurf) { /* x,y indices same as z */
	     tyi00 = tzi00;
	     txi00 = tzi00;
	     tyi10 = tzi10;
	     txi10 = tzi10;
	     tyi11 = tzi11;
	     txi11 = tzi11;
	     tyi01 = tzi01;
	     txi01 = tzi01;
	 }

	 /* (0,0)  corner */
	 tx = x[txi00];
	 ty = y[tyi00];
	 tz = z[tzi00];
	 heights[i] = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) 
		 / (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 /* (1,0) corner */
	 tx = x[txi10];
	 ty = y[tyi10];
	 tz = z[tzi10];
	 th = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) 
		 / (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 /*WAS: if (th > heights[i]) heights[i] = th;  similar below*/
	 /* Why change? causes problems with 3D histogram type
	    plots. changing back. Revisit later (FIXME) */
	 /* if (th > heights[i]) heights[i] = th; */
	 /* Still problems, so changing to average depth */
	 heights[i] += th;

	 /* (1,1) corner */
	 tx = x[txi11];
	 ty = y[tyi11];
	 tz = z[tzi11];
	 th = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) 
		 / (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);
	 /* if (th > heights[i]) heights[i] = th; */
	 heights[i] += th;


	 /* (0,1) corner */
	 tx = x[txi01];
	 ty = y[tyi01];
	 tz = z[tzi01];
	 th = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) 
		 / (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);
	 /* if (th > heights[i]) heights[i] = th; */
	 heights[i] += th;

	 heights[i] /= 4.0;
     }

     /* Rprintf("\nFinished depth calculation, ordering...\n");  */
     
     call = PROTECT(lang2(install("order"), sHeights));
     sHeights = eval(call, R_GlobalEnv);
     UNPROTECT(2);
     horder = INTEGER(PROTECT(sHeights));

     /* printf("\nFinished ordering, proceeding..."); */


     if (shade == 1) {
	 sxx = PROTECT(allocVector(REALSXP, 3));
	 syy = PROTECT(allocVector(REALSXP, 3));
	 szz = PROTECT(allocVector(REALSXP, 3));
     }
     else {
	 sxx = PROTECT(allocVector(REALSXP, 4));
	 syy = PROTECT(allocVector(REALSXP, 4));
	 szz = PROTECT(allocVector(REALSXP, 4));
     }

     smisc = PROTECT(allocVector(REALSXP, 4));
     
     pxx = REAL(sxx);
     pyy = REAL(syy);
     pzz = REAL(szz);
     misc = REAL(smisc); 

     /* 
	misc:

	0: cos angle between normal and incident light
	1: cos angle between reflected light and eye
	2: original z-height averaged
	3: group indicator
     */


     call = PROTECT(lang4(install("wirePolygon"), sxx, syy, smisc));

     /* 
     Rprintf("\nShade = %d,  isparsurf = %d...\n", shade, isparsurf);
     Rprintf("\n nx = %d, ny = %d, nh = %d\n", nx, ny, nh);
     */



     for (i = 0; i < nh; i++) {
         double tx, ty, tz, txx, tyy, tzz, tmp;
	 int tgi, ti, anyNaOrNan;
	 int txi00, tyi00, tzi00;
	 int txi01, tyi01, tzi01;
	 int txi11, tyi11, tzi11;
	 int txi10, tyi10, tzi10;

	 ti = horder[i] - 1;         /* will draw ti-th facet now */
	 tgi = ti / ((nx-1)*(ny-1)); /* group index               */
	 ti =  ti % ((nx-1)*(ny-1)); /* height index w/in group   */

	 tyi00 = ti % (ny-1);   /* y index - (0,0) corner  */
	 txi00 = ti / (ny-1);   /* x index - (0,0) corner  */

	 tyi10 = tyi00;       /* y index - (1,0) corner  */
	 txi10 = txi00 + 1;   /* x index - (1,0) corner  */
	 tyi11 = tyi00 + 1;   /* y index - (1,1) corner  */
	 txi11 = txi00 + 1;   /* x index - (1,1) corner  */
	 tyi01 = tyi00 + 1;   /* y index - (0,1) corner  */
	 txi01 = txi00;       /* x index - (0,1) corner  */

	 tzi00 = tgi * nx * ny + txi00 * ny + tyi00;  /* z index     */
	 tzi10 = tgi * nx * ny + txi10 * ny + tyi10;  /* z index     */
	 tzi11 = tgi * nx * ny + txi11 * ny + tyi11;  /* z index     */
	 tzi01 = tgi * nx * ny + txi01 * ny + tyi01;  /* z index     */

	 if (isparsurf) { /* x,y indices same as z */
	     tyi00 = tzi00;
	     txi00 = tzi00;
	     tyi10 = tzi10;
	     txi10 = tzi10;
	     tyi11 = tzi11;
	     txi11 = tzi11;
	     tyi01 = tzi01;
	     txi01 = tzi01;
	 }


	 misc[3] = (double) tgi + 1;
	 misc[2] = 0;


	 /* (0,0) corner */
	 tx = x[txi00];
	 ty = y[tyi00];
	 tz = z[tzi00];

	 if (isparsurf)	misc[2] += sqrt(tx * tx + ty * ty + tz * tz);
	 else misc[2] += tz;
	 tmp = (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 txx = (rot[0] * tx + rot[4] * ty + rot[8] * tz + rot[12]) / tmp;
	 tyy = (rot[1] * tx + rot[5] * ty + rot[9] * tz + rot[13]) / tmp;
	 tzz = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) / tmp;

	 uxx[0] = txx;
	 uyy[0] = tyy;
	 uzz[0] = tzz;

	 if (distance != 0) {
	     txx /= (1/distance - tzz);
	     tyy /= (1/distance - tzz);
	 }

	 xx[0] = txx;
	 yy[0] = tyy;
	 zz[0] = tzz;

	 /* (1,0) corner */
	 tx = x[txi10];
	 ty = y[tyi10];
	 tz = z[tzi10];

	 if (isparsurf)	misc[2] += sqrt(tx * tx + ty * ty + tz * tz);
	 else misc[2] += tz;
	 tmp = (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 txx = (rot[0] * tx + rot[4] * ty + rot[8] * tz + rot[12]) / tmp;
	 tyy = (rot[1] * tx + rot[5] * ty + rot[9] * tz + rot[13]) / tmp;
	 tzz = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) / tmp;

	 uxx[1] = txx;
	 uyy[1] = tyy;
	 uzz[1] = tzz;

	 if (distance != 0) {
	     txx /= (1/distance - tzz);
	     tyy /= (1/distance - tzz);
	 }

	 xx[1] = txx;
	 yy[1] = tyy;
	 zz[1] = tzz;


	 /* (1,1) corner */
	 tx = x[txi11];
	 ty = y[tyi11];
	 tz = z[tzi11];

	 if (isparsurf)	misc[2] += sqrt(tx * tx + ty * ty + tz * tz);
	 else misc[2] += tz;
	 tmp = (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 txx = (rot[0] * tx + rot[4] * ty + rot[8] * tz + rot[12]) / tmp;
	 tyy = (rot[1] * tx + rot[5] * ty + rot[9] * tz + rot[13]) / tmp;
	 tzz = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) / tmp;

	 uxx[2] = txx;
	 uyy[2] = tyy;
	 uzz[2] = tzz;

	 if (distance != 0) {
	     txx /= (1/distance - tzz);
	     tyy /= (1/distance - tzz);
	 }

	 xx[2] = txx;
	 yy[2] = tyy;
	 zz[2] = tzz;




	 /* (0,1) corner */
	 tx = x[txi01];
	 ty = y[tyi01];
	 tz = z[tzi01];

	 if (isparsurf)	misc[2] += sqrt(tx * tx + ty * ty + tz * tz);
	 else misc[2] += tz;
	 tmp = (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 txx = (rot[0] * tx + rot[4] * ty + rot[8] * tz + rot[12]) / tmp;
	 tyy = (rot[1] * tx + rot[5] * ty + rot[9] * tz + rot[13]) / tmp;
	 tzz = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) / tmp;

	 uxx[3] = txx;
	 uyy[3] = tyy;
	 uzz[3] = tzz;

	 if (distance != 0) {
	     txx /= (1/distance - tzz);
	     tyy /= (1/distance - tzz);
	 }

	 xx[3] = txx;
	 yy[3] = tyy;
	 zz[3] = tzz;



	 /* Done with all four corners */

	 misc[2] /= 4; /* average height */

	 if (shade == 1) {

	     /*  
		 two different triangles will be drawn, given by
		 indices 0, 1, 2 and 2, 3, 0

	     */

	     /* first, 0,1,2 */

	     for (k = 0; k < 3; k++) {
		 pxx[k] = uxx[k];
		 pyy[k] = uyy[k];
		 pzz[k] = uzz[k];
	     }
	     calculate_angles(pxx, pyy, pzz, ls, misc, distance);

	     anyNaOrNan = 0;
	     for (k = 0; k < 3; k++) {
		 pxx[k] = xx[k];
		 pyy[k] = yy[k];
		 pzz[k] = zz[k];
		 if (ISNAN(pxx[k]) || ISNAN(pyy[k]) || ISNAN(pzz[k]))
		     anyNaOrNan = 1;
	     }
	     if (anyNaOrNan == 0) eval(call, env);

	     /* same again, but replace 1 by 3 */

	     for (k = 0; k < 3; k++) {
		 pxx[k] = uxx[k];
		 pyy[k] = uyy[k];
		 pzz[k] = uzz[k];
	     }
	     pxx[1] = uxx[3];
	     pyy[1] = uyy[3];
	     pzz[1] = uzz[3];
	     calculate_angles(pxx, pyy, pzz, ls, misc, distance);

	     anyNaOrNan = 0;
	     for (k = 0; k < 3; k++) {
		 pxx[k] = xx[k];
		 pyy[k] = yy[k];
		 pzz[k] = zz[k];
		 if (k!=1 && (ISNAN(pxx[k]) || ISNAN(pyy[k]) || ISNAN(pzz[k])))
		     anyNaOrNan = 1;
	     }
	     pxx[1] = xx[3];
	     pyy[1] = yy[3];
	     pzz[1] = zz[3];
	     if (ISNAN(pxx[1]) || ISNAN(pyy[1]) || ISNAN(pzz[1]))
		 anyNaOrNan = 1;
	     if (anyNaOrNan == 0) eval(call, env);

	 }
	 else {
	     anyNaOrNan = 0;
	     for (k = 0; k < 4; k++) {
		 pxx[k] = xx[k];
		 pyy[k] = yy[k];
		 pzz[k] = zz[k];
		 if (ISNAN(pxx[k]) || ISNAN(pyy[k]) || ISNAN(pzz[k]))
		     anyNaOrNan = 1;
	     }
	     if (anyNaOrNan == 0) eval(call, env);
	 }



     }

     UNPROTECT(6);
     return R_NilValue;
}




