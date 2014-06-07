/* Automatically generated from all.nw using noweb */
#include "survS.h"

/* allocate a ragged array of a given number of rows and columns */
static double **dmatrix2(int nrow, int ncol) {
    int i;
    double **mat;
    double *d;

    mat = (double **) R_alloc(nrow, sizeof(double *));
    d   = (double *) R_alloc(nrow*ncol, sizeof(double));
    for (i=0; i<nrow; i++) {
        mat[i] = d;
        d += ncol;
        }
    return(mat);
    }
SEXP survfitci(SEXP ftime2,  SEXP sort12,  SEXP sort22, SEXP ntime2,
                    SEXP status2, SEXP cstate2, SEXP wt2,  SEXP id2,
                    SEXP p2,  SEXP sefit2) {   
    int i, j, k, kk;   /* generic loop indices */
    int ck, itime, eptr; /*specific indices */
    int ctime;      /*current time of interest, in the main loop */
    int nprotect;   /* number of protect calls issued */
    int oldstate, newstate; /*when changing state */

    double temp, *temp2;  /* scratch */
    double *p;         /* current prevalence vector */
    double **hmat;      /* hazard matrix at this time point */
    double **umat;     /* per subject leverage at this time point */
    int *atrisk;       /* 1 if the subject is currently at risk */
    int   *ns;         /* number curently in each state */
    double *ws;        /* weighted count of number state */
    double *wtp;       /* case weights indexed by subject */
    double wevent;     /* weighted number of events at current time */
    int nstate;        /* number of states */
    int n, nperson;    /*number of obs, subjects*/
    double **chaz;     /* cumulative hazard matrix */

    /* pointers to the R variables */
    int *sort1, *sort2;  /*sort index for entry time, event time */
    int *entry,* etime;  /*entry time, event time */
    int ntime;          /* number of unique event time values */
    int *status;        /*0=censored, 1,2,... new states */
    int *cstate;        /* current state for each subject */
    double *wt;         /* weight for each observation */
    int *id;            /* for each obs, which subject is it */
    int sefit;
        
    /* returned objects */
    SEXP rlist;         /* the returned list and variable names of same */  
    const char *rnames[]= {"nrisk","nevent","ncensor", "prev", 
                           "cumhaz", "var", ""};
    SEXP pmat2, vmat2, cumhaz2;  /*list components */
    SEXP nevent2, ncensor2, nrisk2;
    double *pmat, *vmat, *cumhaz;
    int  *ncensor, *nrisk, *nevent;
    ntime= asInteger(ntime2);
    nperson = LENGTH(cstate2);
    n   = LENGTH(sort12);
    PROTECT(cstate2 = duplicate(cstate2));
    cstate  = INTEGER(cstate2);
    entry= INTEGER(ftime2);
    etime= entry + n;
    sort1= INTEGER(sort12);
    sort2= INTEGER(sort22);
    status= INTEGER(status2);
    wt = REAL(wt2);
    id = INTEGER(id2);
    PROTECT(p2 = duplicate(p2));  /*copy of initial prevalence */
    p = REAL(p2);
    nstate = LENGTH(p2);  /* number of states */
    sefit = asInteger(sefit2);

    /* allocate space for the output objects */
    PROTECT(pmat2 = allocMatrix(REALSXP, nstate, ntime));
    pmat = REAL(pmat2);
    if (sefit >0)
        PROTECT(vmat2 = allocMatrix(REALSXP, nstate, ntime));
    else PROTECT(vmat2 = allocMatrix(REALSXP, 1, 1)); /* dummy object */
    vmat = REAL(vmat2);
    PROTECT(nevent2 = allocVector(INTSXP, ntime));
    nevent = INTEGER(nevent2);
    PROTECT(ncensor2= allocVector(INTSXP, ntime));
    ncensor = INTEGER(ncensor2);
    PROTECT(nrisk2 = allocMatrix(INTSXP, nstate, ntime));
    nrisk = INTEGER(nrisk2);
    PROTECT(cumhaz2= allocVector(REALSXP, nstate*nstate*ntime));
    cumhaz = REAL(cumhaz2);
    nprotect = 8;  

    /* allocate space for scratch vectors */
    ws = (double *) R_alloc(2*nstate, sizeof(double));
    temp2 = ws + nstate;
    ns  = (int *) R_alloc(nstate, sizeof(int));
    atrisk = (int *) R_alloc(nperson, sizeof(int));
    wtp = (double *) R_alloc(nperson, sizeof(double));
    hmat = (double**) dmatrix2(nstate, nstate);
    if (sefit >0) umat = (double**) dmatrix2(nperson, nstate);
    chaz = (double**) dmatrix2(nstate, nstate);

    /* R_alloc does not zero allocated memory */
    for (i=0; i<nstate; i++) {
        ws[i] =0;
        ns[i] =0;
        for (j=0; j<nstate; j++) {
                hmat[i][j] =0;
                chaz[i][j] =0;
        }
        if (sefit) {for (j=0; j<nperson; j++) umat[j][i]=0;}
     }
    for (i=0; i<nperson; i++) atrisk[i] =0;
    itime =0; /*current time index, for output arrays */
    eptr  = 0; /*index to sort1, the entry times */
    for (i=0; i<n; ) {
        ck = sort2[i];
        ctime = etime[ck];  /* current time value of interest */

        /* Add subjects whose entry time is < ctime into the counts */
        for (; eptr<n; eptr++) {
            k = sort1[eptr];
            if (entry[k] < ctime) {
                kk = cstate[id[k]];  /*current state of the addition */
                ns[kk]++;
                ws[kk] += wt[k];
                wtp[id[k]] = wt[k];
                atrisk[id[k]] =1;   /* mark them as being at risk */
            }
            else break;
        }
            
        for (j=0; j<nstate; j++) {
            for (k=0; k<nstate; k++) {
                hmat[j][k] =0;
            }
         }

        /* Count up the number of events and censored at this time point */
        nevent[itime] =0;
        ncensor[itime] =0;
        wevent =0;
        for (j=i; j<n; j++) {
            k = sort2[j];
            if (etime[k] == ctime) {
                if (status[k] >0) {
                    newstate = status[k] -1;  /* 0 based subscripts */
                    oldstate = cstate[id[k]];
                    nevent[itime]++;
                    wevent += wt[k];
                    hmat[oldstate][newstate] += wt[k];
                }
                else ncensor[itime]++;
            }
            else break;
         }
                
        if (nevent[itime]> 0) { 
            /* finish computing H */
            for (j=0; j<nstate; j++) {
                if (ns[j] >0) {
                    temp =0;
                    for (k=0; k<nstate; k++) {
                        temp += hmat[j][k];
                        hmat[j][k] /= ws[j]; /* events/n */
                    }
                    hmat[j][j] =1 -temp/ws[j]; /*rows sum to one */
                }
                else hmat[j][j] =1.0; 
         
            }
            if (sefit >0) {
                /* Update U, part 1  U = U %*% H -- matrix multiplication */
                for (j=0; j<nperson; j++) { /* row of U */
                    for (k=0; k<nstate; k++) { /* column of U */
                        temp2[k]=0;
                        for (kk=0; kk<nstate; kk++) 
                                temp2[k] += umat[j][kk] * hmat[kk][k];
                    }  
                    for (k=0; k<nstate; k++) umat[j][k] = temp2[k];
                 }

                /* Update U, part 2, subtract from everyone at risk 
                       For this I need H2 */
                for (j=0; j<nstate; j++) hmat[j][j] -= 1;
                for (j=0; j<nperson; j++) {
                    if (atrisk[j]==1) {
                        kk = cstate[j];
                        for (k=0; k<nstate; k++) 
                            umat[j][k] -= (p[kk]/ws[kk])* hmat[kk][k];
                    }
                 }

                /* Update U, part 3.  An addition for each event */
                for (j=i; j<n; j++) {
                    k = sort2[j];
                    if (etime[k] == ctime) {
                        if (status[k] >0) {
                            kk = id[k];  /* row number in U */
                            oldstate= cstate[kk];
                            newstate= status[k] -1;
                            umat[kk][oldstate] -= p[oldstate]/ws[oldstate];
                            umat[kk][newstate] += p[oldstate]/ws[oldstate];
                        }
                    }
                    else break;
                 }
            }
            /* Finally, update chaz and p.  */
            for (j=0; j<nstate; j++) {
                if (sefit ==0) hmat[j][j] -= 1;  /* conversion to H2*/
                for (k=0; k<nstate; k++) chaz[j][k] += hmat[j][k];
                
                hmat[j][j] +=1;  /* change from H2 to H */
                temp2[j] =0;
                for (k=0; k<nstate; k++)
                    temp2[j] += p[k] * hmat[k][j];
             }
            for (j=0; j<nstate; j++) p[j] = temp2[j];
        }
        /* store into the matrices that will be passed back */
        for (j=0; j<nstate; j++) {
            *pmat++ = p[j];
            *nrisk++ = ns[j];
            for (k=0; k<nstate; k++) *cumhaz++ = chaz[k][j];
            temp=0;
            if (sefit >0) {
                for (k=0; k<nperson; k++) 
                    temp += wtp[k] * umat[k][j]*umat[k][j];
                *vmat++ = temp;
            }
         }
      
        /* Take the current events and censors out of the risk set */
        for (; i<n; i++) {
            j= sort2[i];
            if (etime[j] == ctime) {
                oldstate = cstate[id[j]]; /*current state */
                ns[oldstate]--;
                ws[oldstate] -= wt[j];
                if (status[j] >0) cstate[id[j]] = status[j]-1; /*new state */
                atrisk[id[j]] =0;
            }
            else break;
        }
        itime++;  
    }  
    /* return a list */
    PROTECT(rlist=mkNamed(VECSXP, rnames));
    SET_VECTOR_ELT(rlist, 0, nrisk2);
    SET_VECTOR_ELT(rlist, 1, nevent2);
    SET_VECTOR_ELT(rlist, 2, ncensor2);
    SET_VECTOR_ELT(rlist, 3, pmat2);
    SET_VECTOR_ELT(rlist, 4, cumhaz2);
    SET_VECTOR_ELT(rlist, 5, vmat2);
    UNPROTECT(nprotect +1);
    return(rlist);
}
