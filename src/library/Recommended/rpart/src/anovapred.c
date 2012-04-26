/* SCCS @(#)anovapred.c	1.3  02/08/98 */
/*
** The error function for anova splitting
*/
double anovapred(double *y, double *yhat)
    {
    double temp;

    temp = y[0] - *yhat;
    return(temp*temp);
    }
