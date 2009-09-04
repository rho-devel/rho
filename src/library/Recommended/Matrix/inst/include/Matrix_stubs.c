#include <Rconfig.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include "cholmod.h"

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif

CHM_DN attribute_hidden
M_as_cholmod_dense(CHM_DN ans, SEXP x)
{
    static CHM_DN(*fun)(CHM_DN,SEXP) = NULL;
    if(fun == NULL)
	fun = (CHM_DN(*)(CHM_DN,SEXP))
	    R_GetCCallable("Matrix", "as_cholmod_dense");
    return fun(ans, x);
}

CHM_FR attribute_hidden
M_as_cholmod_factor(CHM_FR ans, SEXP x)
{
    static CHM_FR(*fun)(CHM_FR,SEXP) = NULL;
    if(fun == NULL)
	fun = (CHM_FR(*)(CHM_FR,SEXP))
	    R_GetCCallable("Matrix", "as_cholmod_factor");
    return fun(ans, x);
}

CHM_SP attribute_hidden
M_as_cholmod_sparse(CHM_SP ans, SEXP x, Rboolean check_Udiag, Rboolean sort_in_place)
{
    static CHM_SP(*fun)(CHM_SP,SEXP,Rboolean,Rboolean)= NULL;
    if(fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,SEXP,Rboolean,Rboolean))
	    R_GetCCallable("Matrix", "as_cholmod_sparse");
    return fun(ans, x, check_Udiag, sort_in_place);
}

CHM_SP attribute_hidden
M_as_cholmod_triplet(CHM_SP ans, SEXP x, Rboolean check_Udiag)
{
    static CHM_SP(*fun)(CHM_SP,SEXP,Rboolean)= NULL;
    if(fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,SEXP,Rboolean))
	    R_GetCCallable("Matrix", "as_cholmod_triplet");
    return fun(ans, x, check_Udiag);
}

SEXP attribute_hidden
M_Csparse_diagU2N(SEXP x)
{
    static SEXP(*fun)(SEXP) = NULL;
    if(fun == NULL)
	fun = (SEXP(*)(SEXP))
	    R_GetCCallable("Matrix", "Csparse_diagU2N");
    return fun(x);
}

SEXP attribute_hidden
M_chm_factor_to_SEXP(CHM_FR f, int dofree)
{
    static SEXP(*fun)(CHM_FR,int) = NULL;
    if(fun == NULL)
	fun = (SEXP(*)(CHM_FR,int))
	    R_GetCCallable("Matrix", "chm_factor_to_SEXP");
    return fun(f, dofree);
}

double attribute_hidden
M_chm_factor_ldetL2(CHM_FR f)
{
    static double(*fun)(CHM_FR) = NULL;
    if(fun == NULL)
	fun = (double(*)(CHM_FR))
	    R_GetCCallable("Matrix", "chm_factor_ldetL2");
    return fun(f);
}

CHM_FR attribute_hidden
M_chm_factor_update(CHM_FR f, CHM_SP A, double mult)
{
    static CHM_FR(*fun)(CHM_FR,CHM_SP,double) = NULL;
    if(fun == NULL)
	fun = (CHM_FR(*)(CHM_FR,CHM_SP,double))
	    R_GetCCallable("Matrix", "chm_factor_update");
    return fun(f, A, mult);
}

SEXP attribute_hidden
M_chm_sparse_to_SEXP(CHM_SP a, int dofree,
		     int uploT, int Rkind, char *diag, SEXP dn)
{
    static SEXP(*fun)(CHM_SP,int,int,int,char*,SEXP) = NULL;
    if(fun == NULL)
	fun = (SEXP(*)(CHM_SP,int,int,int,char*,SEXP))
	    R_GetCCallable("Matrix", "chm_sparse_to_SEXP");
    return fun(a, dofree, uploT, Rkind, diag, dn);
}

SEXP attribute_hidden
M_chm_triplet_to_SEXP(CHM_TR a, int dofree,
		      int uploT, int Rkind, const char *diag, SEXP dn)
{
    static SEXP(*fun)(CHM_TR,int,int,int,const char*,SEXP) = NULL;
    if(fun == NULL)
	fun = (SEXP(*)(CHM_TR,int,int,int,const char*,SEXP))
	    R_GetCCallable("Matrix", "chm_triplet_to_SEXP");
    return fun(a, dofree, uploT, Rkind, diag, dn);
}

CHM_SP attribute_hidden
M_cholmod_aat(CHM_SP A, int *fset, size_t fsize,
	      int mode, CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_SP,int*,size_t,
			int,CHM_CM) = NULL;
    if(fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,int*,size_t,
			 int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_aat");
    return fun(A, fset, fsize, mode, Common);
}

CHM_SP attribute_hidden
M_cholmod_add(CHM_SP A, CHM_SP B,
	      double alpha[2], double beta[2], int values,
	      int sorted, CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_SP,CHM_SP,
			double*,double*,int,int,
			CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,CHM_SP,
			 double*,double*,int,int,
			 CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_add");
    return fun(A, B, alpha, beta, values, sorted, Common);
}

CHM_DN attribute_hidden
M_cholmod_allocate_dense(size_t nrow, size_t ncol, size_t d,
			 int xtype, CHM_CM Common)
{
    static CHM_DN(*fun)(size_t,size_t,size_t,
			int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(size_t,size_t,size_t,
			 int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_allocate_dense");
    return fun(nrow, ncol, d, xtype, Common);
}

CHM_SP attribute_hidden
M_cholmod_allocate_sparse(size_t nrow, size_t ncol, size_t nzmax,
			  int sorted, int packed, int stype,
			  int xtype, CHM_CM Common)
{
    static CHM_SP(*fun)(size_t,size_t,size_t,int,int,
			int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)
	       (size_t,size_t,size_t,int,int,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_allocate_sparse");
    return fun(nrow,ncol,nzmax,sorted,packed,stype,xtype,Common);
}

CHM_TR attribute_hidden
M_cholmod_allocate_triplet(size_t nrow, size_t ncol, size_t nzmax,
			   int stype, int xtype, CHM_CM Common)
{
    static cholmod_triplet*(*fun)(size_t,size_t,size_t,
				  int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (cholmod_triplet*(*)
	       (size_t,size_t,size_t,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_allocate_triplet");
    return fun(nrow,ncol,nzmax,stype,xtype,Common);
}

CHM_SP attribute_hidden
M_cholmod_triplet_to_sparse(cholmod_triplet *T, int nzmax,
			    CHM_CM Common)
{
    static CHM_SP(*fun)
	(cholmod_triplet*,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(cholmod_triplet*,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_triplet_to_sparse");
    return fun(T, nzmax, Common);
}

CHM_TR attribute_hidden
M_cholmod_sparse_to_triplet(CHM_SP A, CHM_CM Common)
{
    static cholmod_triplet*(*fun)
	(CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (cholmod_triplet*(*)(CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_sparse_to_triplet");
    return fun(A, Common);
}

CHM_DN attribute_hidden
M_cholmod_sparse_to_dense(CHM_SP A, CHM_CM Common)
{
    static CHM_DN(*fun)
	(CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_sparse_to_dense");
    return fun(A, Common);
}

CHM_FR attribute_hidden
M_cholmod_analyze(CHM_SP A, CHM_CM Common)
{
    static CHM_FR(*fun)(CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_FR(*)(CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_analyze");
    return fun(A, Common);
}

CHM_FR attribute_hidden
M_cholmod_analyze_p(CHM_SP A, int *Perm, int *fset,
		    size_t fsize, CHM_CM Common)
{
    static CHM_FR(*fun)(CHM_SP,int*,int*,size_t,
			CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_FR(*)(CHM_SP,int*,int*,
			 size_t,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_analyze_p");
    return fun(A, Perm, fset, fsize, Common);
}

CHM_SP attribute_hidden
M_cholmod_copy(CHM_SP A, int stype,
	       int mode, CHM_CM Common)
{
    static CHM_SP(*fun)
	(CHM_SP,int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_copy");
    return fun(A, stype, mode, Common);
}

CHM_DN attribute_hidden
M_cholmod_copy_dense(CHM_DN  A, CHM_CM Common)
{
    static CHM_DN(*fun)(CHM_DN,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(CHM_DN,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_copy_dense");
    return fun(A, Common);
}

CHM_FR attribute_hidden
M_cholmod_copy_factor(CHM_FR L, CHM_CM Common)
{
    static CHM_FR(*fun)(CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_FR(*)(CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_copy_factor");
    return fun(L, Common);
}

int attribute_hidden
M_cholmod_change_factor(int to_xtype, int to_ll, int to_super, int to_packed,
			int to_monotonic, CHM_FR L, CHM_CM Common)
{
    static int(*fun)(int,int,int,int,int,CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(int,int,int,int,int,CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_change_factor");
    return fun(to_xtype, to_ll, to_super, to_packed, to_monotonic, L, Common);
}

CHM_SP attribute_hidden
M_cholmod_copy_sparse(CHM_SP A, CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_copy_sparse");
    return fun(A, Common);
}

CHM_SP attribute_hidden
M_cholmod_factor_to_sparse(CHM_FR L, CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_factor_to_sparse");
    return fun(L, Common);
}

CHM_SP attribute_hidden
M_cholmod_submatrix(CHM_SP A, int *rset, int rsize, int *cset,
		    int csize, int values, int sorted, CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_SP,int*,int,int*,int,
			int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,int*,int,int*,
			 int,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_submatrix");
    return fun(A, rset, rsize, cset, csize, values, sorted, Common);
}

CHM_SP attribute_hidden
M_cholmod_dense_to_sparse(CHM_DN  X, int values, CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_DN,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_DN,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_dense_to_sparse");
    return fun(X, values, Common);
}

int attribute_hidden
M_cholmod_factorize(CHM_SP A, CHM_FR L,
		    CHM_CM Common)
{
    static int(*fun)(CHM_SP,CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_SP,CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_factorize");
    return fun(A, L, Common);
}

int attribute_hidden
M_cholmod_factorize_p(CHM_SP A, double *beta, int *fset,
		      size_t fsize, CHM_FR L,
		      CHM_CM Common)
{
    static int(*fun)(CHM_SP,double*,int*,size_t,
		     CHM_FR,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_SP,double*,int*,size_t,
		      CHM_FR,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_factorize_p");
    return fun(A, beta, fset, fsize, L, Common);
}

int attribute_hidden
M_cholmod_finish(CHM_CM Common)
{

    static int(*fun)(CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_finish");
    return fun(Common);
}

int attribute_hidden
M_cholmod_sort(CHM_SP A, CHM_CM Common)
{
    static int(*fun)(CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_sort");
    return fun(A, Common);
}

int attribute_hidden
M_cholmod_free_dense(CHM_DN  *A, CHM_CM Common)
{
    static int(*fun)(CHM_DN*,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_DN*,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_free_dense");
    return fun(A, Common);
}

int attribute_hidden
M_cholmod_free_factor(CHM_FR *L, CHM_CM Common)
{
    static int(*fun)(CHM_FR*,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_FR*,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_free_factor");
    return fun(L, Common);
}

int attribute_hidden
M_cholmod_free_sparse(CHM_SP *A, CHM_CM Common)
{
    static int(*fun)(CHM_SP*,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_SP*,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_free_sparse");
    return fun(A, Common);
}

int attribute_hidden
M_cholmod_free_triplet(cholmod_triplet **T, CHM_CM Common)
{
    static int(*fun)(cholmod_triplet**,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(cholmod_triplet**,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_free_triplet");
    return fun(T, Common);
}

long attribute_hidden
M_cholmod_nnz(CHM_SP A, CHM_CM Common)
{
    static long(*fun)(CHM_SP,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (long(*)(CHM_SP,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_nnz");
    return fun(A, Common);
}

int attribute_hidden
M_cholmod_sdmult(CHM_SP A, int transpose,
		 double alpha [2], double beta [2],
		 CHM_DN  X, CHM_DN  Y,
		 CHM_CM Common)
{
    static int(*fun)(CHM_SP,int,double*,double*,
		     CHM_DN,CHM_DN,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_SP,int,double*,double*,
		      CHM_DN,CHM_DN,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_sdmult");
    return fun(A, transpose, alpha, beta, X, Y, Common);
}

CHM_SP attribute_hidden
M_cholmod_ssmult(CHM_SP A, CHM_SP B,
		 int stype, int values, int sorted,
		 CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_SP,CHM_SP,
			int,int,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,CHM_SP,
			 int,int,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_ssmult");
    return fun(A, B, stype, values, sorted, Common);
}

CHM_DN attribute_hidden
M_cholmod_solve(int sys, CHM_FR L,
		CHM_DN  B, CHM_CM Common)
{
    static CHM_DN(*fun)(int,CHM_FR,CHM_DN,
			CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(int,CHM_FR,CHM_DN,
			 CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_solve");
    return fun(sys, L, B, Common);
}

CHM_SP attribute_hidden
M_cholmod_speye(size_t nrow, size_t ncol,
		int xtype, CHM_CM Common)
{
    static CHM_SP(*fun)(size_t,size_t,int,CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(size_t,size_t,int,CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_speye");
    return fun(nrow, ncol, xtype, Common);
}

CHM_SP attribute_hidden
M_cholmod_spsolve(int sys, CHM_FR L,
		  CHM_SP B, CHM_CM Common)
{
    static CHM_SP(*fun)(int,CHM_FR,
			CHM_SP, CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(int,CHM_FR,
			 CHM_SP, CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_spsolve");
    return fun(sys, L, B, Common);
}

void attribute_hidden
M_R_cholmod_error(int status, const char *file, int line, const char *message)
{
    error("Cholmod error `%s' at file:%s, line %d", message, file, line);
}

/* just to get 'int' instead of 'void' as required by CHOLMOD's print_function */
static int
R_cholmod_printf(const char* fmt, ...)
{
    va_list(ap);

    va_start(ap, fmt);
    Rprintf((char *)fmt, ap);
    va_end(ap);
    return 0;
}

int attribute_hidden
M_R_cholmod_start(CHM_CM Common)
{
    int val;
    static int(*fun)(CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_start");
    val = fun(Common);
    Common->print_function = R_cholmod_printf; /* Rprintf gives warning */
    Common->error_handler = M_R_cholmod_error;
    return val;
}

CHM_SP attribute_hidden
M_cholmod_transpose(CHM_SP A, int values, CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_SP,int,
			CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,int,
			 CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_transpose");
    return fun(A, values, Common);
}

CHM_SP attribute_hidden
M_cholmod_vertcat(CHM_SP A, CHM_SP B,
		  int values, CHM_CM Common)
{
    static CHM_SP(*fun)(CHM_SP, CHM_SP,
			int, CHM_CM) = NULL;
    if (fun == NULL)
	fun = (CHM_SP(*)(CHM_SP,CHM_SP, int, CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_vertcat");
    return fun(A, B, values, Common);
}

SEXP attribute_hidden
M_dpoMatrix_chol(SEXP x)
{
    static SEXP(*fun)(SEXP) = NULL;
    if (fun == NULL)
	fun = (SEXP(*)(SEXP))
	    R_GetCCallable("Matrix", "dpoMatrix_chol");
    return fun(x);
}

CHM_DN attribute_hidden
M_numeric_as_chm_dense(CHM_DN ans, double *v, int nr, int nc)
{
    static CHM_DN(*fun)(CHM_DN,double*,int,int) = NULL;
    if (fun == NULL)
	fun = (CHM_DN(*)(CHM_DN,double*,int,int))
	    R_GetCCallable("Matrix", "numeric_as_chm_dense");
    return fun(ans, v, nr, nc);
}

int attribute_hidden
M_cholmod_scale(CHM_DN S, int scale, CHM_SP A,
		CHM_CM Common)
{
    static int(*fun)(CHM_DN,int,CHM_SP, CHM_CM) = NULL;
    if (fun == NULL)
	fun = (int(*)(CHM_DN,int,CHM_SP, CHM_CM))
	    R_GetCCallable("Matrix", "cholmod_l_scale");
    return fun(S, scale, A, Common);
}

#ifdef	__cplusplus
}
#endif
