
## nearcor.R :
## Copyright (2007) Jens Oehlschlägel
## GPL licence, no warranty, use at your own risk

nearPD <-
    ## Computes the nearest correlation matrix to an approximate
    ## correlation matrix, i.e. not positive semidefinite.

    function(x               # n-by-n approx covariance/correlation matrix
             , corr = FALSE, keepDiag = FALSE
             , do2eigen = TRUE  # if TRUE do a sfsmisc::posdefify() eigen step
             , only.values = FALSE# if TRUE simply return lambda[j].
             , eig.tol   = 1e-6 # defines relative positiveness of eigenvalues compared to largest
             , conv.tol  = 1e-7 # convergence tolerance for algorithm
             , posd.tol  = 1e-8 # tolerance for enforcing positive definiteness
             , maxit    = 100 # maximum number of iterations allowed
             , trace = FALSE # set to TRUE (or 1 ..) to trace iterations
             )
{
    stopifnot(isSymmetric(x))
    n <- ncol(x)
    if(keepDiag) diagX0 <- diag(x)
    ## U should be like x, but filled with '0' -- following also works for 'Matrix':
    U <- x; U[] <- 0
    X <- x
    iter <- 0 ; converged <- FALSE; conv <- Inf

    while (iter < maxit && !converged) {
        Y <- X
        T <- Y - U

        ## project onto PSD matrices
        e <- eigen(Y, symmetric = TRUE)
        Q <- e$vectors
        d <- e$values
        ## D <- diag(d)

        ## create mask from relative positive eigenvalues
        p <- d > eig.tol*d[1]

        ## use p mask to only compute 'positive' part
        Q <- Q[,p,drop = FALSE]
        ## X <- Q %*% D[p,p,drop = FALSE] %*% t(Q)  --- more efficiently :
        X <- tcrossprod(Q * rep(d[p], each=nrow(Q)), Q)

        ## update Dykstra's correction
        U <- X - T

        ## project onto symmetric and possibly 'given diag' matrices:
        X <- (X + t(X))/2
        if(corr) diag(X) <- 1
        else if(keepDiag) diag(X) <- diagX0

        conv <- norm(Y-X, "I") / norm(Y, "I")
        iter <- iter + 1
	if (trace)
	    cat(sprintf("iter %3d : #{p}=%d, ||Y-X|| / ||Y||= %11g\n",
			iter, sum(p), conv))

        converged <- (conv <= conv.tol)
    }

    if(!converged) {
        warning("nearPD() did not converge in ", iter, " iterations")
    }

    ## force symmetry is *NEVER* needed, we have symmetric X here!
    ## X <- (X + t(X))/2
    if(do2eigen || only.values) {
        ## begin from posdefify(sfsmisc)
        e <- eigen(X, symmetric = TRUE)
        d <- e$values
        Eps <- posd.tol * abs(d[1])
        if (d[n] < Eps) {
            d[d < Eps] <- Eps
            if(!only.values) {
                Q <- e$vectors
                o.diag <- diag(X)
                X <- Q %*% (d * t(Q))
                D <- sqrt(pmax(Eps, o.diag)/diag(X))
                X[] <- D * X * rep(D, each = n)
            }
        }
        if(only.values) return(d)

        ## unneeded(?!): X <- (X + t(X))/2
        if(corr) diag(X) <- 1
        else if(keepDiag) diag(X) <- diagX0

    } ## end from posdefify(sfsmisc)

    structure(list(mat =
		   new("dpoMatrix", x = as.vector(X),
		       Dim = c(n,n), Dimnames = .M.DN(x)),
                   eigenvalues = d,
                   corr = corr, normF = norm(x-X, "F"), iterations = iter,
		   rel.tol = conv, converged = converged),
	      class = "nearPD")
}
