# SCCS @(#)xpred.rpart.s	1.18 07/05/01
#
#  Get a set of cross-validated predictions
xpred.rpart <- function(fit, xval=10, cp)
{
    if (!inherits(fit, 'rpart')) stop("Invalid fit object")

    method <- fit$method
    method.int <- pmatch(method, c("anova", "poisson", "class", "user", "exp"))
    if (method.int==5L) method.int <- 2L
    Terms <- fit$terms

    Y <- fit$y
    X <- fit$x
    wt<- fit$wt
    if (is.null(Y) || is.null(X)) {
	m <- fit$model
	if (is.null(m)) {
	    m <-fit$call[match(c("", 'formula', 'data', 'weights', 'subset',
                                 'na.action'),
                               names(fit$call), nomatch=0)]
	    if (is.null(m$na.action)) m$na.action<- na.rpart
	    m[[1]] <- as.name("model.frame.default")
	    m <- eval(m, parent.frame())
        }
	if (is.null(X)) X <- rpart.matrix(m)
	if (is.null(wt)) wt <- model.extract(m, "weights")
	if (is.null(Y)) {
	    Y <- model.extract(m, "response")
            offset <- attr(Terms, "offset")
	    if (method != "user") {
		init <- (get(paste("rpart", method, sep='.')))(Y,offset, NULL)
		Y <- as.matrix(init$y)
		numy <- ncol(Y)
            }
        }
	else {
	    Y <- as.matrix(Y)
	    numy <- ncol(Y)
	    offset <- 0
        }
    }
    else {
	Y <- as.matrix(Y)
	numy <- ncol(Y)
	offset <- 0
    }

    nobs <- nrow(X)
    nvar <- ncol(X)
    if (length(wt)==0) wt <- rep(1.0, nobs)

    cats <- rep(0, nvar)
    xlevels <- attr(fit, "xlevels")
    if (!is.null(xlevels)){
        cats[match(names(xlevels), dimnames(X)[[2]])] <-
            unlist(lapply(xlevels, length))
    }

    controls <- fit$control
    if (missing(cp)) {
	cp<- fit$cptable[,1L]
	cp <- sqrt(cp * c(10, cp[-length(cp)]))
	cp[1] <- (1+fit$cptable[1,1])/2
    }
    ncp <- length(cp)

    if (length(xval)==1L) {
                                        # make random groups
	xgroups <- sample(rep(1:xval, length=nobs), nobs, replace=FALSE)
    }
    else if (length(xval) == nrow(Y)) {
	xgroups <- xval
	xval <- length(unique(xgroups))
    }
    else {
        ## Check to see if observations were removed due to missing
	if (!is.null(fit$na.action)) {
            ## if na.rpart was used, then na.action will be a vector
	    temp <- as.integer(fit$na.action)
	    xval <- xval[-temp]
	    if (length(xval) == nobs) {
		xgroups <- xval
		xval <- length(unique(xgroups))
            }
	    else stop("Wrong length for xval")
        }
	else stop("Wrong length for xval")
    }

    costs <- fit$call$costs
    if (is.null(costs)) costs <- rep(1.0, nvar)

    parms <- fit$parms
    if (method=="user") {
	mlist <- fit$functions
	if (length(parms)==0L) init <- mlist$init(Y, offset, wt=wt)
	else                   init <- mlist$init(Y, offset, parms, wt)

        ## assign this to avoid garbage collection
        keep <- rpartcallback(mlist, nobs, init)
    }

    rpfit <- .C(C_s_xpred,
                n = as.integer(nobs),
                nvarx = as.integer(nvar),
                ncat = as.integer(cats * !fit$ordered),
                method= as.integer(method.int),
                as.double(unlist(controls)),
                parms = as.double(unlist(parms)),
                as.integer(xval),
                as.integer(xgroups),
                as.double(t(Y)),
                as.double(X),
                as.integer(is.na(X)),
                pred = double(ncp* nobs),
                as.integer(ncp),
                as.double(cp * fit$frame[1,"dev"]),
                error = character(1),
                wt = as.double(wt),
                as.integer(numy),
                as.double(costs),
                NAOK=TRUE)
    if (rpfit$n == -1)  stop(rpfit$error)

    matrix(rpfit$pred, ncol=ncp, byrow=TRUE,
           dimnames=list(dimnames(X)[[1L]], format(cp)) )
}
