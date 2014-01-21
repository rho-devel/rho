
daisy <- function(x, metric = c("euclidean", "manhattan", "gower"),
		  stand = FALSE, type = list(), weights = rep.int(1, p))
{
    ## check type of input matrix
    if(length(dx <- dim(x)) != 2 || !(is.data.frame(x) || is.numeric(x)))
	stop("x is not a dataframe or a numeric matrix.")
    n <- dx[1]# nrow
    p <- dx[2]# ncol
    varnms <- dimnames(x)[[2]]
    pColl <- function(n) paste(n, collapse = ", ")
    if(length(type)) {
	if(!is.list(type) || is.null(ntyp <- names(type)) || any(ntyp == ""))
	    stop("invalid ", sQuote("type"),"; must be named list")
	## check each component to be valid column names or numbers:
	for(nt in ntyp) {
	    cvec <- type[[nt]]
	    if(is.character(cvec)) {
		if(!is.null(varnms) && !all(cvec %in% varnms))
		    stop("type$", nt, " has invalid column names")
	    }
	    else if(is.numeric(cvec)) {
		if(!all(1 <= cvec & cvec <= p))
		    stop("type$", nt, " must be in 1:ncol(x)")
	    }
	    else stop("type$", nt, " must contain column names or numbers")
	}
	tA <- type$asymm
	tS <- type$symm
	if(!is.null(tA) || !is.null(tS)) {
	    ## tA and tS might be character and integer!
	    d.bin <- cbind(as.data.frame(x[, tA, drop= FALSE]),
					 x[, tS, drop= FALSE])
	    lenB <- sapply(lapply(d.bin, function(y)
				 levels(as.factor(y))), length)
	    if(any(lenB > 2))
		stop("at least one binary variable has more than 2 levels.")
	    if(any(lenB < 2))
		warning("at least one binary variable has not 2 different levels.")
	    ## Convert factors to integer, such that ("0","1") --> (0,1):
	    if(any(is.f <- sapply(d.bin, is.factor)))
		d.bin[is.f] <- lapply(d.bin[is.f],
				      function(f) as.integer(as.character(f)))
	    if(!all(sapply(d.bin, function(y)
			   is.logical(y) ||
			   all(sort(unique(as.numeric(y[!is.na(y)])))%in% 0:1))))
		stop("at least one binary variable has values not in {0,1,NA}")
	}
    }
    ## transform variables and construct 'type' vector
    if(is.data.frame(x)) {
	type2 <- sapply(x, data.class)
	x <- data.matrix(x)
    } else { ## matrix
        type2 <- rep("numeric", p)
        names(type2) <- colnames(x)
    }
    if(length(type)) {
	tT <- type$ ordratio
	tL <- type$ logratio
	x[, names(type2[tT])] <- unclass(as.ordered(x[, names(type2[tT])]))
	x[, names(type2[tL])] <- log10(		    x[, names(type2[tL])])
	type2[tA] <- "A"
	type2[tS] <- "S"
	type2[tT] <- "T" # was "O" (till 2000-12-14) accidentally !
    }
    type2[tI <- type2 %in% c("numeric", "integer") ] <- "I"
    if(n > 9 && any(tI) &&
       any(iBin <- apply(x[, tI, drop = FALSE], 2,
			 function(v) length(table(v)) == 2)))
	warning("binary variable(s) ", pColl(which(tI)[iBin]),
		" treated as interval scaled")

    type2[type2 == "ordered"] <- "O"
    type2[type2 == "factor"] <- "N"
    if(any(ilog <- type2 == "logical")) {
	warning(sprintf(ngettext(sum(ilog),
				 "setting 'logical' variable %s to type 'asymm'",
				 "setting 'logical' variables %s to type 'asymm'"),
			pColl(which(ilog))), domain = NA)
	type2[ilog] <- "A"
    }
    ## Note: We have 2 status codings:  ndyst = (0,1,2) and jdat = (1,2);
    ##       the latter is superfluous in principle

    ## standardize, if necessary
    all.I <- all(type2 == "I")
    if(all.I && { metric <- match.arg(metric); metric != "gower" }) {
	if(stand) {
	    x <- scale(x, center = TRUE, scale = FALSE) #-> 0-means
	    sx <- colMeans(abs(x), na.rm = TRUE)# can still have NA's
	    if(0 %in% sx) {
		warning(sQuote("x"), " has constant columns ",
			pColl(which(sx == 0)), "; these are standardized to 0")
		sx[sx == 0] <- 1
	    }
	    x <- scale(x, center = FALSE, scale = sx)
	}
	jdat <- 2L
	ndyst <- if(metric == "manhattan") 2L else 1L
    }
    else { ## mixed case or explicit "gower"
	if(!missing(metric) && metric != "gower" && !all.I)
	    warning("with mixed variables, metric \"gower\" is used automatically")
        ## FIXME: think of a robust alternative scaling to
        ##        Gower's  (x - min(x)) / (max(x) - min(x))
	colR <- apply(x, 2, range, na.rm = TRUE)
	colmin <- colR[1,]
	sx <- colR[2,] - colmin
	if(any(sx == 0))
	    sx[sx == 0] <- 1
	x <- scale(x, center = colmin, scale = sx)
	jdat <- 1L
	ndyst <- 0L
        ## weights only used in this "gower" case
        if(length(weights) == 1)
            weights <- rep.int(weights, p)
        else if(length(weights) != p)
            stop("'weights' must be of length p (or 1)")
    }

    ##	type2 <- paste(type2, collapse = "")
    typeCodes <- c('A','S','N','O','I','T')
    ##              1   2   3   4   5   6  --> passed to Fortran below
    type3 <- match(type2, typeCodes)# integer
    if(any(ina <- is.na(type3)))
	stop("invalid type ", type2[ina],
	     " for column numbers ", pColl(which(is.na)))
    if((mdata <- any(inax <- is.na(x)))) { # TRUE if x[] has any NAs
	jtmd <- as.integer(ifelse(apply(inax, 2, any), -1, 1))
	## VALue for MISsing DATa
	valmisdat <- 1.1* max(abs(range(x, na.rm=TRUE)))
	x[inax] <- valmisdat
	valmd <- rep(valmisdat, p)
    }
    ## call Fortran routine
    storage.mode(x) <- "double"
    disv <- .Fortran(cl_daisy,
		     n,
		     p,
		     x,
		     if(mdata)valmd else double(1),
                     as.double(weights),
		     if(mdata) jtmd else integer(1),
		     jdat,
		     type3,		# vtype
		     ndyst,
		     as.integer(mdata),
		     dis = double((n * (n - 1))/2),
		     NAOK = TRUE,# only to allow "+- Inf"
		     DUP = FALSE)$dis
    ## adapt Fortran output to S:
    ## convert lower matrix, read by rows, to upper matrix, read by rows.
    disv[disv == -1] <- NA
    full <- matrix(0, n, n)
    full[!lower.tri(full, diag = TRUE)] <- disv
    disv <- t(full)[lower.tri(full)]
    ## give warning if some dissimilarities are missimg
    if(any(is.na(disv))) attr(disv, "NA.message") <-
	"NA-values in the dissimilarity matrix !"
    ## construct S object -- "dist" methods are *there* !
    class(disv) <- dissiCl # see ./0aaa.R
    attr(disv, "Labels") <- dimnames(x)[[1]]
    attr(disv, "Size") <- n
    attr(disv, "Metric") <- if(!ndyst) "mixed" else metric
    if(!ndyst) attr(disv, "Types") <- typeCodes[type3]
    disv
}

print.dissimilarity <-
    function(x, diag = NULL, upper = NULL,
	     digits = getOption("digits"), justify = "none", right = TRUE, ...)
{
    cat("Dissimilarities :\n")
    ##orig {Rousseeuw..}: print(as.vector(x), ...)
    stats:::print.dist(x, diag=diag, upper=upper, digits=digits,
                       justify=justify, right=right, ...)
    ##
    cat("\n")
    if(!is.null(attr(x, "na.message")))
	cat("Warning : ", attr(x, "NA.message"), "\n")
    cat("Metric : ", attr(x, "Metric"),
	if(!is.null(aT <- attr(x,"Types")))
	paste(";  Types =", paste(aT, collapse=", ")), "\n")
    cat("Number of objects : ", attr(x, "Size"), "\n", sep="")
    invisible(x)
}

summary.dissimilarity <-
    function(object, digits = max(3, getOption("digits") - 2), ...)
    ## 'digits': want a bit higher precision
{
    sx <- summary(as.vector(object), digits = digits, ...)
    at <- attributes(object)
    r <- c(list(summ = sx, n = length(object)), at[names(at) != "class"])
    class(r) <- "summary.dissimilarity"
    r
}

print.summary.dissimilarity <- function(x, ...)
{
    cat(x$n, "dissimilarities, summarized :\n")
    print(x$summ, ...)
    cat("Metric : ", x $ Metric,
	if(!is.null(aT <- x $ Types))
	paste(";  Types =", paste(aT, collapse=", ")), "\n")
    cat("Number of objects : ", x $ Size, "\n", sep="")
    if(!is.null(x $ na.message))
	cat("Warning : ", x $ NA.message, "\n")
    invisible(x)
}
