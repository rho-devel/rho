###            Fit a general linear mixed effects model
###
### Copyright 1997-2003  Jose C. Pinheiro,
###                      Douglas M. Bates <bates@stat.wisc.edu>
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

createConLin <-
    function(fixed, data = sys.frame(sys.parent()),
             random = pdSymm(eval(as.call(fixed[-2]))), ...)
{
##    Call <- match.call()
    if(!inherits(fixed, "formula") || length(fixed) != 3) {
        stop("\nfixed-effects model must be a formula of the form \"resp ~ pred\"")
    }
    REML <- FALSE
    reSt <- reStruct(random, REML = REML, data = NULL)
    groups <- getGroupsFormula(reSt)
    if(is.null(groups)) {
        if(inherits(data, "groupedData")) {
            groups <- getGroupsFormula(data)
            groupsL <- rev(getGroupsFormula(data,
                                            asList = TRUE))
            Q <- length(groupsL)
            if(length(reSt) != Q) {		# may need to repeat reSt
                if(length(reSt) != 1) {
                    stop("incompatible lengths for 'random' and grouping factors")
                }
                auxForm <-
                    eval(parse(text = paste("~", deparse(formula(random)[[2]]), "|",
                               deparse(groups[[2]]))))
                reSt <- reStruct(auxForm, REML = REML, data = NULL)
            }
            else {
                names(reSt) <- names(groupsL)
            }
        }
        else {
            stop("'data' must inherit from \"groupedData\" class if 'random' does not define groups")
        }
    }
    ## create an lme structure containing the random effects model
    lmeSt <- lmeStruct(reStruct = reSt)
    ## extract a data frame with enough information to evaluate
    ## fixed, groups, reStruct, corStruct, and varStruct
    dataMix <-
        model.frame(formula = asOneFormula(formula(lmeSt), fixed, groups),
                    data = data, drop.unused.levels = TRUE)
    origOrder <- row.names(dataMix)	# preserve the original order
    ## sort the model.frame by groups and get the matrices and parameters
    ## used in the estimation procedures
    grps <- getGroups(dataMix, eval(parse(text = paste("~1",
					  deparse(groups[[2]]), sep = "|"))))
    ## ordering data by groups
    if(inherits(grps, "factor")) {	# single level
        ##"order" treats a single named argument peculiarly so must split this off
        ord <- order(grps)
        grps <- data.frame(grps)
        row.names(grps) <- origOrder
        names(grps) <- as.character(deparse((groups[[2]])))
    }
    else {
        ord <- do.call("order", grps)
        ## making group levels unique
        for(i in 2:ncol(grps)) {
            grps[, i] <-
                as.factor(paste(as.character(grps[, i - 1]), as.character(grps[, i]),
                                sep = "/"))
            NULL
        }
    }
    grps <- grps[ord,  , drop = FALSE]
    dataMix <- dataMix[ord,  , drop = FALSE]
##    revOrder <- match(origOrder, row.names(dataMix)) # putting in orig. order
    ## obtaining basic model matrices
    N <- nrow(grps)
    Z <- model.matrix(reSt, dataMix)
    ncols <- attr(Z, "ncols")
    Names(lmeSt$reStruct) <- attr(Z, "nams")
    ## keeping the contrasts for later use in predict
    contr <- attr(Z, "contr")
    X <- model.frame(fixed, dataMix)
    auxContr <- lapply(X, function(el)
                       if(inherits(el, "factor")) contrasts(el))
    contr <- c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
    contr <- contr[!unlist(lapply(contr, is.null))]
    X <- model.matrix(fixed, X)
    y <- eval(fixed[[2]], dataMix)
    ncols <- c(ncols, dim(X)[2], 1)
    Q <- ncol(grps)	## creating the condensed linear model
    list(Xy = array(c(Z, X, y), c(N, sum(ncols)),
	 list(row.names(dataMix),
	      c(colnames(Z), colnames(X), deparse(fixed[[2]])))),
         dims = MEdims(grps, ncols), logLik = 0)
}

simulate.lme <-
    function(object, nsim = 1, seed = as.integer(runif(1, 0, .Machine$integer.max)),
             m2, method = c("REML", "ML"), niterEM = c(40, 200),
             useGen = FALSE, ...)
{
    if (inherits(nsim, "lm") || inherits(nsim, "lme"))
        stop("order of arguments in 'simulate.lme' has changed to conform with generic in R-2.2.0", domain = NA)
    ## object is a list of arguments to lme, or an lme object from which the
    ##    call is extracted, to define the null model
    ## m2 is an option list of arguments to lme to define the feared model
    getResults1 <-
        function(conLin, nIter, pdClass, REML, ssq, p, pp1)
        {
            unlist(.C(mixed_combined,
                      as.double(conLin$Xy),
                      as.integer(unlist(conLin$dims)),
                      double(ssq),
                      as.integer(nIter),
                      as.integer(pdClass),
                      as.integer(REML),
                      logLik = double(1),
                      R0 = double(pp1),
                      lRSS = double(1),
                      info = integer(1))[c("info", "logLik")])
        }
    getResults2 <-
        function(conLin, reSt, REML, control)
        {
            lmeSt <- lmeStruct(reStruct = reStruct(reSt, REML = REML))
            attr(lmeSt, "conLin") <- conLin
            lmeSt <- Initialize(lmeSt, data = NULL, groups = NULL, control = control)
            attr(lmeSt, "conLin") <- MEdecomp(attr(lmeSt, "conLin"))
            aMs <- nlminb(c(coef(lmeSt)),
               function(lmePars) -logLik(lmeSt, lmePars),
               control = list(iter.max = control$msMaxIter,
               eval.max = control$msMaxEval,
               trace = control$msVerbose))
            c(info = aMs$flags[1], logLik = -aMs$value)
        }

    if(!exists(".Random.seed", envir = .GlobalEnv))
        runif(1)		     # initialize the RNG if necessary
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)

    if (inherits(object, "lme")) {      # given as an lme object
        fit1 <- object
        object <- as.list(object$call[-1])
    } else {
        object <- as.list(match.call(lme, substitute(object))[ -1 ])
        fit1 <- do.call("lme", object)
    }
    if (length(fit1$modelStruct) > 1) {
        stop("models with \"corStruct\" and/or \"varFunc\" objects not allowed")
    }
    reSt1 <- fit1$modelStruct$reStruct
    condL1 <- do.call("createConLin", object)
    pdClass1 <- unlist(lapply(reSt1, data.class))
    pdClass1 <- match(pdClass1, c("pdSymm", "pdDiag", "pdIdent",
                                  "pdCompSymm", "pdLogChol"), 0) - 1
    control1 <- lmeControl()
    if (!is.null(object$control)) {
        control1[names(object$control)] <- object$control
    }
    control1$niterEM <- niterEM[1]
    sig <- fit1$sigma
    DeltaInv <- pdMatrix(reSt1, factor = TRUE)
    for(i in names(DeltaInv)) {
        DeltaInv[[i]] <- sig * DeltaInv[[i]]
    }
    if (missing(useGen)) {
        useGen <- any(pdClass1 == -1)
    }
    nullD <- condL1$dims
    N <- nullD$N
    Q <- nullD$Q
    p1 <- nullD$ncol[Q + 1]
    pp11 <- p1 * (p1 + 1)
    ycol1 <- sum(nullD$ncol)
    qvec <- nullD$qvec[1:Q]
    ssq1 <- sum(qvec^2)
    csq1 <- cumsum(c(1, qvec[ - Q]))
    csq2 <- cumsum(qvec)
    ngrp <- nullD$ngrps
    ind <- vector("list", Q)
    ## base for creating response
    base <-
        condL1$Xy[, ycol1 - (nullD$ncol[Q + 1]:1), drop = FALSE] %*% fixef(fit1)
    for(i in 1:Q) {
        ind[[i]] <- rep(1:ngrp[i], nullD$ZXlen[[i]])
    }
    value <- list(null = list())
    if (ML <- !is.na(match("ML", method))) {
        value$null$ML <-
            array(0, c(nsim, 2), list(1:nsim, c("info", "logLik")))
    }
    if (REML <- !is.na(match("REML", method))) {
        value$null$REML <-
            array(0, c(nsim, 2), list(1:nsim, c("info", "logLik")))
    }
    attr(value, "call") <- match.call()
    attr(value, "seed") <- seed
    ALT <- FALSE
    if (!missing(m2)) {
        ALT <- TRUE
        if (inherits(m2, "lme")) {            # given as an lme object
            fit2 <- m2
            m2 <- as.list(m2$call[-1])
        } else {
            m2 <- as.list(match.call(lme, substitute(m2))[ -1 ])
            if (is.null(m2$random)) {
                m2$random <- asOneSidedFormula(object$fixed[-2])
            }
            aux <- object
            aux[names(m2)] <- m2
            m2 <- aux
            fit2 <- do.call("lme", m2)
        }
        if (length(fit2$modelStruct) > 1) {
            stop("models with \"corStruct\" and/or \"varFunc\" objects not allowed")
        }
        condL2 <- do.call("createConLin", m2)
        reSt2 <- fit2$modelStruct$reStruct
        control2 <- lmeControl()
        if (!is.null(m2$control)) {
            control2[names(m2$control)] <- m2$control
        }
        control2$niterEM <- niterEM[2]
        pdClass2 <- unlist(lapply(fit2$modelStruct$reStruct, data.class))
        pdClass2 <- match(pdClass2, c("pdSymm", "pdDiag", "pdIdent",
                                      "pdCompSymm", "pdLogChol"), 0) - 1
        useGen <- useGen || any(pdClass2 == -1)
        altD <- condL2$dims
        ssq2 <- sum((altD$qvec[1:altD$Q])^2)
        p2 <- altD$ncol[altD$Q + 1]
        pp12 <- p2 * (p2 + 1)
        ycol2 <- sum(altD$ncol)
        if (ML) {
            value$alt$ML <- value$null$ML
        }
        if (REML) {
            value$alt$REML <- value$null$REML
        }
    }
    for(i in 1:nsim) {
        base2 <- base + rnorm(N, sd = sig)
        for(j in 1:Q) {
            base2 <- base2 +
                ((array(rnorm(ngrp[j] * qvec[j]), c(ngrp[j], qvec[j]),
                        list(1:ngrp[j], NULL)) %*%
                  DeltaInv[[j]])[ind[[j]], , drop = FALSE] * condL1$Xy[,csq1[j]:csq2[j],
                                             drop = FALSE]) %*% rep(1, qvec[j])
        }
        condL1$Xy[, ycol1] <- base2
        if (REML) {
            if (useGen) {
                value$null$REML[i,] <-
                    getResults2(condL1, reSt1, TRUE, control1)
            } else {
                value$null$REML[i,] <-
                    getResults1(condL1, niterEM[1], pdClass1, TRUE, ssq1, p1, pp11)
            }
        }
        if (ML) {
            if (useGen) {
                value$null$ML[i,] <-
                    getResults2(condL1, reSt1, FALSE, control1)
            } else {
                value$null$ML[i,] <-
                    getResults1(condL1, niterEM[1], pdClass1, FALSE, ssq1, p1, pp11)
            }
        }
        if (ALT) {
            condL2$Xy[, ycol2] <- base2
            if (REML) {
                if (useGen) {
                    value$alt$REML[i,] <-
                        getResults2(condL2, reSt2, TRUE, control2)
                } else {
                    value$alt$REML[i,] <-
                        getResults1(condL2, niterEM[2], pdClass2, TRUE, ssq2, p2, pp12)
                }
            }
            if (ML) {
                if (useGen) {
                    value$alt$ML[i,] <-
                        getResults2(condL2, reSt2, FALSE, control2)
                } else {
                    value$alt$ML[i,] <-
                        getResults1(condL2, niterEM[2], pdClass2, FALSE, ssq2, p2, pp12)
                }
            }
        }
    }
    if (ML) {
        value$null$ML[, "logLik"] <-
            N * (log(N) - (1 + log(2*pi)))/2 + value$null$ML[, "logLik"]
        if (ALT) {
            value$alt$ML[, "logLik"] <-
                N * (log(N) - (1 + log(2*pi)))/2 + value$alt$ML[, "logLik"]
        }
    }
    if (REML) {
        value$null$REML[, "logLik"] <-
            (N - p1) * (log(N - p1) - (1 + log(2*pi)))/2 + value$null$REML[, "logLik"]
        if (ALT) {
            value$alt$REML[, "logLik"] <-
                (N - p2) * (log(N - p2) - (1 + log(2*pi)))/2 + value$alt$REML[, "logLik"]
        }
    }
    attr(value, "df") <- p1 + length(coef(reSt1)) + 1
    if (ALT) {
        attr(value, "df") <-
            abs(attr(value, "df") - (p2 + length(coef(reSt2)) + 1))
    }
    attr(value, "useGen") <- useGen
    class(value) <- "simulate.lme"
    assign(".Random.seed", RNGstate, envir = .GlobalEnv)
    value
}

print.simulate.lme <-
    function(x, ...)
{
    ox <- x
    if (is.null(attr(x, "useGen"))) {     # from simulate.lme
        attr(x$null, "dims") <- NULL
        if (!is.null(x$alt)) {
            attr(x$alt, "dims") <- NULL
        }
    } else {
        attr(x, "useGen") <- attr(x, "df") <- NULL
    }
    attr(x, "seed") <- attr(x, "call") <- NULL
    NextMethod()
    invisible(ox)
}


plot.simulate.lme <-
    function(x, form = y ~ x | df * method, df = attr(x, "df"), weights,
             xlab = "Empirical p-value",
             ylab = "Nominal p-value", xlim = c(0.037, 0.963),
             ylim = c(0.037, 0.963), aspect = 1,
             strip = function(...) strip.default(..., style = 1), ...)
{
    ML <- !is.null(x$null$ML)
    if(ML) {
        if (is.null(x$alt$ML))
            stop("plot method only implemented for comparing models")
        okML <- x$null$ML[, "info"] < 8 & x$alt$ML[, "info"] < 8
    }
    REML <- !is.null(x$null$REML)
    if(REML) {
        if (is.null(x$alt$REML))
            stop("plot method only implemented for comparing models")
        okREML <- x$null$REML[, "info"] < 8 & x$alt$REML[, "info"] < 8
    }

    if (is.null(df)) {
        stop("no degrees of freedom specified")
    }
    if ((ldf <- length(df)) > 1) {
        df <- sort(unique(df))
        if (missing(weights)) {
            weights <- rep.int(1/ldf, ldf)
        } else {
	    if (!identical(weights,FALSE) && length(weights) != ldf)
		stop("degrees of freedom and weights must have the same length")
        }
    } else {
        weights <- FALSE
    }
    useWgts <- (length(weights) != 1)

    if (any(df < 0)) {
        stop("negative degrees of freedom not allowed")
    } else {
        if ((ldf == 1) && (df == 0)) {
            stop("more than one degree of freedom is needed when one them is zero.")
        }
    }
    if (ML) {
        MLstat <-
            rev(sort(2 * pmax(0, x$alt$ML[okML, "logLik"] - x$null$ML[okML,"logLik"])))
        MLy <- lapply(df,
                      function(df, x) {
			  if (df > 0) 1 - pchisq(x, df) else 1*(x == 0)
                      }, x = MLstat)
        dfC <- paste("df",df,sep="=")
        if (useWgts) {                      # has weights
            if (ldf == 2) {                   # will interpolate
                MLy <-
                    c(MLy[[1]], weights[1] * MLy[[1]] + weights[2] * MLy[[2]], MLy[[2]])
                MLdf <- rep(c(dfC[1], paste("Mix(",df[1],",",df[2],")",sep=""),
                              dfC[2]), rep(length(MLstat), ldf + 1))
            } else {
                aux <- weights[1] * MLy[[1]]
                auxNam <- paste("Mix(",df[1],sep="")
                for(i in 2:ldf) {
                    aux <- aux + weights[i] * MLy[[i]]
                    auxNam <- paste(auxNam, ",", df[i],sep="")
                }
                auxNam <- paste(auxNam, ")",sep="")
                MLy <- c(unlist(MLy), aux)
                MLdf <- rep(c(dfC, auxNam), rep(length(MLstat), ldf + 1))
            }
            MLx <- rep((1:length(MLstat) - 0.5)/length(MLstat), ldf + 1)
        } else {
            MLy <- unlist(MLy)
            MLdf <- rep(dfC, rep(length(MLstat), ldf))
            MLx <- rep((1:length(MLstat) - 0.5)/length(MLstat), ldf)
        }
        auxInd <- MLdf != "df=0"
        meth <- rep("ML", length(MLy))
        Mdf <- MLdf
    } else {
        MLy <- MLdf <- MLx <- auxInd <- meth <- Mdf <- NULL
    }
    if (REML) {
        REMLstat <- rev(sort(2 * pmax(0, x$alt$REML[okREML, "logLik"] -
                                      x$null$REML[okREML, "logLik"])))
        REMLy <- lapply(df,
                        function(df, x) {
                            if (df > 0) {
                                1 - pchisq(x, df)
                            } else {
                                val <- rep(0, length(x))
                                val[x == 0] <- 1
                                val
                            }
                        }, x = REMLstat)
        dfC <- paste("df",df,sep="=")
        if (useWgts) {                      # has weights
            if (ldf == 2) {                   # will interpolate
                REMLy <-
                    c(REMLy[[1]], weights[1] * REMLy[[1]] + weights[2] * REMLy[[2]], REMLy[[2]])
                REMLdf <- rep(c(dfC[1], paste("Mix(",df[1],",",df[2],")",sep=""),
                                dfC[2]), rep(length(REMLstat), ldf + 1))
            } else {
                aux <- weights[1] * REMLy[[1]]
                auxNam <- paste("Mix(",df[1],sep="")
                for(i in 2:ldf) {
                    aux <- aux + weights[i] * REMLy[[i]]
                    auxNam <- paste(auxNam, ",", df[i],sep="")
                }
                auxNam <- paste(auxNam, ")",sep="")
                REMLy <- c(unlist(REMLy), aux)
                REMLdf <- rep(c(dfC, auxNam), rep(length(REMLstat), ldf + 1))
            }
            REMLx <- rep((1:length(REMLstat) - 0.5)/length(REMLstat), ldf + 1)
        } else {
            REMLy <- unlist(REMLy)
            REMLdf <- rep(dfC, rep(length(REMLstat), ldf))
            REMLx <- rep((1:length(REMLstat) - 0.5)/length(REMLstat), ldf)
        }
        auxInd <- c(auxInd, REMLdf != "df=0")
        meth <- c(meth, rep("REML", length(REMLy)))
        Mdf <- c(Mdf, REMLdf)
    } else {
        REMLy <- REMLdf <- REMLx <- NULL
    }

    meth <- meth[auxInd]
    Mdf <- Mdf[auxInd]
    Mdf <- ordered(Mdf, levels = unique(Mdf))
    frm <- data.frame(x = c(MLx, REMLx)[auxInd], y = c(MLy, REMLy)[auxInd],
                      df = Mdf,  method = meth)
    ##  names(frm$x) <- rep(1, nrow(frm))
    ##  if (df[1] == 0) {
    ##    names(frm$x)[substring(frm$df,1,3) == "Mix"] <- 1 - weights[1]
    ##    if (missing(ylim)) {
    ##      ylim <- c(0.0384, 1)
    ##    }
    ##  }
    xyplot(form, data = frm, type = c('g', 'l'),
           ##	 panel = function(x, y, ...) {
           ##           panel.grid()
           ##           panel.xyplot(x, y, type = "l", ...)
           ##           if ((dfType <- as.double(names(x)[1])) == 1) {
           ##             panel.abline( 0, as.double(names(x)[1]), lty = 2 )
           ##           } else {
           ##             panel.xyplot(c(0,dfType,dfType,1), c(0,dfType,1,1),
           ##                          type="l", lty = 2, col = 1)
           ##           }
           ##         },
           strip = strip, xlab = xlab, ylab = ylab, aspect = aspect,
           xlim = xlim, ylim = ylim, ...)
}
