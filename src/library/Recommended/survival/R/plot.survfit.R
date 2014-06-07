# Automatically generated from all.nw using noweb
plot.survfit<- function(x, conf.int,  mark.time=TRUE,
                        mark=3, col=1,lty=1, lwd=1, 
                        cex=1, log=FALSE,
                        xscale=1, yscale=1, 
                        firstx=0, firsty=1,
                        xmax, ymin=0,
                        fun, xlab="", ylab="", xaxs='S', ...) {

    dotnames <- names(list(...))
    if (any(dotnames=='type'))
        stop("The graphical argument 'type' is not allowed")
    
    if (inherits(x, "survfitms")) {
        x$surv <- 1- x$prev
        if (is.matrix(x$surv)) dimnames(x$surv) <- list(NULL, x$states)
        if (!is.null(x$lower)) {
            x$lower <- 1- x$lower
            x$upper <- 1- x$upper
        }
        if (missing(fun)) fun <- "event"
    }
    if (is.logical(log)) {
        logy <- log
        logx <- FALSE
        if (logy) logax <- 'y'
        else      logax <- ""
    }
    else {
        logy <- (log=='y' || log=='xy')
        logx <- (log=='x' || log=='xy')
        logax  <- log
    }

    # The special x axis style only applies when firstx is not given
    if (missing(xaxs) && (firstx!=0 || !missing(fun) ||
                          (missing(fun) && inherits(x, "survfitms"))))
        xaxs <- par("xaxs")  #use the default
    ssurv <- as.matrix(x$surv)
    stime <- x$time
    if( !is.null(x$upper)) {
        supper <- as.matrix(x$upper)
        slower <- as.matrix(x$lower)
    }
    else conf.int=FALSE

    # Two cases where we don't put marks at the censoring times
    if (inherits(x, 'survexp') || inherits(x, 'survfit.coxph')) {
        if (missing(mark.time)) mark.time <- FALSE
    }
        
    # set up strata
    if (is.null(x$strata)) {
        nstrat <- 1
        stemp <- rep(1, length(x$time)) # same length as stime
    }
    else {
        nstrat <- length(x$strata)
        stemp <- rep(1:nstrat, x$strata) # same length as stime
    }
    ncurve <- nstrat * ncol(ssurv)
    if (!missing(xmax) && any(x$time>xmax)) {
        # prune back the survival curves
        # I need to replace x's over the limit with xmax, and y's over the
        #  limit with either the prior y value or firsty
        keepx <- keepy <- NULL  # lines to keep
        yzero <- NULL           # if all points on a curve are < xmax
        tempn <- table(stemp)
        offset <- cumsum(c(0, tempn))
        for (i in 1:nstrat) {
            ttime <-stime[stemp==i]
            if (all(ttime <= xmax)) {
                keepx <- c(keepx, 1:tempn[i] + offset[i])
                keepy <- c(keepy, 1:tempn[i] + offset[i])
            }
            else {
                bad <- min((1:tempn[i])[ttime>xmax])
                if (bad==1)  {
                    keepy <- c(keepy, 1+offset[i])
                    yzero <- c(yzero, 1+offset[i])
                    } 
                else  keepy<- c(keepy, c(1:(bad-1), bad-1) + offset[i])
                keepx <- c(keepx, (1:bad)+offset[i])
                stime[bad+offset[i]] <- xmax
                x$n.event[bad+offset[i]] <- 1   #don't plot a tick mark
            }
        }        

        # ok, now actually prune it
        stime <- stime[keepx]
        stemp <- stemp[keepx]
        x$n.event <- x$n.event[keepx]
        if (!is.null(x$n.censor)) x$n.censor <- x$n.censor[keepx]

        if (length(yzero))
            ssurv[yzero,] <- firsty
        ssurv <- ssurv[keepy,,drop=FALSE]
        if (!is.null(supper)) {
            if (length(yzero)) supper[yzero,] <- slower[yzero,] <- firsty
            supper <- supper[keepy,,drop=FALSE]
            slower <- slower[keepy,,drop=FALSE]
        }
    }
    stime <- stime/xscale  #scaling is deferred until xmax processing is done

    if (!missing(fun)) {
        if (is.character(fun)) {
            tfun <- switch(fun,
                           'log' = function(x) x,
                           'event'=function(x) 1-x,
                           'cumhaz'=function(x) -log(x),
                           'cloglog'=function(x) log(-log(x)),
                           'pct' = function(x) x*100,
                           'logpct'= function(x) 100*x,
                           stop("Unrecognized function argument")
                           )
        }
        else if (is.function(fun)) tfun <- fun
        else stop("Invalid 'fun' argument")
        
        ssurv <- tfun(ssurv )
        if (!is.null(supper)) {
            supper <- tfun(supper)
            slower <- tfun(slower)
            }
        firsty <- tfun(firsty)
    }
    if (missing(firsty) && (is.null(x$type) || 
                            x$type=="mright" || x$type=="mcounting")) {
        firsty <- NA
        firstx <- min(stime)
    }
    else if (missing(firstx)) {
        if (!is.null(x$start.time)) 
            firstx <- x$start.time
        else {
            if (logx || (!missing(fun) && is.character(fun) && fun=='cloglog'))
                firstx <- min(stime[stime>0])
            else      firstx <- min(0, stime)
        }
    }

    #The default for plot is to add confidence limits if there is only one curve
    if (missing(conf.int)) conf.int <- (ncurve==1)
    # Marks are not placed on confidence bands
    mark <- rep(mark, length.out=ncurve)
    mcol <- rep(col,  length.out=ncurve)
    if (is.numeric(mark.time)) mark.time <- sort(mark.time)

    # The actual number of curves is ncurve*3 if there are confidence bands
    # If the number of line types is 1 and lty is an integer, then use lty 
    #    for the curve and lty+1 for the CI
    # If the length(lty) <= length(ncurve), use the same color for curve and CI
    #   otherwise assume the user knows what they are about and has given a full
    #   vector of line types.
    # Colors and line widths work like line types, excluding the +1 rule.
    if (conf.int) {
        if (length(lty)==1 && is.numeric(lty))
            lty <- rep(c(lty, lty+1, lty+1), ncurve)
        else if (length(lty) <= ncurve)
            lty <- rep(rep(lty, each=3), length.out=(ncurve*3))
        else lty <- rep(lty, length.out= ncurve*3)
        
        if (length(col) <= ncurve) col <- rep(rep(col, each=3), length.out=3*ncurve)
        else col <- rep(col, length.out=3*ncurve)
        
        if (length(lwd) <= ncurve) lwd <- rep(rep(lwd, each=3), length.out=3*ncurve)
        else lwd <- rep(lwd, length.out=3*ncurve)
    }
    else {
        col  <- rep(col, length.out=ncurve)
        lty  <- rep(lty, length.out=ncurve)
        lwd  <- rep(lwd, length.out=ncurve)
    }
    #axis setting parmaters that depend on the fun argument
    if (!missing(fun)) {
        if (is.character(fun)) {
            if (fun=='log'|| fun=='logpct') logy <- TRUE
            if (fun=='cloglog') {
                logx <- TRUE
                if (logy) logax <- 'xy'
                else logax <- 'x'
            }
        }
        ymin <- tfun(ymin)  #lines routine doesn't have it
    }

    # Do axis range computations
    if (xaxs=='S') {
        #special x- axis style for survival curves
        xaxs <- 'i'  #what S thinks
        tempx <- max(stime) * 1.04
    }
    else tempx <- max(stime)
    tempx <- c(firstx, tempx, firstx)

    if (logy) {
        tempy <-  range(ssurv[is.finite(ssurv)& ssurv>0])
        if (tempy[2]==1) tempy[2] <- .99
        if (any(ssurv==0)) {
            tempy[1] <- tempy[1]*.8
            ssurv[ssurv==0] <- tempy[1]
            if (!is.null(supper)) {
                supper[supper==0] <- tempy[1]
                slower[slower==0] <- tempy[1]
            }
        }
        tempy <- c(tempy, firsty)
    }
    else tempy <- c(range(ssurv[is.finite(ssurv)] ), firsty)
        
    if (missing(fun)) {
        tempx <- c(tempx, firstx)
        tempy <- c(tempy, ymin)
    }

    #
    # Draw the basic box
    #
    plot(tempx, tempy*yscale, type='n', log=logax,
         xlab=xlab, ylab=ylab, xaxs=xaxs,...)

    if(yscale != 1) {
        if (logy) par(usr =par("usr") -c(0, 0, log10(yscale), log10(yscale))) 
        else par(usr =par("usr")/c(1, 1, yscale, yscale))   
    }
    # Create a step function, removing redundancies that sometimes occur in
    #  curves with lots of censoring.
    dostep <- function(x,y) {
        if (is.na(x[1] + y[1])) {
            x <- x[-1]
            y <- y[-1]
        }
        n <- length(x)
        if (n==1)       list(x=x, y=y)
        else if (n==2)  list(x=x[c(1,2,2)], y=y[c(1,1,2)])
        else {
            # replace verbose horizonal sequences like
            # (1, .2), (1.4, .2), (1.8, .2), (2.3, .2), (2.9, .2), (3, .1)
            # with (1, .2), (3, .1).  They are slow, and can smear the looks
            # of the line type.
            dupy <- c(!duplicated(y)[-n], TRUE)
            n2 <- sum(dupy)
            
            #create a step function
            xrep <- rep(x[dupy], c(1, rep(2, n2-1)))
            yrep <- rep(y[dupy], c(rep(2, n2-1), 1))
            list(x=xrep, y=yrep)
        }
    }

    drawmark <- function(x, y, mark.time, censor, ...) {
        if (!is.numeric(mark.time)) {
            xx <- x[censor]
            yy <- y[censor]
        }
        else { #interpolate
            xx <- mark.time
            yy <- approx(x, y, xx, method="constant", f=0)$y
        }
        points(xx, yy, ...)
            
    }
    plot.surv <- TRUE
    c1 <- 1  # keeps track of the curve number
    c2 <- 1  # keeps track of the lty, col, etc
    xend <- yend <- double(ncurve)

    for (i in unique(stemp)) {  #for each strata
        who <- which(stemp==i)
        censor <- if (is.null(x$n.censor))
            (x$n.event[who] ==0)  else (x$n.censor[who] >0) #places with a censoring
        xx <- c(firstx, stime[who])
        censor <- c(FALSE, censor)  #no mark at firstx
        for (j in 1:ncol(ssurv)) {
            yy <- c(firsty, ssurv[who,j])
            if (plot.surv) {
                lines(dostep(xx, yy), lty=lty[c2], col=col[c2], lwd=lwd[c2]) 
                if (is.numeric(mark.time) || mark.time) 
                    drawmark(xx, yy, mark.time, censor, pch=mark[c1], col=mcol[c1])
            }
            xend[c1] <- max(xx)
            yend[c1] <- yy[length(yy)]
            c1 <- c1 +1
            c2 <- c2 +1

            if (conf.int) {
                lines(dostep(xx, c(firsty, slower[who,j])), lty=lty[c2], col=col[c2],
                      lwd=lwd[c2])
                c2 <- c2 +1
                lines(dostep(xx, c(firsty, supper[who,j])), lty=lty[c2], col=col[c2],
                      lwd= lwd[c2])
                c2 <- c2 + 1
            }
        }
    }
    invisible(list(x=xend, y=yend))
}

lines.survfit <- function(x, type='s', 
                          mark=3, col=1, lty=1, lwd=1,
                          mark.time=TRUE, xscale=1, 
                          firstx=0, firsty=1, xmax,
                          fun,  conf.int=FALSE, ...) {
    if (inherits(x, "survfitms")) {
        x$surv <- 1- x$prev
        if (is.matrix(x$surv)) dimnames(x$surv) <- list(NULL, x$states)
        if (!is.null(x$lower)) {
            x$lower <- 1- x$lower
            x$upper <- 1- x$upper
        }
        if (missing(fun)) fun <- "event"
    }
    ssurv <- as.matrix(x$surv)
    stime <- x$time
    if( !is.null(x$upper)) {
        supper <- as.matrix(x$upper)
        slower <- as.matrix(x$lower)
    }
    else conf.int=FALSE

    # Two cases where we don't put marks at the censoring times
    if (inherits(x, 'survexp') || inherits(x, 'survfit.coxph')) {
        if (missing(mark.time)) mark.time <- FALSE
    }
        
    # set up strata
    if (is.null(x$strata)) {
        nstrat <- 1
        stemp <- rep(1, length(x$time)) # same length as stime
    }
    else {
        nstrat <- length(x$strata)
        stemp <- rep(1:nstrat, x$strata) # same length as stime
    }
    ncurve <- nstrat * ncol(ssurv)
    if (!missing(xmax) && any(x$time>xmax)) {
        # prune back the survival curves
        # I need to replace x's over the limit with xmax, and y's over the
        #  limit with either the prior y value or firsty
        keepx <- keepy <- NULL  # lines to keep
        yzero <- NULL           # if all points on a curve are < xmax
        tempn <- table(stemp)
        offset <- cumsum(c(0, tempn))
        for (i in 1:nstrat) {
            ttime <-stime[stemp==i]
            if (all(ttime <= xmax)) {
                keepx <- c(keepx, 1:tempn[i] + offset[i])
                keepy <- c(keepy, 1:tempn[i] + offset[i])
            }
            else {
                bad <- min((1:tempn[i])[ttime>xmax])
                if (bad==1)  {
                    keepy <- c(keepy, 1+offset[i])
                    yzero <- c(yzero, 1+offset[i])
                    } 
                else  keepy<- c(keepy, c(1:(bad-1), bad-1) + offset[i])
                keepx <- c(keepx, (1:bad)+offset[i])
                stime[bad+offset[i]] <- xmax
                x$n.event[bad+offset[i]] <- 1   #don't plot a tick mark
            }
        }        

        # ok, now actually prune it
        stime <- stime[keepx]
        stemp <- stemp[keepx]
        x$n.event <- x$n.event[keepx]
        if (!is.null(x$n.censor)) x$n.censor <- x$n.censor[keepx]

        if (length(yzero))
            ssurv[yzero,] <- firsty
        ssurv <- ssurv[keepy,,drop=FALSE]
        if (!is.null(supper)) {
            if (length(yzero)) supper[yzero,] <- slower[yzero,] <- firsty
            supper <- supper[keepy,,drop=FALSE]
            slower <- slower[keepy,,drop=FALSE]
        }
    }
    stime <- stime/xscale  #scaling is deferred until xmax processing is done

    if (!missing(fun)) {
        if (is.character(fun)) {
            tfun <- switch(fun,
                           'log' = function(x) x,
                           'event'=function(x) 1-x,
                           'cumhaz'=function(x) -log(x),
                           'cloglog'=function(x) log(-log(x)),
                           'pct' = function(x) x*100,
                           'logpct'= function(x) 100*x,
                           stop("Unrecognized function argument")
                           )
        }
        else if (is.function(fun)) tfun <- fun
        else stop("Invalid 'fun' argument")
        
        ssurv <- tfun(ssurv )
        if (!is.null(supper)) {
            supper <- tfun(supper)
            slower <- tfun(slower)
            }
        firsty <- tfun(firsty)
    }

    if (is.logical(conf.int)) plot.surv <- TRUE
    else {
        temp <- pmatch(conf.int, c("both", "only", "none"))
        if (is.na(temp)) stop("invalid value for conf.int")
        if (temp=="none") conf.int <- FALSE  else conf.int <- TRUE
        if (temp=="only") plot.surv <- FALSE  else plot.surv <- TRUE
    }
    
    if (missing(firsty) && (is.null(x$type) || 
                            x$type=="mright" || x$type=="mcounting")) {
        firsty <- NA
        firstx <- min(stime)
    }
    else if (missing(firstx)) {
        if (!is.null(x$start.time)) 
            firstx <- x$start.time
        else  firstx <- min(0, stime)
    }
    # Marks are not placed on confidence bands
    mark <- rep(mark, length.out=ncurve)
    mcol <- rep(col,  length.out=ncurve)
    if (is.numeric(mark.time)) mark.time <- sort(mark.time)

    # The actual number of curves is ncurve*3 if there are confidence bands
    # If the number of line types is 1 and lty is an integer, then use lty 
    #    for the curve and lty+1 for the CI
    # If the length(lty) <= length(ncurve), use the same color for curve and CI
    #   otherwise assume the user knows what they are about and has given a full
    #   vector of line types.
    # Colors and line widths work like line types, excluding the +1 rule.
    if (conf.int) {
        if (length(lty)==1 && is.numeric(lty))
            lty <- rep(c(lty, lty+1, lty+1), ncurve)
        else if (length(lty) <= ncurve)
            lty <- rep(rep(lty, each=3), length.out=(ncurve*3))
        else lty <- rep(lty, length.out= ncurve*3)
        
        if (length(col) <= ncurve) col <- rep(rep(col, each=3), length.out=3*ncurve)
        else col <- rep(col, length.out=3*ncurve)
        
        if (length(lwd) <= ncurve) lwd <- rep(rep(lwd, each=3), length.out=3*ncurve)
        else lwd <- rep(lwd, length.out=3*ncurve)
    }
    else {
        col  <- rep(col, length.out=ncurve)
        lty  <- rep(lty, length.out=ncurve)
        lwd  <- rep(lwd, length.out=ncurve)
    }
    # Create a step function, removing redundancies that sometimes occur in
    #  curves with lots of censoring.
    dostep <- function(x,y) {
        if (is.na(x[1] + y[1])) {
            x <- x[-1]
            y <- y[-1]
        }
        n <- length(x)
        if (n==1)       list(x=x, y=y)
        else if (n==2)  list(x=x[c(1,2,2)], y=y[c(1,1,2)])
        else {
            # replace verbose horizonal sequences like
            # (1, .2), (1.4, .2), (1.8, .2), (2.3, .2), (2.9, .2), (3, .1)
            # with (1, .2), (3, .1).  They are slow, and can smear the looks
            # of the line type.
            dupy <- c(!duplicated(y)[-n], TRUE)
            n2 <- sum(dupy)
            
            #create a step function
            xrep <- rep(x[dupy], c(1, rep(2, n2-1)))
            yrep <- rep(y[dupy], c(rep(2, n2-1), 1))
            list(x=xrep, y=yrep)
        }
    }

    drawmark <- function(x, y, mark.time, censor, ...) {
        if (!is.numeric(mark.time)) {
            xx <- x[censor]
            yy <- y[censor]
        }
        else { #interpolate
            xx <- mark.time
            yy <- approx(x, y, xx, method="constant", f=0)$y
        }
        points(xx, yy, ...)
            
    }
    c1 <- 1  # keeps track of the curve number
    c2 <- 1  # keeps track of the lty, col, etc
    xend <- yend <- double(ncurve)

    for (i in unique(stemp)) {  #for each strata
        who <- which(stemp==i)
        censor <- if (is.null(x$n.censor))
            (x$n.event[who] ==0)  else (x$n.censor[who] >0) #places with a censoring
        xx <- c(firstx, stime[who])
        censor <- c(FALSE, censor)  #no mark at firstx
        for (j in 1:ncol(ssurv)) {
            yy <- c(firsty, ssurv[who,j])
            if (plot.surv) {
                lines(dostep(xx, yy), lty=lty[c2], col=col[c2], lwd=lwd[c2]) 
                if (is.numeric(mark.time) || mark.time) 
                    drawmark(xx, yy, mark.time, censor, pch=mark[c1], col=mcol[c1])
            }
            xend[c1] <- max(xx)
            yend[c1] <- yy[length(yy)]
            c1 <- c1 +1
            c2 <- c2 +1

            if (conf.int) {
                lines(dostep(xx, c(firsty, slower[who,j])), lty=lty[c2], col=col[c2],
                      lwd=lwd[c2])
                c2 <- c2 +1
                lines(dostep(xx, c(firsty, supper[who,j])), lty=lty[c2], col=col[c2],
                      lwd= lwd[c2])
                c2 <- c2 + 1
            }
        }
    }
    invisible(list(x=xend, y=yend))
}

points.survfit <- function(x, xscale=1,
                           xmax, fun, ...) {
    if (inherits(x, "survfitms")) {
        x$surv <- 1- x$prev
        if (is.matrix(x$surv)) dimnames(x$surv) <- list(NULL, x$states)
        if (!is.null(x$lower)) {
            x$lower <- 1- x$lower
            x$upper <- 1- x$upper
        }
        if (missing(fun)) fun <- "event"
    }
    ssurv <- as.matrix(x$surv)
    stime <- x$time
    if( !is.null(x$upper)) {
        supper <- as.matrix(x$upper)
        slower <- as.matrix(x$lower)
    }
    else conf.int=FALSE

    # Two cases where we don't put marks at the censoring times
    if (inherits(x, 'survexp') || inherits(x, 'survfit.coxph')) {
        if (missing(mark.time)) mark.time <- FALSE
    }
        
    # set up strata
    if (is.null(x$strata)) {
        nstrat <- 1
        stemp <- rep(1, length(x$time)) # same length as stime
    }
    else {
        nstrat <- length(x$strata)
        stemp <- rep(1:nstrat, x$strata) # same length as stime
    }
    ncurve <- nstrat * ncol(ssurv)
    if (!missing(xmax) && any(x$time>xmax)) {
        # prune back the survival curves
        # I need to replace x's over the limit with xmax, and y's over the
        #  limit with either the prior y value or firsty
        keepx <- keepy <- NULL  # lines to keep
        yzero <- NULL           # if all points on a curve are < xmax
        tempn <- table(stemp)
        offset <- cumsum(c(0, tempn))
        for (i in 1:nstrat) {
            ttime <-stime[stemp==i]
            if (all(ttime <= xmax)) {
                keepx <- c(keepx, 1:tempn[i] + offset[i])
                keepy <- c(keepy, 1:tempn[i] + offset[i])
            }
            else {
                bad <- min((1:tempn[i])[ttime>xmax])
                if (bad==1)  {
                    keepy <- c(keepy, 1+offset[i])
                    yzero <- c(yzero, 1+offset[i])
                    } 
                else  keepy<- c(keepy, c(1:(bad-1), bad-1) + offset[i])
                keepx <- c(keepx, (1:bad)+offset[i])
                stime[bad+offset[i]] <- xmax
                x$n.event[bad+offset[i]] <- 1   #don't plot a tick mark
            }
        }        

        # ok, now actually prune it
        stime <- stime[keepx]
        stemp <- stemp[keepx]
        x$n.event <- x$n.event[keepx]
        if (!is.null(x$n.censor)) x$n.censor <- x$n.censor[keepx]

        if (length(yzero))
            ssurv[yzero,] <- firsty
        ssurv <- ssurv[keepy,,drop=FALSE]
        if (!is.null(supper)) {
            if (length(yzero)) supper[yzero,] <- slower[yzero,] <- firsty
            supper <- supper[keepy,,drop=FALSE]
            slower <- slower[keepy,,drop=FALSE]
        }
    }
    stime <- stime/xscale  #scaling is deferred until xmax processing is done

    if (!missing(fun)) {
        if (is.character(fun)) {
            tfun <- switch(fun,
                           'log' = function(x) x,
                           'event'=function(x) 1-x,
                           'cumhaz'=function(x) -log(x),
                           'cloglog'=function(x) log(-log(x)),
                           'pct' = function(x) x*100,
                           'logpct'= function(x) 100*x,
                           stop("Unrecognized function argument")
                           )
        }
        else if (is.function(fun)) tfun <- fun
        else stop("Invalid 'fun' argument")
        
        ssurv <- tfun(ssurv )
        if (!is.null(supper)) {
            supper <- tfun(supper)
            slower <- tfun(slower)
            }
        firsty <- tfun(firsty)
    }
    if (ncol(ssurv)==1) points(stime, ssurv, ...)
    else  matpoints(stime, ssurv, ...)
}
