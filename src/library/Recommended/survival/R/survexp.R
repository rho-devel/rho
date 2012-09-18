# Automatically generated from all.nw using noweb
if (!is.R()) setOldClass(c('survexp', 'survfit'))  #set up inheritance

survexp <- function(formula, data,
        weights, subset, na.action, rmap, 
        times,  cohort=TRUE,  conditional=FALSE,
        ratetable=survexp.us, scale=1, npoints, se.fit,
        model=FALSE, x=FALSE, y=FALSE) {
    call <- match.call()
    m <- match.call(expand.dots=FALSE)
        
    # keep the first element (the call), and the following selected arguments
    m <- m[c(1, match(c('formula', 'data', 'weights', 'subset', 'na.action'),
                      names(m), nomatch=0))]
    m[[1]] <- as.name("model.frame")
        
    Terms <- if(missing(data)) terms(formula, 'ratetable')
             else              terms(formula, 'ratetable',data=data)
    rate <- attr(Terms, "specials")$ratetable                   
    if(length(rate) > 1)
            stop("Can have only 1 ratetable() call in a formula")
    if(length(rate) == 1) {
        if (!missing(rmap)) 
            stop("The ratetable() call in a formula is depreciated")

        stemp <- untangle.specials(Terms, 'ratetable')
        rcall <- as.call(parse(text=stemp$var)[[1]])   # as a call object
        rcall[[1]] <- as.name('list')                  # make it a call to list(..
        Terms <- Terms[-stemp$terms]                   # remove from the formula
        }
    else if (!missing(rmap)) {
        rcall <- substitute(rmap)
        if (!is.call(rcall) || rcall[[1]] != as.name('list'))
            stop ("Invalid rcall argument")
        }
    else rcall <- NULL   # A ratetable, but not rcall argument

    # Check that there are no illegal names in rcall, then expand it
    #  to include all the names in the ratetable
    if(is.ratetable(ratetable))   varlist <- attr(ratetable, "dimid")
    else if(inherits(ratetable, "coxph")) {
        ## Remove "log" and such things, to get just the list of
        #   variable names
        varlist <- all.vars(delete.response(ratetable$terms))
        }
    else stop("Invalid rate table")

    temp <- match(names(rcall)[-1], varlist) # 2,3,... are the argument names
    if (any(is.na(temp)))
        stop("Variable not found in the ratetable:", (names(rcall))[is.na(temp)])
        
    if (any(!(varlist %in% names(rcall)))) {
        to.add <- varlist[!(varlist %in% names(rcall))]
        temp1 <- paste(text=paste(to.add, to.add, sep='='), collapse=',')
        if (is.null(rcall)) rcall <- parse(text=paste("list(", temp1, ")"))[[1]]
        else {
            temp2 <- deparse(rcall)
            rcall <- parse(text=paste("c(", temp2, ",list(", temp1, "))"))[[1]]
            }
        }
    # Create a temporary formula, used only in the call to model.frame
    newvar <- all.vars(rcall)
    if (length(newvar) > 0) {
        tform <- paste(deparse(Terms), paste(newvar, collapse='+'), sep='+')
        if (is.R()) m$formula <- as.formula(tform, environment(Terms))
        else m$formula <- as.formula(tform)
        }

    if (is.R())  m <- eval(m, parent.frame())
    else         m <- eval(m, sys.parent())
    n <- nrow(m)
    if (n==0) stop("Data set has 0 rows")
    weights <- model.extract(m, 'weights')
    if (!is.null(weights)) warning("Weights ignored")

    if (any(attr(Terms, 'order') >1))
            stop("Survexp cannot have interaction terms")
    if (!missing(times)) {
        if (any(times<0)) stop("Invalid time point requested")
        if (length(times) >1 )
            if (any(diff(times)<0)) stop("Times must be in increasing order")
        }
    Y <- model.extract(m, 'response')
    no.Y <- is.null(Y)
    if (!no.Y) {
        if (is.matrix(Y)) {
            if (is.Surv(Y) && attr(Y, 'type')=='right') Y <- Y[,1]
            else stop("Illegal response value")
            }
        if (any(Y<0)) stop ("Negative follow up time")
        if (missing(npoints)) temp <- unique(Y)
        else                  temp <- seq(min(Y), max(Y), length=npoints)
        if (missing(times)) newtime <- sort(temp)
        else  newtime <- sort(unique(c(times, temp[temp<max(times)])))
        }
    else conditional <- FALSE
    ovars <- attr(Terms, 'term.labels')
    # rdata contains the variables matching the ratetable
    rdata <- data.frame(eval(rcall, m), stringsAsFactors=TRUE)  
    if (is.ratetable(ratetable)) {
        israte <- TRUE
        if (no.Y) {
            if (missing(times))
               stop("There is no times argument, and no follow-up times are given in the formula")
            else newtime <- sort(unique(times))
            Y <- rep(max(times), n)
            }
        se.fit <- FALSE
        rtemp <- match.ratetable(rdata, ratetable)
        R <- rtemp$R
        }
    else if (inherits(ratetable, 'coxph')) {
        israte <- FALSE
        Terms <- ratetable$terms
        if (!is.null(attr(Terms, 'offset')))
            stop("Cannot deal with models that contain an offset")
        strats <- attr(Terms, "specials")$strata
        if (length(strats))
            stop("survexp cannot handle stratified Cox models")

        if (any(names(m[,rate]) !=  attr(ratetable$terms, 'term.labels')))
             stop("Unable to match new data to old formula")
        R <- model.matrix.coxph(ratetable, data=rdata)

        if (no.Y) {
            if (missing(se.fit)) se.fit <- TRUE
            }
        else se.fit <- FALSE
        }
    else stop("Invalid ratetable")
    if (!cohort) { #individual survival
        if (no.Y) stop("For non-cohort, an observation time must be given")
        if (israte)
             temp <- survexp.fit (1:n, R, Y, max(Y), TRUE, ratetable)
        else temp<- survexp.cfit(1:n, R, Y, FALSE, TRUE, ratetable, FALSE)
        xx <- temp$surv
        names(xx) <- row.names(m)
        na.action <- attr(m, "na.action")
        if (length(na.action)) return(naresid(na.action, xx))
        else return(xx)
        }
    if (length(ovars)==0)  X <- rep(1,n)  #no categories
    else {
        odim <- length(ovars)
        for (i in 1:odim) {
            temp <- m[[ovars[i]]]
            ctemp <- class(temp)
            if (!is.null(ctemp) && ctemp=='tcut')
                stop("Can't use tcut variables in expected survival")
            }
        X <- strata(m[ovars])
        }

    #do the work
    if (israte)
        temp <- survexp.fit(as.numeric(X), R, Y, newtime,
                           conditional, ratetable)
    else {
        temp <- survexp.cfit(as.numeric(X), R, Y, conditional, FALSE,
                           ratetable, se.fit=se.fit)
        newtime <- temp$times
        }
    if (missing(times)) {
        n.risk <- temp$n
        surv <- temp$surv
        if (se.fit) err <- temp$se
        }
    else {
        if (israte) keep <- match(times, newtime)
        else {
            # The result is from a Cox model, and it's list of
            #  times won't match the list requested in the user's call
            # Interpolate the step function, giving survival of 1 and
            #  se of 0 for requested points that precede the Cox fit's
            #  first downward step.  The code is like summary.survfit.
            n <- length(newtime)
            keep <- approx(c(0, newtime), 0:n, xout=times,
                           method='constant', f=0, rule=2)$y
            }

        if (is.matrix(temp$surv)) {
            surv <- (rbind(1,temp$surv))[keep+1,,drop=FALSE]
            n.risk <- temp$n[pmax(1,keep),,drop=FALSE]
            if (se.fit) err <- (rbind(0,temp$se))[keep+1,,drop=FALSE]
            }
        else {
            surv <- (c(1,temp$surv))[keep+1]
            n.risk <- temp$n[pmax(1,keep)]
            if (se.fit) err <- (c(0,temp$se))[keep+1]
            }
        newtime <- times
        }
    newtime <- newtime/scale
    if (is.matrix(surv)) {
        dimnames(surv) <- list(NULL, levels(X))
        out <- list(call=call, surv=surv, n.risk=c(n.risk[,1]),
                        time=newtime)
        if (se.fit) out$std.err <- err
        }
    else {
         out <- list(call=call, surv=c(surv), n.risk=c(n.risk),
                       time=newtime)
         if (se.fit) out$std.err <- c(err)
         }
    if (model) out$model <- m
    else {
        if (x) out$x <- X
        if (y) out$y <- Y
        }
    if (israte && !is.null(rtemp$summ)) out$summ <- rtemp$summ
    if (no.Y) out$method <- 'Ederer'
    else if (conditional) out$method <- 'conditional'
    else                  out$method <- 'cohort'
    if (is.R()) class(out) <- c('survexp', 'survfit')
    else        oldClass(out) <- c('survexp')
    out
}
