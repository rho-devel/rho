# Automatically generated from all.nw using noweb
pyears <- function(formula, data,
        weights, subset, na.action, rmap,
        ratetable, scale=365.25,  expect=c('event', 'pyears'),
        model=FALSE, x=FALSE, y=FALSE, data.frame=FALSE) {

    expect <- match.arg(expect)
    call <- match.call()
    m <- match.call(expand.dots=FALSE)
    m <- m[c(1, match(c('formula', 'data', 'weights', 'subset', 'na.action'),
                      names(m), nomatch=0))]
    m[[1]] <- as.name("model.frame")

    Terms <- if(missing(data)) terms(formula, 'ratetable')
             else              terms(formula, 'ratetable',data=data)
    if (any(attr(Terms, 'order') >1))
            stop("Pyears cannot have interaction terms")

    rate <- attr(Terms, "specials")$ratetable                   
    if (length(rate) >0 || !missing(rmap) || !missing(ratetable)) {
        has.ratetable <- TRUE
        if(length(rate) > 1)
            stop("Can have only 1 ratetable() call in a formula")
        if (missing(ratetable)) stop("No rate table specified")

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
        }
    else has.ratetable <- FALSE

    if (is.R())  m <- eval(m, parent.frame())
    else         m <- eval(m, sys.parent())

    Y <- model.extract(m, 'response')
    if (is.null(Y)) stop ("Follow-up time must appear in the formula")
    if (!is.Surv(Y)){
        if (any(Y <0)) stop ("Negative follow up time")
        Y <- as.matrix(Y)
        if (ncol(Y) >2) stop("Y has too many columns")
        if (ncol(Y)==2 && any(Y[,2] <= Y[,1]))
            stop("stop time must be > start time")
        }
    else {
        stype <- attr(Y, 'type')
        if (stype == 'right') {
            if (any(Y[,1] <0)) stop("Negative survival time")
            nzero <- sum(Y[,1]==0 & Y[,2] ==1)
            if (nzero >0) 
                warning(paste(nzero, 
                         "observations with an event and 0 follow-up time,",
                       "any rate calculations are statistically questionable"))
            }
        else if (stype != 'counting')
            stop("Only right-censored and counting process survival types are supported")
        }

    n <- nrow(Y)
    if (is.null(n) || n==0) stop("Data set has 0 observations")

    weights <- model.extract(m, 'weights')
    if (is.null(weights)) weights <- rep(1.0, n)
    # rdata contains the variables matching the ratetable
    if (has.ratetable) {
        rdata <- data.frame(eval(rcall, m), stringsAsFactors=TRUE)  
        if (is.ratetable(ratetable)) {
            israte <- TRUE
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
                stop("pyears cannot handle stratified Cox models")

            if (any(names(m[,rate]) !=  attr(ratetable$terms, 'term.labels')))
                 stop("Unable to match new data to old formula")
            R <- model.matrix.coxph(ratetable, data=rdata)
            }
        else stop("Invalid ratetable")
        }
    ovars <- attr(Terms, 'term.labels')
    if (length(ovars)==0)  {
        # no categories!
        X <- rep(1,n)
        ofac <- odim <- odims <- ocut <- 1
        }
    else {
        odim <- length(ovars)
        ocut <- NULL
        odims <- ofac <- double(odim)
        X <- matrix(0, n, odim)
        outdname <- vector("list", odim)
        for (i in 1:odim) {
            temp <- m[[ovars[i]]]
            if (inherits(temp, 'tcut')) {
                X[,i] <- temp
                temp2 <- attr(temp, 'cutpoints')
                odims[i] <- length(temp2) -1
                ocut <- c(ocut, temp2)
                ofac[i] <- 0
                outdname[[i]] <- attr(temp, 'labels')
                }
            else {
                temp2 <- as.factor(temp)
                X[,i] <- temp2
                temp3 <- levels(temp2)
                odims[i] <- length(temp3)
                ofac[i] <- 1
                outdname[[i]] <- temp3
                }
        }
    }
    ocut <-c(ocut,0)   #just in case it were of length 0
    osize <- prod(odims)
    if (has.ratetable) {  #include expected
        atts <- attributes(ratetable)
        cuts <- atts$cutpoints
        if (is.null(atts$type)) {
            #old stlye table
            rfac <- atts$factor
            us.special <- (rfac >1)
            }
        else {
            rfac <- 1*(atts$type ==1)
            us.special <- (atts$type==4)
            }
        if (any(us.special)) {  #special handling for US pop tables
            # Now, the 'entry' date on a US rate table is the number of days 
            #  since 1/1/1960, and the user data has been aligned to the
            #  same system by match.ratetable and marked as "year".
            # The birth date is entry date - age in days (based on 1/1/1960).
            # I don't much care which date functions I use to do the arithmetic
            #  below.  Unfortunately R and Splus don't share one.  My "date"
            #  class is simple, but is also one of the earlier date class
            #  attempts, has less features than others, and will one day fade
            #  away; so I don't want to depend on it alone.
            #
            cols <- match(c("age", "year"), atts$dimid)
                  if (any(is.na(cols))) 
                 stop("Ratetable does not have expected shape")
            if (exists("as.Date")) {  # true for modern version of R
                bdate <- as.Date('1960/1/1') + (R[,cols[2]] - R[,cols[1]])
                byear <- format(bdate, "%Y")
                offset <- bdate - as.Date(paste(byear, "01/01", sep='/'), 
                                          origin="1960/01/01")
                }
            #else if (exists('month.day.year')) { # Splus, usually
            #    bdate <- R[,cols[2]] - R[,cols[1]]
            #    byear <- month.day.year(bdate)$year
            #    offset <- bdate - julian(1,1,byear)
            #    }
            #else if (exists('date.mdy')) { # Therneau's date class is available
            #    bdate <- as.date(R[,cols[2]] - R[,cols[1]])
            #    byear <- date.mdy(bdate)$year
            #    offset <- bdate - mdy.date(1,1,byear)
            #    }
            else stop("Can't find an appropriate date class\n") 
            R[,cols[2]] <- R[,cols[2]] - offset

            # Doctor up "cutpoints" - only needed for old style rate tables
            #  for which the C code does interpolation on the fly
            if (any(rfac >1)) {
                temp <-  which(us.special)
                nyear <- length(cuts[[temp]])
                nint <- rfac[temp]       #intervals to interpolate over
                cuts[[temp]] <- round(approx(nint*(1:nyear), cuts[[temp]],
                                        nint:(nint*nyear))$y - .0001)
                }
            }

        temp <- .C(Cpyears1,
                        as.integer(n),
                        as.integer(ncol(Y)),
                        as.integer(is.Surv(Y)),
                        as.double(Y),
                        as.double(weights),
                        as.integer(length(atts$dim)),
                        as.integer(rfac),
                        as.integer(atts$dim),
                        as.double(unlist(cuts)),
                        as.double(ratetable),
                        as.double(R),
                        as.integer(odim),
                        as.integer(ofac),
                        as.integer(odims),
                        as.double(ocut),
                        as.integer(expect=='event'),
                        as.double(X),
                        pyears=double(osize),
                        pn    =double(osize),
                        pcount=double(if(is.Surv(Y)) osize else 1),
                        pexpect=double(osize),
                        offtable=double(1),
                    DUP=FALSE)[18:22]
        }
    else {   #no expected
        temp <- .C(Cpyears2,
                        as.integer(n),
                        as.integer(ncol(Y)),
                        as.integer(is.Surv(Y)),
                        as.double(Y),
                        as.double(weights),
                        as.integer(odim),
                        as.integer(ofac),
                        as.integer(odims),
                        as.double(ocut),
                        as.double(X),
                        pyears=double(osize),
                        pn    =double(osize),
                        pcount=double(if(is.Surv(Y)) osize else 1),
                        offtable=double(1)) [11:14]
        }
    if (data.frame) {
        # Create a data frame as the output, rather than a set of
        #  rate tables
        keep <- (temp$pyears >0)  # what rows to keep in the output
        names(outdname) <- ovars
        if (length(outdname) ==1) {
            # if there is only one variable, the call to "do.call" loses
            #  the variable name, since expand.grid returns a factor
            df <- data.frame((outdname[[1]])[keep], 
                             pyears= temp$pyears[keep]/scale,
                             n = temp$pn[keep])
            names(df) <- c(names(outdname), 'pyears', 'n')
            }
        else {
            df <- cbind(do.call("expand.grid", outdname)[keep,],
                             pyears= temp$pyears[keep]/scale,
                             n = temp$pn[keep])
            }
        row.names(df) <- 1:nrow(df)
        if (has.ratetable) df$expected <- temp$pexpect[keep]
        if (expect=='pyears') df$expected <- df$expected/scale
        if (is.Surv(Y)) df$event <- temp$pcount[keep]

        out <- list(call=call,
                    data= df, offtable=temp$offtable/scale)  
        if (has.ratetable && !is.null(rtemp$summ))
            out$summary <- rtemp$summ
        }

    else if (prod(odims) ==1) {  #don't make it an array
        out <- list(call=call, pyears=temp$pyears/scale, n=temp$pn,
                    offtable=temp$offtable/scale)
        if (has.ratetable) {
            out$expected <- temp$pexpect
            if (expect=='pyears') out$expected <- out$expected/scale
            if (!is.null(rtemp$summ)) out$summary <- rtemp$summ
            }
        if (is.Surv(Y)) out$event <- temp$pcount
        }
    else {
        out <- list(call = call,
                pyears= array(temp$pyears/scale, dim=odims, dimnames=outdname),
                n     = array(temp$pn,     dim=odims, dimnames=outdname),
                offtable = temp$offtable/scale)
        if (has.ratetable) {
            out$expected <- array(temp$pexpect, dim=odims, dimnames=outdname)
            if (expect=='pyears') out$expected <- out$expected/scale
            if (!is.null(rtemp$summ)) out$summary <- rtemp$summ
            }
        if (is.Surv(Y))
                out$event <- array(temp$pcount, dim=odims, dimnames=outdname)
        }
    na.action <- attr(m, "na.action")
    if (length(na.action))  out$na.action <- na.action
    if (model) out$model <- m
    else {
        if (x) out$x <- X
        if (y) out$y <- Y
        }
    oldClass(out) <- 'pyears'
    out
    }
