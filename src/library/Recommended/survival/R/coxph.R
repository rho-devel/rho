# Automatically generated from all.nw using noweb
tt <- function(x) x
coxph <- function(formula, data, weights, subset, na.action,
        init, control, ties= c("efron", "breslow", "exact"),
        singular.ok =TRUE, robust=FALSE,
        model=FALSE, x=FALSE, y=TRUE,  tt, method=ties, ...) {

    ties <- match.arg(ties)
    Call <- match.call()

    # create a call to model.frame() that contains the formula (required)
    #  and any other of the relevant optional arguments
    # then evaluate it in the proper frame
    indx <- match(c("formula", "data", "weights", "subset", "na.action"),
                  names(Call), nomatch=0) 
    if (indx[1] ==0) stop("A formula argument is required")
    temp <- Call[c(1,indx)]  # only keep the arguments we wanted
    temp[[1]] <- as.name('model.frame')  # change the function called
    
    special <- c("strata", "cluster", "tt")
    temp$formula <- if(missing(data)) terms(formula, special)
                    else              terms(formula, special, data=data)
    if (is.R()) m <- eval(temp, parent.frame())
    else        m <- eval(temp, sys.parent())

    if (nrow(m) ==0) stop("No (non-missing) observations")
    Terms <- terms(m)

    
    ## We want to pass any ... args to coxph.control, but not pass things
    ##  like "dats=mydata" where someone just made a typo.  The use of ...
    ##  is simply to allow things like "eps=1e6" with easier typing
    extraArgs <- list(...)
    if (length(extraArgs)) {
        controlargs <- names(formals(coxph.control)) #legal arg names
        indx <- pmatch(names(extraArgs), controlargs, nomatch=0L)
        if (any(indx==0L))
            stop(gettextf("Argument %s not matched", names(extraArgs)[indx==0L]),
                 domain = NA)
    }
    if (missing(control)) control <- coxph.control(...)

    Y <- model.extract(m, "response")
    if (!inherits(Y, "Surv")) stop("Response must be a survival object")
    type <- attr(Y, "type")
    if (type!='right' && type!='counting')
        stop(paste("Cox model doesn't support \"", type,
                          "\" survival data", sep=''))
    weights <- model.weights(m)
    data.n <- nrow(Y)   #remember this before any time transforms
    
    strats <- attr(Terms, "specials")$strata
    if (length(strats)) {
        stemp <- untangle.specials(Terms, 'strata', 1)
        if (length(stemp$terms) >0) #beware strata by covariate interactions
            Terms2 <- Terms[-stemp$terms] #not needed for model.matrix later
        else Terms2 <- Terms  
        if (length(stemp$vars)==1) strata.keep <- m[[stemp$vars]]
        else strata.keep <- strata(m[,stemp$vars], shortlabel=TRUE)
        strats <- as.numeric(strata.keep)
        }
    else Terms2 <- Terms
    
    timetrans <- attr(Terms, "specials")$tt
    if (length(timetrans)) {
        timetrans <- untangle.specials(Terms, 'tt')
        ntrans <- length(timetrans$terms)

        if (missing(tt) || is.null(tt)) {
            tt <- function(x, time, riskset, weights){ #default to O'Brien's logit rank
                obrien <- function(x) {
                    r <- rank(x)
                    (r-.5)/(.5+length(r)-r)
                }
                unlist(tapply(x, riskset, obrien))
            }
        }
        if (is.function(tt)) tt <- list(tt)  #single function becomes a list
            
        if (is.list(tt)) {
            if (any(!sapply(tt, is.function))) 
                stop("The tt argument must contain function or list of functions")
            if (length(tt) != ntrans) {
                if (length(tt) ==1) {
                    temp <- vector("list", ntrans)
                    for (i in 1:ntrans) temp[[i]] <- tt[[1]]
                    tt <- temp
                }
                else stop("Wrong length for tt argument")
            }
        }
        else stop("The tt argument must contain function or list of functions")

        if (ncol(Y)==2) {
            if (length(strats)==0) {
                sorted <- order(-Y[,1], Y[,2])
                newstrat <- rep.int(0L, nrow(Y))
                newstrat[1] <- 1L
                }
            else {
                sorted <- order(strats, -Y[,1], Y[,2])
                #newstrat marks the first obs of each strata
                newstrat <-  as.integer(c(1, 1*(diff(strats[sorted])!=0))) 
                }
            if (storage.mode(Y) != "double") storage.mode(Y) <- "double"
            counts <- .Call(Ccoxcount1, Y[sorted,], 
                            as.integer(newstrat))
            tindex <- sorted[counts$index]
        }
        else {
            if (length(strats)==0) {
                sort.end  <- order(-Y[,2], Y[,3])
                sort.start<- order(-Y[,1])
                newstrat  <- c(1L, rep(0, nrow(Y) -1))
            }
            else {
                sort.end  <- order(strats, -Y[,2], Y[,3])
                sort.start<- order(strats, -Y[,1])
                newstrat  <- c(1L, as.integer(diff(strats[sort.end])!=0))
            }
            if (storage.mode(Y) != "double") storage.mode(Y) <- "double"
            counts <- .Call(Ccoxcount2, Y, 
                            as.integer(sort.start -1L),
                            as.integer(sort.end -1L), 
                            as.integer(newstrat))
            tindex <- counts$index
        }
        m <- m[tindex,]
        Y <- Surv(rep(counts$time, counts$nrisk), counts$status)
        type <- 'right'  # new Y is right censored, even if the old was (start, stop]
        strats <- factor(rep(1:length(counts$nrisk), counts$nrisk)) 
        weights <- model.weights(m)
        for (i in 1:ntrans) 
            m[[timetrans$var[i]]] <- (tt[[i]])(m[[timetrans$var[i]]], Y[,1], strats, 
                                               weights)
        }

    offset <- model.offset(m)
    if (is.null(offset) | all(offset==0)) offset <- rep(0., nrow(m))

    cluster<- attr(Terms, "specials")$cluster
    if (length(cluster)) {
        robust <- TRUE  #flag to later compute a robust variance
        tempc <- untangle.specials(Terms2, 'cluster', 1:10)
        ord <- attr(Terms2, 'order')[tempc$terms]
        if (any(ord>1)) stop ("Cluster can not be used in an interaction")
        cluster <- strata(m[,tempc$vars], shortlabel=TRUE)  #allow multiples
        Terms2 <- Terms2[-tempc$terms]
        }
    else {
        if (!missing(robust)) warning("The robust option is depricated")
        else robust <- FALSE
    }

    attr(Terms2, 'intercept') <- 1  #baseline hazard is always present
    X <- model.matrix(Terms2, m)
    # Attributes of X need to be saved away before the X <- X[,-1] line removes the
    #  intercept, since subscripting removes some of them!
    Xatt <- attributes(X)
    if (is.R()) {
         assign <- lapply(attrassign(X, Terms2)[-1], function(x) x-1)
         xlevels <- .getXlevels(Terms2, m)
         contr.save <- attr(X, 'contrasts')
         }
    else {
        assign <- lapply(attr(X, 'assign')[-1], function(x) x -1)
        xvars <- as.character(attr(Terms2, 'variables'))
        xvars <- xvars[-attr(Terms2, 'response')]
        if (length(xvars) >0) {
                xlevels <- lapply(m[xvars], levels)
                xlevels <- xlevels[!unlist(lapply(xlevels, is.null))]
                if(length(xlevels) == 0)
                        xlevels <- NULL
                }
        else xlevels <- NULL
        contr.save <- attr(X, 'contrasts')
        }

    X <- X[,-1, drop=F]  #remove the intercept column

    if (missing(init)) init <- NULL
    pterms <- sapply(m, inherits, 'coxph.penalty')
    if (any(pterms)) {
        pattr <- lapply(m[pterms], attributes)
        pname <- names(pterms)[pterms]
        # 
        # Check the order of any penalty terms
        ord <- attr(Terms, "order")[match(pname, attr(Terms, 'term.labels'))]
        if (any(ord>1)) stop ('Penalty terms cannot be in an interaction')
        pcols <- assign[match(pname, names(assign))] 
        
        fit <- coxpenal.fit(X, Y, strats, offset, init=init,
                            control,
                            weights=weights, method=method,
                            row.names(m), pcols, pattr, assign)
    }
    else {
        if( method=="breslow" || method =="efron") {
            if (type== 'right')  fitter <- get("coxph.fit")
            else                 fitter <- get("agreg.fit")
        }
        else if (method=='exact') {
            if (type== "right")  fitter <- get("coxexact.fit")
            else  fitter <- get("agexact.fit")
        }
        else stop(paste ("Unknown method", method))

        fit <- fitter(X, Y, strats, offset, init, control, weights=weights,
                      method=method, row.names(m))
    }
    if (is.character(fit)) {
        fit <- list(fail=fit)
        if (is.R()) class(fit) <- 'coxph'
        else oldClass(fit) <- 'coxph'
    }
    else {
        if (!is.null(fit$coefficients) && any(is.na(fit$coefficients))) {
           vars <- (1:length(fit$coefficients))[is.na(fit$coefficients)]
           msg <-paste("X matrix deemed to be singular; variable",
                           paste(vars, collapse=" "))
           if (singular.ok) warning(msg)
           else             stop(msg)
        }
        fit$n <- data.n
        fit$nevent <- sum(Y[,ncol(Y)])
        fit$terms <- Terms
        fit$assign <- assign
        if (is.R()) class(fit) <- fit$method        
        else       oldClass(fit) <-  fit$method[1]
        if (robust) {
            fit$naive.var <- fit$var
            fit$method    <- method
            # a little sneaky here: by calling resid before adding the
            #   na.action method, I avoid having missings re-inserted
            # I also make sure that it doesn't have to reconstruct X and Y
            fit2 <- c(fit, list(x=X, y=Y, weights=weights))
            if (length(strats)) fit2$strata <- strats
            if (length(cluster)) {
                temp <- residuals.coxph(fit2, type='dfbeta', collapse=cluster,
                                          weighted=TRUE)
                # get score for null model
                if (is.null(init))
                        fit2$linear.predictors <- 0*fit$linear.predictors
                else fit2$linear.predictors <- c(X %*% init)
                temp0 <- residuals.coxph(fit2, type='score', collapse=cluster,
                                         weighted=TRUE)
        }
            else {
                temp <- residuals.coxph(fit2, type='dfbeta', weighted=TRUE)
                fit2$linear.predictors <- 0*fit$linear.predictors
                temp0 <- residuals.coxph(fit2, type='score', weighted=TRUE)
        }
            fit$var <- t(temp) %*% temp
            u <- apply(as.matrix(temp0), 2, sum)
            fit$rscore <- coxph.wtest(t(temp0)%*%temp0, u, control$toler.chol)$test
        }
        #Wald test
        if (length(fit$coefficients) && is.null(fit$wald.test)) {  
            #not for intercept only models, or if test is already done
            nabeta <- !is.na(fit$coefficients)
            # The init vector might be longer than the betas, for a sparse term
            if (is.null(init)) temp <- fit$coefficients[nabeta]
            else temp <- (fit$coefficients - 
                          init[1:length(fit$coefficients)])[nabeta]
            fit$wald.test <-  coxph.wtest(fit$var[nabeta,nabeta], temp,
                                          control$toler.chol)$test
        }
        na.action <- attr(m, "na.action")
        if (length(na.action)) fit$na.action <- na.action
        if (model) {
            if (length(timetrans)) {
                # Fix up the model frame -- still in the thinking stage
                m[[".surv."]]   <- Y
                m[[".strata."]] <- strats
                stop("Time transform + model frame: code incomplete")
            }
            fit$model <- m
        }
        if (x)  {
            Xatt$dim <- attr(X, 'dim')
            Xatt$dimnames <- attr(X, 'dimnames')
            Xatt$assign <- Xatt$assign[-1]
            attributes(X) <- Xatt
            fit$x <- X
            if (length(strats)) {
                if (length(timetrans)) fit$strata <- strats
                else     fit$strata <- strata.keep
            }
        }
        if (y)     fit$y <- Y
    }
    if (!is.null(weights) && any(weights!=1)) fit$weights <- weights
    names(fit$means) <- names(fit$coefficients)

    fit$formula <- formula(Terms)
    if (length(xlevels) >0) fit$xlevels <- xlevels
    fit$contrasts <- contr.save
    if (any(offset !=0)) fit$offset <- offset
    fit$call <- Call
    fit$method <- method
    fit
    }
