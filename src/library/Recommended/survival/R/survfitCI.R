# Automatically generated from all.nw using noweb
survfitCI <- function(X, Y, weights, id, istate, 
                      type=c('kaplan-meier', 'fleming-harrington', 'fh2'),
                      se.fit=TRUE,
                      conf.int= .95,
                      conf.type=c('log',  'log-log',  'plain', 'none'),
                      conf.lower=c('usual', 'peto', 'modified')){

    method <- match.arg(type)
#    error <- match.arg(error)
#    if (error != "inf")
#        warning("Only the infinetesimal jackknife error is supported for CI curves")
    conf.type <- match.arg(conf.type)
    conf.lower<- match.arg(conf.lower)
    if (is.logical(conf.int)) {
        # A common error is for users to use "conf.int = FALSE"
        #  it's illegal per documentation, but be kind
        if (!conf.int) conf.type <- "none"
        conf.int <- .95
    }

    type <- attr(Y, "type")
    if (type !='mright' && type!='mcounting' && 
        type != "right" && type != "counting")
        stop(paste("Cumulative incidence computation doesn't support \"", type,
                          "\" survival data", sep=''))

    n <- nrow(Y)
    status <- Y[,ncol(Y)]
    ncurve <- length(levels(X))
    
    state.names <- attr(Y, "states")
    if (missing(istate) || is.null(istate)) istate <- rep(0L, n)
    else if (is.factor(istate) || is.character(istate)) {
        # Match levels with the survival variable
        temp <- as.factor(istate)
        state.names <- unique(c(attr(Y, "states"), levels(istate)))
        istate <- as.numeric(factor(as.character(istate), levels=state.names))
    }
    else if (!is.numeric(istate) || any(istate != floor(istate)))
        stop("istate should be a vector of integers or a factor")
    
    if (length(id) ==0) id <- 1:n
    # these next two lines should be impossible, since istate came from the data frame
    if (length(istate) ==1) istate <- rep(istate,n)
    if (length(istate) !=n) stop ("wrong length for istate")

    states <- sort(unique(c(istate, 1:length(attr(Y, "states"))))) #list of all
    docurve2 <- function(entry, etime, status, istate, wt, states, id, se.fit) {
        #
        # round off error can cause trouble, if two times are within machine
        #  precsion
        # solve this by using creating a factor
        ftime <- factor(c(entry,etime))
        ltime <- levels(ftime)
        ftime <- matrix(as.integer(ftime), ncol=2)
        n <- length(entry)
        timeset <- as.numeric(ltime[sort(unique(ftime[,2]))]) #unique event times
         
        nstate <- length(states)
        uid <- sort(unique(id))
        P <- as.vector(tapply(wt, factor(istate, levels=states), sum) / sum(wt))
        P <- ifelse(is.na(P), 0, P) # initial probability distribution
        cstate <- istate[match(uid, id)]   #current state for each observation
        
        storage.mode(wt) <- "double" # just in case someone had integer weights
        storage.mode(cstate) <- "integer"
        storage.mode(status) <- "integer"
        # C code has 0 based subscripts
        fit <- .Call(Csurvfitci, ftime, 
                     order(ftime[,1]) - 1L,
                     order(ftime[,2]) - 1L,
                     length(timeset),
                     status,
                     cstate - 1L,
                     wt,
                     match(id, uid) -1L,
                     P, as.integer(se.fit))
        if (se.fit) 
            list(time=timeset, pmat=t(fit$p), std=sqrt(t(fit$var)),
                 n.risk = colSums(fit$nrisk),n.event = fit$nevent, 
                 n.censor=fit$ncensor, 
                 cumhaz=array(fit$cumhaz, dim=c(nstate,nstate, length(timeset))))
        else list(time=timeset, pmat=t(fit$p),
                 n.risk = colSums(fit$nrisk),n.event = fit$nevent, 
                 n.censor=fit$ncensor, 
                 cumhaz=array(fit$cumhaz, dim=c(nstate,nstate, length(timeset))))
    }
    if (any(states==0)) {
        state0 <- TRUE
        states <- states + 1
        istate <- istate + 1
        status <- ifelse(status==0, 0, status+1)
        }
    else state0 <- FALSE
    
    curves <- vector("list", ncurve)
    names(curves) <- levels(X)
                            
    if (ncol(Y)==2) {  # 1 transition per subject
        indx <- which(status == istate & status!=0)
        if (length(indx)) {
            warning("an observation transitions to it's starting state, ignored")
            status[indx] <- 0
        }
        if (length(id) && any(duplicated(id)))
            stop("Cannot have duplicate id values with (time, status) data")

        # dummy entry time that is < any event time
        entry <- rep(min(-1, 2*min(Y[,1])-1), n)  
        for (i in levels(X)) {
            indx <- which(X==i)
 #           temp  <- docurve1(entry[indx], Y[indx,1], status[indx], 
 #                                   istate[indx], weights[indx], states, 
 #                                   id[indx])
            curves[[i]] <- docurve2(entry[indx], Y[indx,1], status[indx], 
                                    istate[indx], weights[indx], states, 
                                    id[indx], se.fit)
         }
    }
    else {
        if (missing(id) || is.null(id))
            stop("the id argument is required for start:stop data")

        indx <- order(id, Y[,2])  #ordered event times within subject
        indx1 <- c(NA, indx)  #a pair of lagged indices
        indx2 <- c(indx, NA)
        same <- (id[indx1] == id[indx2] & !is.na(indx1) & !is.na(indx2)) #indx1, indx2= same id?
        if (any(same & X[indx1] != X[indx2])) {
            who <- 1 + min(which(same & X[indx1] != X[indx2]))
            stop("subject is in two different groups, id ", (id[indx1])[who])
        }
        if (any(same & Y[indx1,2] != Y[indx2,1])) {
            who <- 1 + min(which(same & Y[indx1,2] != Y[indx2,1]))
            stop("gap in follow-up, id ", (id[indx1])[who])
        }
        if (any(Y[,1] == Y[,2])) 
            stop("cannot have start time == stop time")

        if (any(same & Y[indx1,3] == Y[indx2,3] & Y[indx1,3] !=0)) {
            who <-  1 + min(which(same & Y[indx1,1] != Y[indx2,2]))
            stop("subject changes to the same state, id ", (id[indx1])[who])
        }
        if (any(same & weights[indx1] != weights[indx2])) {
            who <-  1 + min(which(same & weights[indx1] != weights[indx2]))
            stop("subject changes case weights, id ", (id[indx1])[who])
        }
        # We only want to pay attention to the istate variable for the very first
        #  observation of any given subject, but the program logic does better with
        #  a full one.  So construct one that will do this
        indx <- order(Y[,2])
        uid <- unique(id)
        temp <- (istate[indx])[match(uid, id[indx])]  #first istate for each subject
        istate <- temp[match(id, uid)]  #replicate it to full length

        # Now to work
        for (i in levels(X)) {
            indx <- which(X==i)
        #    temp <- docurve1(Y[indx,1], Y[indx,2], status[indx], 
        #                          istate[indx], weights[indx], states, id[indx])
            curves[[i]] <- docurve2(Y[indx,1], Y[indx,2], status[indx], 
                                  istate[indx], weights[indx], states, id[indx], se.fit)
        }
    }

    # Turn the result into a survfit type object
    grabit <- function(clist, element) {
        temp <-(clist[[1]][[element]]) 
        if (is.matrix(temp)) {
            nc <- ncol(temp)
            matrix(unlist(lapply(clist, function(x) t(x[[element]]))),
                            byrow=T, ncol=nc)
            }
        else as.vector(unlist(lapply(clist, function(x) x[element])))
        }
    kfit <- list(n =      as.vector(table(X)),
                 time =   grabit(curves, "time"),
                 n.risk=  grabit(curves, "n.risk"),
                 n.event= grabit(curves, "n.event"),
                 n.censor=grabit(curves, "n.censor"),
                 prev   = grabit(curves, "pmat"))
    nstate <- length(states)
    kfit$cumhaz <- array(unlist(lapply(curves, function(x) x$cumhaz)),
                               dim=c(nstate, nstate, length(kfit$time)))
    if (length(curves) >1)
        kfit$strata <- unlist(lapply(curves, function(x) length(x$time)))
    if (se.fit) kfit$std.err <- grabit(curves, "std")

    # if state 0 was present, remove it
    if (state0) {
        kfit$prev <- kfit$prev[,-1]
        if (se.fit) kfit$std.err <- kfit$std.err[,-1]
    }
    #       
    # Last bit: add in the confidence bands:
    #   modeled on survfit.km, though for P instead of S
    #   
    #
    if (se.fit) {
        std.err <- kfit$std.err
        zval <- qnorm(1- (1-conf.int)/2, 0,1)
        surv <- 1-kfit$prev

        if (conf.type=='plain') {
            temp <- zval* std.err
            kfit <- c(kfit, list(lower =pmax(kfit$prev-temp, 0), 
                                 upper=pmin(kfit$prev+temp, 1),
                             conf.type='plain', conf.int=conf.int))
            }

        if (conf.type=='log') {
            #avoid some "log(0)" messages
            xx <- ifelse(kfit$prev==1, 1, 1- kfit$prev)  

            temp1 <- ifelse(surv==0, NA, exp(log(xx) + zval* std.err/xx))
            temp2 <- ifelse(surv==0, NA, exp(log(xx) - zval* std.err/xx))
            kfit <- c(kfit, list(lower=pmax(1-temp1,0), upper= 1- temp2,
                             conf.type='log', conf.int=conf.int))
            }

        if (conf.type=='log-log') {
            who <- (surv==0 | surv==1) #special cases
            temp3 <- ifelse(surv==0, NA, 1)
            xx <- ifelse(who, .1,kfit$surv)  #avoid some "log(0)" messages
            temp1 <- exp(-exp(log(-log(xx)) + zval*std.err/(xx*log(xx))))
            temp1 <- ifelse(who, temp3, temp1)
            temp2 <- exp(-exp(log(-log(xx)) - zval*std.err/(xx*log(xx))))
            temp2 <- ifelse(who, temp3, temp2)
            kfit <- c(kfit, list(lower=1-temp1, upper=1-temp2,
                             conf.type='log-log', conf.int=conf.int))
            }
        }

    kfit$states <- state.names
    kfit$type   <- attr(Y, "type")
    kfit
}
