# Automatically generated from all.nw using noweb
survfitcoxph.fit <- function(y, x, wt, x2, risk, newrisk, strata, se.fit,
                              survtype, vartype, varmat, id, y2, strata2,
                              unlist=TRUE) {
    if (is.factor(strata)) ustrata <- levels(strata)
    else                   ustrata <- sort(unique(strata))
    nstrata <- length(ustrata)
    survlist <- vector('list', nstrata)

    for (i in 1:nstrata) {
        indx <- which(strata== ustrata[i])
        survlist[[i]] <- agsurv(y[indx,,drop=F], x[indx,,drop=F], 
                                wt[indx], risk[indx],
                                survtype, vartype)
        }

    expand <- function(fit, x2, varmat, se.fit) {
        if (survtype==1) 
            surv <- cumprod(fit$surv)
        else surv <- exp(-fit$cumhaz)

        if (is.matrix(x2) && nrow(x2) >1) {  #more than 1 row in newdata
            fit$surv <- outer(surv, newrisk, '^')
            if (se.fit) {
                varh <- matrix(0., nrow=length(fit$varhaz), ncol=nrow(x2))
                for (i in 1:nrow(x2)) {
                    dt <- outer(fit$cumhaz, x2[i,], '*') - fit$xbar
                    varh[,i] <- (cumsum(fit$varhaz) + rowSums((dt %*% varmat)* dt))*
                        newrisk[i]^2
                    }
                fit$std.err <- sqrt(varh)
                }
            }
        else {
            fit$surv <- surv^newrisk
            if (se.fit) {
                dt <-  outer(fit$cumhaz, c(x2)) - fit$xbar
                varh <- (cumsum(fit$varhaz) + rowSums((dt %*% varmat)* dt)) * 
                    newrisk^2
                fit$std.err <- sqrt(varh)
                }
            }
        fit
        }
    if (missing(id) || is.null(id)) 
        result <- lapply(survlist, expand, x2, varmat, se.fit)
    else {
        onecurve <- function(slist, x2, y2, strata2,  newrisk, se.fit) {
            ntarget <- nrow(x2)  #number of different time intervals
            surv <- vector('list', ntarget)
            n.event <- n.risk <- n.censor <- varh1 <- varh2 <-  time <- surv
            stemp <- as.integer(strata2)
            timeforward <- 0
            for (i in 1:ntarget) {
                slist <- survlist[[stemp[i]]]
                indx <- which(slist$time > y2[i,1] & slist$time <= y2[i,2])
                if (length(indx)==0) {
                    timeforward <- timeforward + y2[i,2] - y2[i,1]
                    # No deaths or censors in user interval.  Possible
                    # user error, but not uncommon at the tail of the curve.
                    }
                else {
                    time[[i]] <- diff(c(y2[i,1], slist$time[indx])) #time increments
                    time[[i]][1] <- time[[i]][1] + timeforward
                    timeforward <- y2[i,2] - max(slist$time[indx])
                
                    if (survtype==1) surv[[i]] <- slist$surv[indx]^newrisk[i]
                    else             surv[[i]] <- slist$hazard[indx]*newrisk[i]

                    n.event[[i]] <- slist$n.event[indx]
                    n.risk[[i]]  <- slist$n.risk[indx]
                    n.censor[[i]]<- slist$n.censor[indx]
                    dt <-  outer(slist$cumhaz[indx], x2[i,]) - slist$xbar[indx,,drop=F]
                    varh1[[i]] <- slist$varhaz[indx] *newrisk[i]^2
                    varh2[[i]] <- rowSums((dt %*% varmat)* dt) * newrisk[i]^2
                    }
                }

            if (survtype==1) surv <- cumprod(unlist(surv))  #increments (K-M)
            else surv <- exp(-cumsum(unlist(surv)))         #hazards

            if (se.fit) 
                list(n=as.vector(table(strata)[stemp[1]]),
                       time=cumsum(unlist(time)),
                       n.risk = unlist(n.risk),
                       n.event= unlist(n.event),
                       n.censor= unlist(n.censor),
                       surv = surv,
                       std.err = sqrt(cumsum(unlist(varh1)) + unlist(varh2)))
            else list(n=as.vector(table(strata)[stemp[1]]),
                       time=cumsum(unlist(time)),
                       n.risk = unlist(n.risk),
                       n.event= unlist(n.event),
                       n.censor= unlist(n.censor),
                       surv = surv)
            }

        if (all(id ==id[1])) {
            result <- list(onecurve(survlist, x2, y2, strata2, newrisk, se.fit))
            }
        else {
            uid <- unique(id)
            result <- vector('list', length=length(uid))
            for (i in 1:length(uid)) {
                indx <- which(id==uid[i])
                result[[i]] <- onecurve(survlist, x2[indx,,drop=FALSE], 
                                         y2[indx,,drop=FALSE], 
                                         strata2[indx],  newrisk[indx], se.fit)
                }
            names(result) <- unique(id)
            }
        }

    if (unlist) {
        if (length(result)==1) { # the no strata case
            if (se.fit)
                result[[1]][c("n", "time", "n.risk", "n.event", "n.censor",
                          "surv", "std.err")]
            else result[[1]][c("n", "time", "n.risk", "n.event", "n.censor",
                          "surv")]
        }
        else {
            temp <-list(n   =    unlist(lapply(result, function(x) x$n)),
                        time=    unlist(lapply(result, function(x) x$time)),
                        n.risk=  unlist(lapply(result, function(x) x$n.risk)),
                        n.event= unlist(lapply(result, function(x) x$n.event)),
                        n.censor=unlist(lapply(result, function(x) x$n.censor)),
                        strata = sapply(result, function(x) length(x$time)))
            names(temp$strata) <- ustrata
            
            if ((missing(id) || is.null(id)) && nrow(x2)>1) {
                 temp$surv <- t(matrix(unlist(lapply(result, 
                                                        function(x) t(x$surv))),
                                       nrow= nrow(x2)))
                 if (se.fit) temp$std.err = t(matrix(unlist(lapply(result,
                                                     function(x) t(x$std.err))),
                                                     nrow= nrow(x2)))
                 }
            else {             
                temp$surv <- unlist(lapply(result, function(x) x$surv))
                if (se.fit) temp$std.err <- unlist(lapply(result, 
                                                        function(x) x$std.err))
                }
            temp
            }
        }
    else {
        names(result) <- ustrata
        result
        }
    }    
