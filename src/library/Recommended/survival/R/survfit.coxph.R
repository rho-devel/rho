# Automatically generated from all.nw using noweb
survfit.coxph <-
  function(formula, newdata, se.fit=TRUE, conf.int=.95, individual=FALSE,
            type, vartype,
            conf.type=c("log", "log-log", "plain", "none"),
            censor=TRUE, id, newstrata=missing(id),
            na.action=na.pass, ...) {

    Call <- match.call()
    Call[[1]] <- as.name("survfit")  #nicer output for the user
    object <- formula     #'formula' because it has to match survfit

    if (!is.null(attr(object$terms, "specials")$tt))
        stop("The survfit function can not yet process coxph models with a tt term")

    if (missing(type)) {
        # Use the appropriate one from the model
        temp1 <- c("exact", "breslow", "efron")
        survtype <- match(object$method, temp1)
            }
    else {
        temp1 <- c("kalbfleisch-prentice", "aalen", "efron",
                   "kaplan-meier", "breslow", "fleming-harrington",
                   "greenwood", "tsiatis", "exact")
        survtype <- match(match.arg(type, temp1), temp1)
        survtype <- c(1,2,3,1,2,3,1,2,3)[survtype]
        }
    if (missing(vartype)) {
        vartype <- survtype
        }
    else {
        temp2 <- c("greenwood", "aalen", "efron", "tsiatis")
        vartype <- match(match.arg(vartype, temp2), temp2)
        if (vartype==4) vartype<- 2
        }

    if (!se.fit) conf.type <- "none"
    else conf.type <- match.arg(conf.type)
    if (is.null(object$y) || is.null(object[['x']]) ||
        !is.null(object$call$weights) || 
        !is.null(attr(object$terms, 'specials')$strata) ||
        !is.null(attr(object$terms, 'offset'))) {
        
        mf <- model.frame(object)
        }
    else mf <- NULL  #useful for if statements later
    if (is.null(mf)) y <- object[['y']]
    else {
        y <- model.response(mf)
        y2 <- object[['y']]
        if (!is.null(y2) && any(as.matrix(y2) != as.matrix(y)))
            stop("Could not reconstruct the y vector")
        }

    if (is.null(object[['x']])) x <- model.matrix.coxph(object, data=mf)
    else x <- object[['x']]

    n <- nrow(y)
    if (n != object$n[1] || nrow(x) !=n) 
        stop("Failed to reconstruct the original data set")

    if (is.null(mf)) wt <- rep(1., n)
    else {
        wt <- model.weights(mf)
        if (is.null(wt)) wt <- rep(1.0, n)
        }

    type <- attr(y, 'type')
    if (type != 'right' && type != 'counting') 
        stop("Cannot handle \"", type, "\" type survival data")
    missid <- missing(id) # I need this later, and setting id below makes
                          # "missing(id)" always false
    if (!missid) individual <- TRUE
    else if (missid && individual) id <- rep(0,n)
    else id <- NULL

    if (individual && missing(newdata)) {
        warning("the id and/or individual options only make sense with new data")
        individual <- FALSE
    }

    if (individual && type!= 'counting')
        stop("The individual option is  only valid for start-stop data")
    if (!missing(newstrata)) {
        if (!is.logical(newstrata)) stop("newstrata must be TRUE/FALSE")
        if (individual && !newstrata)
            stop("newstrata must be TRUE for the individual or id options")
    }
    else newstrata <- individual

    if (is.null(mf)) offset <- 0
    else {
        offset <- model.offset(mf)
        if (is.null(offset)) offset <- 0
        }
        
    Terms <- object$terms
    temp <- untangle.specials(Terms, 'strata')
    if (length(temp$terms)==0) strata <- rep(0L,n)
    else {
        if (length(temp$vars) ==1) strata <- mf[[temp$vars]]
        else strata <- strata(mf[, temp$vars], shortlabel=TRUE)
        }
    if (!is.null(x) && ncol(x) >0) { #not a ~1 or ~offset(x) model
        tempf <-attr(Terms, "factors")[-attr(Terms, 'response'),,drop=F] 
        stype <- ifelse(colSums(tempf[temp$terms,,drop=FALSE]) >0,
                        attr(Terms, "order"), 0)
        }
    else stype <- 0  #dummy value
    if (is.null(x) || ncol(x)==0) { # a model with ~1 on the right hand side
        # Give it a dummy x so the rest of the code goes through
        #  (This case is really rare)
        x <- matrix(0., nrow=n)
        coef <- 0.0
        varmat <- matrix(0.0,1,1)
        risk <- rep(exp(offset- mean(offset)), length=n)
        }
    else {
        varmat <- object$var
        coef <- ifelse(is.na(object$coefficients), 0, object$coefficients)
        xcenter <- object$means    
        if (is.null(object$frail)) {
            x <- scale(x, center=xcenter, scale=FALSE)    
            risk <- c(exp(x%*% coef + offset - mean(offset)))
            }
       else {
           keep <- !is.na(match(dimnames(x)[[2]], names(coef)))
           x <- x[,keep, drop=F]
    #       varmat <- varmat[keep,keep]  #coxph already has trimmed it
           risk <- exp(object$linear.predictor)
           x <- scale(x, center=xcenter, scale=FALSE)    
           }
        }
    subterms <- function(x, i) {
        dataClasses <- attr(x, "dataClasses")
        predvars <- attr(x, "predvars")
        x <- x[i]
        if (!is.null(predvars)) 
            attr(x, "predvars") <- attr(x, "variables")
        if (!is.null(dataClasses)){
            temp <- dimnames(attr(x, 'factors'))[[1]]
            attr(x, "dataClasses") <- dataClasses[temp]
        }
        x
    }
    temp <- untangle.specials(Terms, 'cluster')
    if (length(temp$vars)) {
        ptemp <- attr(temp, "predvars")
        Terms <- subterms(Terms, -temp$terms)
        stype <- stype[-temp$terms]
    }
    if (missing(newdata)) {
        mf2 <- as.list(object$means)   #create a dummy newdata
        names(mf2) <- names(object$coefficients)
        mf2 <- as.data.frame(mf2)
        }
    else {
        if (!is.null(object$frail))
            stop("Newdata cannot be used when a model has sparse frailty terms")

        Terms2 <- Terms 
        if (!individual)  Terms2 <- delete.response(Terms)
        if (!newstrata && (any(stype>0))) 
            Terms2 <- subterms(Terms2,stype==0) #remove strata and interactions
        if (!is.null(object$xlevels)) { 
            myxlev <- object$xlevels[match(attr(Terms2, "term.labels"),
                                           names(object$xlevels), nomatch=0)]
            if (length(myxlev)==0) myxlev <- NULL
            }
        else myxlev <- NULL
         
        if (is.vector(newdata, "numeric")) {
            if (individual) stop("newdata must be a data frame")
            if (is.null(names(newdata))) {
                stop("Newdata argument must be a data frame")
            }
            newdata <- data.frame(as.list(newdata))
        }
        if (newstrata && missid) 
            mf2 <- model.frame(Terms2, data=newdata, na.action=na.action, xlev=myxlev)
        else {
            tcall <- Call[c(1, match(c('id', "na.action"), names(Call), nomatch=0))]
            tcall$data <- newdata
            tcall$formula <- Terms2
            if (!is.null(object$xlevels)) tcall$xlev <- myxlev
            tcall[[1]] <- as.name('model.frame')
            mf2 <- eval(tcall)
        }
        }
    if (newstrata) {
        temp <- untangle.specials(Terms2, 'strata')
        if (length(temp$vars) >0) {
            strata2 <- strata(mf2[temp$vars], shortlabel=TRUE)
            strata2 <- factor(strata2, levels=levels(strata))
            if (any(is.na(strata2)))
                stop("New data set has strata levels not found in the original")
            Terms2 <- Terms2[-temp$terms]
            }
        else strata2 <- factor(rep(0, nrow(mf2)))
    }

    if (individual) {
        if (missing(newdata)) 
            stop("The newdata argument must be present when individual=TRUE")
        if (!missid) {  #grab the id variable
            id <- model.extract(mf2, "id")
            if (is.null(id)) stop("id=NULL is an invalid argument")
            }
        
        x2 <- model.matrix(Terms2, mf2)[,-1, drop=FALSE]  #no intercept
        if (length(x2)==0) stop("Individual survival but no variables")
        x2 <- scale(x2, center=xcenter, scale=FALSE)

        offset2 <- model.offset(mf2)
        if (length(offset2) >0) offset2 <- offset2 - mean(offset)
        else offset2 <- 0
                    
        y2 <- model.extract(mf2, 'response')
        if (attr(y2,'type') != type)
            stop("Survival type of newdata does not match the fitted model")
        if (attr(y2, "type") != "counting")
            stop("Individual=TRUE is only valid for counting process data")
        y2 <- y2[,1:2, drop=F]  #throw away status, it's never used

        newrisk <- exp(c(x2 %*% coef) + offset2)
        result <- survfitcoxph.fit(y, x, wt, x2, risk, newrisk, strata,
                                    se.fit, survtype, vartype, varmat, 
                                    id, y2, strata2)
       }
    else if (any(stype==2)){
        tframe <- mf[match(levels(strata), strata),]
        temp <- vector('list', nrow(tframe))  #number of strata
        for (i in 1:nrow(tframe)) {
            mf3 <- tframe[rep(i, nrow(mf2)),]
            mf3[names(mf2)] <- mf2
            attr(mf3, 'terms') <- attr(mf, 'terms')
            x2 <- model.matrix(Terms[stype!=1], data=mf3,
                               xlev=object$xlevels)[,-1,drop=FALSE]
            x2 <- scale(x2, center=xcenter, scale=FALSE)
            offset2 <- model.offset(mf3)
            if (is.null(offset2)) offset2 <-0
            newrisk <- c(exp(x2%*%coef) + offset2)
            zed<- survfitcoxph.fit(y, x, wt, x2, risk, newrisk, strata,
                                               se.fit, survtype, vartype, varmat,
                                               id=NULL, unlist=FALSE)
            temp[[i]] <- zed[[i]]
            }
        tfun <- function(x, fun) as.vector(unlist(lapply(x,fun))) #no names
        result <- list(n   =    tfun(temp, function(x) x$n),
                       time=    tfun(temp, function(x) x$time),
                       n.risk=  tfun(temp, function(x) x$n.risk),
                       n.event= tfun(temp, function(x) x$n.event),
                       n.censor=tfun(temp, function(x) x$n.censor),
                       strata = sapply(temp, function(x) length(x$time)))
        names(result$strata) <- levels(strata)
        if (nrow(x2) >1) {  #matrix result
            result$surv =  t(matrix(tfun(temp, function(x) t(x$surv)),
                                    nrow= nrow(x2)))
            if (se.fit) result$std.err =  t(matrix(tfun(temp,function(x) t(x$std)),
                                                   nrow= nrow(x2)))
            }
        else {
            result$surv =  tfun(temp, function(x) x$surv)
            if (se.fit) result$std.err = tfun(temp, function(x) x$std.err)
            }
        }
    else {
        if (missing(newdata)) {
            x2 <- matrix(0.0, nrow=1, ncol=ncol(x))
            offset2 <- 0
        }
        else {
           offset2 <- model.offset(mf2)
           if (length(offset2) >0) offset2 <- offset2 - mean(offset)
           else offset2 <- 0
           x2 <- model.matrix(Terms2, mf2)[,-1, drop=FALSE]  #no intercept
           x2 <- scale(x2, center=xcenter, scale=FALSE)
       }

        newrisk <- exp(c(x2 %*% coef) + offset2)
        result <- survfitcoxph.fit(y, x, wt, x2, risk, newrisk, strata,
                                    se.fit, survtype, vartype, varmat, 
                                    id, y2, strata2)
        if (newstrata) {
            warning("newstrata argument under construction, value ignored")
        }
    }
    if (!censor) {
        kfun <- function(x, keep){ if (is.matrix(x)) x[keep,,drop=F] 
                                  else if (length(x)==length(keep)) x[keep]
                                  else x}
        keep <- (result$n.event > 0)
        if (!is.null(result$strata)) {
            temp <- rep(names(result$strata), result$strata)
            result$strata <- c(table(temp[keep]))
            }
        result <- lapply(result, kfun, keep)
        }

    if (se.fit) {
        zval <- qnorm(1- (1-conf.int)/2, 0,1)
        if (conf.type=='plain') {
            temp1 <- result$surv + zval* result$std.err * result$surv
            temp2 <- result$surv - zval* result$std.err * result$surv
            result <- c(result, list(upper=pmin(temp1,1), lower=pmax(temp2,0),
                            conf.type='plain', conf.int=conf.int))
            }
        if (conf.type=='log') {
            xx <- ifelse(result$surv==0,1,result$surv)  #avoid some "log(0)" messages
            temp1 <- ifelse(result$surv==0, 0*result$std.err, 
                            exp(log(xx) + zval* result$std.err))
            temp2 <- ifelse(result$surv==0, 0*result$std.err, 
                            exp(log(xx) - zval* result$std.err))
            result <- c(result, list(upper=pmin(temp1,1), lower=temp2,
                            conf.type='log', conf.int=conf.int))
            }
        if (conf.type=='log-log') {
            who <- (result$surv==0 | result$surv==1) #special cases
            xx <- ifelse(who, .1,result$surv)  #avoid some "log(0)" messages
            temp1 <- exp(-exp(log(-log(xx)) + zval*result$std.err/log(xx)))
            temp1 <- ifelse(who, result$surv + 0*result$std.err, temp1)
            temp2 <- exp(-exp(log(-log(xx)) - zval*result$std.err/log(xx)))
            temp2 <- ifelse(who, result$surv + 0*result$std.err, temp2)
            result <- c(result, list(upper=temp1, lower=temp2,
                            conf.type='log-log', conf.int=conf.int))
            }
        }

    result$call <- Call

    # The "type" component is in the middle -- match history
    indx <- match('surv', names(result))
    result <- c(result[1:indx], type=attr(y, 'type'), result[-(1:indx)])
    if (is.R()) class(result) <- c('survfit.cox', 'survfit')
    else        oldClass(result) <- 'survfit.cox'
    result
    }
