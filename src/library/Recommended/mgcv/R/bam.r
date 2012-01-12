## routines for very large dataset generalized additive modelling.
## (c) Simon N. Wood 2009 


ls.size <- function(x) {
## If `x' is a list, return the size of its elements, in bytes, in a named array
## otherwise return the size of the object
 if (is.list(x)==FALSE) return(object.size(x))

 xn <- names(x)
 n <- length(x)
 sz <- rep(-1,n)
 for (i in 1:n) sz[i] <- object.size(x[[i]])
 names(sz) <- xn
 sz
}

rwMatrix <- function(stop,row,weight,X) {
## Routine to recombine the rows of a matrix X according to info in 
## stop,row and weight. Consider the ith row of the output matrix 
## ind <- 1:stop[i] if i==1 and ind <- (stop[i-1]+1):stop[i]
## otherwise. The ith output row is then X[row[ind],]*weight[ind]
  if (is.matrix(X)) { n <- nrow(X);p<-ncol(X);ok <- TRUE} else { n<- length(X);p<-1;ok<-FALSE}
  stop <- stop - 1;row <- row - 1 ## R indeces -> C indeces
  oo <-.C(C_rwMatrix,as.integer(stop),as.integer(row),as.double(weight),X=as.double(X),as.integer(n),as.integer(p))
  if (ok) return(matrix(oo$X,n,p)) else
  return(oo$X) 
}


qr.update <- function(Xn,yn,R=matrix(0,0,ncol(Xn)),f=array(0,0),y.norm2=0)
## Let X = QR and f = Q'y. This routine updates f and R
## when Xn is appended to X and yn appended to y. If R has no rows
## then initial QR of Xn is performed. ||y||^2 is accumulated as well
{ #qrx <- qr(Xn)
  p <- ncol(Xn)
  #fn <- qr.qty(qrx,yn)[1:p] 
  y.norm2 <- y.norm2+sum(yn*yn)
  if (nrow(R)) {
    Xn <- rbind(R,Xn)
    yn <- c(f,yn)
  }
  qrx <- qr(Xn,tol=0)
  fn <- qr.qty(qrx,yn)[1:p]

  list(R = qr.R(qrx),f=fn,y.norm2=y.norm2)
}

mini.mf <-function(mf,chunk.size) {
## takes a model frame and produces a representative subset of it, suitable for 
## basis setup.
  n <- nrow(mf)
  if (n<=chunk.size) return(mf)
  seed <- try(get(".Random.seed",envir=.GlobalEnv),silent=TRUE) ## store RNG seed
  if (inherits(seed,"try-error")) {
     runif(1)
     seed <- get(".Random.seed",envir=.GlobalEnv)
  }
  kind <- RNGkind(NULL)
  RNGkind("default", "default")
  set.seed(66)
  ind <- sample(1:n,chunk.size)
  RNGkind(kind[1], kind[2])
  assign(".Random.seed", seed, envir = .GlobalEnv)
  mf0 <- mf[ind,] ## random sample of rows
  ## now need to ensure that max and min are in sample for each element of mf0
  ## note that min and max might occur twice, but this shouldn't matter (and
  ## is better than min overwriting max, for example)
  for (j in 1:length(mf)) if (is.numeric(mf0[[j]])) {
    if (is.matrix(mf0[[j]])) { ## find row containing minimum
      j.min <- min((1:n)[as.logical(rowSums(mf[[j]]==min(mf[[j]])))])
      j.max <- min((1:n)[as.logical(rowSums(mf[[j]]==max(mf[[j]])))])
      mf0[[j]][1,] <- mf[[j]][j.min,]
      mf0[[j]][2,] <- mf[[j]][j.max,] 
    } else { ## vector
      mf0[[j]][1] <- min(mf[[j]])
      mf0[[j]][2] <- max(mf[[j]]) 
    }
  }
  mf0
}

bgam.fit <- function (G, mf, chunk.size, gp ,scale ,gamma,method, etastart = NULL,
    mustart = NULL, offset = rep(0, nobs),
    control = gam.control(), intercept = TRUE)
{   y <- mf[[gp$response]]
    weights <- G$w
    conv <- FALSE
    nobs <- nrow(mf)
    nvars <- ncol(G$X)
    offset <- G$offset
    family <- G$family
    G$family <- gaussian() ## needed if REML/ML used
    variance <- family$variance
    dev.resids <- family$dev.resids
    aic <- family$aic
    linkinv <- family$linkinv
    mu.eta <- family$mu.eta
    if (!is.function(variance) || !is.function(linkinv))
        stop("'family' argument seems not to be a valid family object")
    valideta <- family$valideta
    if (is.null(valideta))
        valideta <- function(eta) TRUE
    validmu <- family$validmu
    if (is.null(validmu))
        validmu <- function(mu) TRUE
    if (is.null(mustart)) {
        eval(family$initialize)
    }
    else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
    }
 
    coefold <- NULL
    eta <- if (!is.null(etastart))
         etastart
    else family$linkfun(mustart)
    
    mu <- linkinv(eta)
    if (!(validmu(mu) && valideta(eta)))
       stop("cannot find valid starting values: please specify some")
    dev <- sum(dev.resids(y, mu, weights))*2 ## just to avoid converging at iter 1
    boundary <- conv <- FALSE
    ## construct indices for splitting up model matrix construction... 
    n.block <- nobs%/%chunk.size ## number of full sized blocks
    stub <- nobs%%chunk.size ## size of end block
    if (n.block>0) {
      start <- (0:(n.block-1))*chunk.size+1
      stop <- (1:n.block)*chunk.size
      if (stub>0) {
        start[n.block+1] <- stop[n.block]+1
        stop[n.block+1] <- nobs
        n.block <- n.block+1
      } 
    } else {
      n.block <- 1
      start <- 1
      stop <- nobs
    }
 
    G$coefficients <- rep(0,ncol(G$X))
    class(G) <- "gam"
    
    conv <- FALSE
    for (iter in 1L:control$maxit) { ## main fitting loop
       ## accumulate the QR decomposition of the weighted model matrix
       wt <- rep(0,0) 
       devold <- dev
       dev <- 0     
       for (b in 1:n.block) {
        
         ind <- start[b]:stop[b]
         G$model <- mf[ind,]
         X <- predict(G,type="lpmatrix")
         if (iter>1) eta1 <- drop(X%*%coef) + offset[ind] else eta1 <- eta[ind]
         mu <- linkinv(eta1) 
         y <- G$model[[gp$response]] ## - G$offset[ind]
         weights <- G$w[ind]
         mu.eta.val <- mu.eta(eta1)
         good <- (weights > 0) & (mu.eta.val != 0)
         z <- (eta1 - offset[ind])[good] + (y - mu)[good]/mu.eta.val[good]
         w <- (weights[good] * mu.eta.val[good]^2)/variance(mu)[good]
         dev <- dev + sum(dev.resids(y,mu,weights))
         wt <- c(wt,w)
         w <- sqrt(w)
         if (b == 1) qrx <- qr.update(w*X[good,],w*z) 
         else qrx <- qr.update(w*X[good,],w*z,qrx$R,qrx$f,qrx$y.norm2)
         rm(X);gc() ## X can be large: remove and reclaim
      }
   
      G$n <- nobs
      G$y <- mf[[gp$response]]
   
      rss.extra <- qrx$y.norm2 - sum(qrx$f^2)
      
      if (control$trace)
         cat("Deviance =", dev, "Iterations -", iter,"\n")

      if (!is.finite(dev)) stop("Non-finite deviance")

      ## preparation for working model fit is ready, but need to test for convergence first
      if (iter>2 && abs(dev - devold)/(0.1 + abs(dev)) < control$epsilon) {
          conv <- TRUE
          coef <- start
          break
      }

      if (method=="GCV.Cp") {
         fit <- magic(qrx$f,qrx$R,G$sp,G$S,G$off,L=G$L,lsp0=G$lsp0,rank=G$rank,
                      H=G$H,C=G$C,gamma=gamma,scale=scale,gcv=(scale<=0),
                      extra.rss=rss.extra,n.score=G$n)
 
         post <- magic.post.proc(qrx$R,fit,qrx$f*0+1) 
      } else { ## method is "REML" or "ML"
        y <- G$y; w <- G$w; n <- G$n;offset <- G$offset
        G$y <- qrx$f
        G$w <- G$y*0+1
        G$X <- qrx$R
        G$n <- length(G$y)
        G$offset <- G$y*0
        G$dev.extra <- rss.extra
        G$pearson.extra <- rss.extra
        G$n.true <- n
        object <- gam(G=G,method=method,gamma=gamma,scale=scale)
        y -> G$y; w -> G$w; n -> G$n;offset -> G$offset
      }
      gc()

      if (method=="GCV.Cp") { 
        object <- list()
        object$coefficients <- fit$b
        object$edf <- post$edf
        object$full.sp <- fit$sp.full
        object$gcv.ubre <- fit$score
        object$hat <- post$hat
        object$mgcv.conv <- fit$gcv.info 
        object$optimizer="magic"
        object$rank <- fit$gcv.info$rank
        object$Ve <- post$Ve
        object$Vp <- post$Vb
        object$sig2 <- object$scale <- fit$scale
        object$sp <- fit$sp
        names(object$sp) <- names(G$sp)
        class(object)<-c("gam")
      }

      coef <- object$coefficients
        
      if (any(!is.finite(coef))) {
          conv <- FALSE
          warning("non-finite coefficients at iteration ",
                  iter)
          break
      }
    } ## fitting iteration

    if (!conv)
       warning("algorithm did not converge")
   
    eps <- 10 * .Machine$double.eps
    if (family$family == "binomial") {
         if (any(mu > 1 - eps) || any(mu < eps))
                warning("fitted probabilities numerically 0 or 1 occurred")
    }
    if (family$family == "poisson") {
            if (any(mu < eps))
                warning("fitted rates numerically 0 occurred")
    }
      
  
   
  #  wtdmu <- if (intercept)
  #      sum(weights * y)/sum(weights)
  #  else linkinv(offset)
  #  nulldev <- sum(dev.resids(y, wtdmu, weights))
  object$wt <- wt
  rm(G);gc()
  object
} ## end bgam.fit



bam.fit <- function(G,mf,chunk.size,gp,scale,gamma,method,rho=0) 
## function that does big additive model fit in strictly additive case
{  ## first perform the QR decomposition, blockwise....
   n <- nrow(mf)
   if (rho!=0) { ## AR1 error model
     ld <- 1/sqrt(1-rho^2) ## leading diagonal of root inverse correlation
     sd <- -rho*ld         ## sub diagonal
   }
   if (n>chunk.size) { 
    G$coefficients <- rep(0,ncol(G$X))
    class(G) <- "gam"
    n.block <- n%/%chunk.size ## number of full sized blocks
    stub <- n%%chunk.size ## size of end block
    if (stub>0) n.block <- n.block + 1
#    start <- 0:(n.block-1)*chunk.size + 1 ## block starts
#    end <- start + chunk.size;           ## block ends
#    if (rho==0) end <- end - 1  ## otherwise most blocks go to 1 beyond block end
#    end[n.block] <- n           ## block ends

    start <- 0:(n.block-1)*chunk.size    ## block starts
    end <- start + chunk.size;           ## block ends
    end[n.block] <- n
    if (rho==0) start <- start + 1  ## otherwise most blocks go to 1 before block start
    start[1] <- 1  

    qrx <- list(R=matrix(0,0,ncol(G$X)),f=array(0,0),y.norm2=0) ## initial empty qr object
    for (i in 1:n.block) {
      ind <- start[i]:end[i] 
      if (rho!=0) {
        N <- end[i]-start[i]+1
        ## following are alternative form (harder to update)...       
        #row <- rep(1:N,rep(2,N))[-1]
        #weight <- c(rep(c(ld,sd),N-1),ld)
        #if (end[i]==n) weight[2*N-1] <- 1
        #stop <- c(1:(N-1)*2,2*N-1)
        row <- c(1,rep(1:N,rep(2,N))[-c(1,2*N)])
        weight <- c(1,rep(c(sd,ld),N-1))
        stop <- c(1,1:(N-1)*2+1)
        
      }
      G$model <- mf[ind,]
      w <- sqrt(G$w[ind])
      X <- w*predict(G,type="lpmatrix")
      y <- w*(G$model[[gp$response]] - G$offset[ind])
      if (rho!=0) {
        ## Apply transform...
     #   if (end[i]==n) {
     #     X <- rwMatrix(stop,row,weight,X)
     #     y <- rwMatrix(stop,row,weight,y)
     #   } else {
     #     X <- rwMatrix(stop,row,weight,X)[-N,]
     #     y <- rwMatrix(stop,row,weight,y)[-N]
     #   }
        if (end[i]==n) yX.last <- c(y[nrow(X)],X[nrow(X),]) ## store final row, in case of update
        if (i==1) {
           X <- rwMatrix(stop,row,weight,X)
           y <- rwMatrix(stop,row,weight,y)
        } else {
           X <- rwMatrix(stop,row,weight,X)[-1,]
           y <- rwMatrix(stop,row,weight,y)[-1]
        } 
      }      

      qrx <- qr.update(X,y,qrx$R,qrx$f,qrx$y.norm2)
      rm(X);gc() ## X can be large: remove and reclaim
    }
    G$n <- n
    G$y <- mf[[gp$response]]
   
  } else { ## n <= chunk.size
    if (rho==0) qrx <- qr.update(sqrt(G$w)*G$X,sqrt(G$w)*G$y) else {
       #row <- rep(1:n,rep(2,n))[-1]
       #weight <- c(rep(c(ld,sd),n-1),1)
       #stop <- c(1:(n-1)*2,2*n-1)
       row <- c(1,rep(1:n,rep(2,n))[-c(1,2*n)])
       weight <- c(1,rep(c(sd,ld),n-1))
       stop <- c(1,1:(n-1)*2+1)
       yX.last <- c(G$y[n],G$X[n,])  ## store final row, in case of update
       X <- rwMatrix(stop,row,weight,sqrt(G$w)*G$X)
       y <- rwMatrix(stop,row,weight,sqrt(G$w)*G$y)
       qrx <- qr.update(X,y)
   
       rm(X);gc() ## X can be large: remove and reclaim
    }
  }

  rss.extra <- qrx$y.norm2 - sum(qrx$f^2)
 
  if (method=="GCV.Cp") {
    fit <- magic(qrx$f,qrx$R,G$sp,G$S,G$off,L=G$L,lsp0=G$lsp0,rank=G$rank,
               H=G$H,C=G$C,gamma=gamma,scale=scale,gcv=(scale<=0),
               extra.rss=rss.extra,n.score=G$n)
 
    post <- magic.post.proc(qrx$R,fit,qrx$f*0+1) 
  } else { ## method is "REML" or "ML"
    y <- G$y; w <- G$w; n <- G$n;offset <- G$offset
    G$y <- qrx$f
    G$w <- G$y*0+1
    G$X <- qrx$R
    G$n <- length(G$y)
    G$offset <- G$y*0
    G$dev.extra <- rss.extra
    G$pearson.extra <- rss.extra
    G$n.true <- n
    object <- gam(G=G,method=method,gamma=gamma,scale=scale)
    y -> G$y; w -> G$w; n -> G$n;offset -> G$offset
    if (rho!=0) { ## correct RE/ML score for AR1 transform
      object$gcv.ubre <- object$gcv.ubre - (n-1)*log(ld)
    }
  }
  gc()
  if (method=="GCV.Cp") { 
    object <- list()
    object$coefficients <- fit$b
    object$edf <- post$edf
    object$full.sp <- fit$sp.full
    object$gcv.ubre <- fit$score
    object$hat <- post$hat
    object$mgcv.conv <- fit$gcv.info 
    object$optimizer="magic"
    object$rank <- fit$gcv.info$rank
    object$Ve <- post$Ve
    object$Vp <- post$Vb
    object$sig2 <- object$scale <- fit$scale
    object$sp <- fit$sp
    class(object)<-c("gam")
  } else {
    
  }
  G$smooth <- G$X <- NULL

  object$AR1.rho <- rho
  if (rho!=0) { ## need to store last model matrix row, to allow update
    object$yX.last <- yX.last
  }

  object$gamma <- gamma;object$G <- G;object$qrx <- qrx ## to allow updating of the model
  object
}




bam <- function(formula,family=gaussian(),data=list(),weights=NULL,subset=NULL,na.action=na.omit,
                offset=NULL,method="REML",control=list(),scale=0,gamma=1,knots=NULL,
                sp=NULL,min.sp=NULL,paraPen=NULL,chunk.size=10000,rho=0,...)

## Routine to fit an additive model to a large dataset. The model is stated in the formula, 
## which is then interpreted to figure out which bits relate to smooth terms and which to 
## parametric terms.
## This is a modification of `gam' designed to build the QR decompostion of the model matrix 
## up in chunks, to keep memory costs down.
{ control <- do.call("gam.control",control)
  if (is.character(family))
            family <- eval(parse(text = family))
  if (is.function(family))
            family <- family()
  if (is.null(family$family))
            stop("family not recognized")
  ##family = gaussian() ## no choise here
  if (family$family=="gaussian"&&family$link=="identity") am <- TRUE else am <- FALSE
  if (scale==0) { if (family$family%in%c("poisson","binomial")) scale <- 1 else scale <- -1} 
  if (!method%in%c("GCV.Cp","REML","ML","P-REML","P-ML")) stop("un-supported smoothness selection method")
  gp<-interpret.gam(formula) # interpret the formula 
  cl<-match.call() # call needed in gam object for update to work
  mf<-match.call(expand.dots=FALSE)
  mf$formula<-gp$fake.formula 
  mf$method <-  mf$family<-mf$control<-mf$scale<-mf$knots<-mf$sp<-mf$min.sp <-
  mf$gamma <- mf$paraPen<- mf$chunk.size <- mf$rho <- mf$...<-NULL
  mf$drop.unused.levels<-TRUE
  mf[[1]]<-as.name("model.frame")
  pmf <- mf
 
  pmf$formula <- gp$pf
  pmf <- eval(pmf, parent.frame()) # pmf contains all data for parametric part
  pterms <- attr(pmf,"terms") ## pmf only used for this
  rm(pmf);gc()

  mf <- eval(mf, parent.frame()) # the model frame now contains all the data 
  if (nrow(mf)<2) stop("Not enough (non-NA) data to do anything meaningful")
  terms <- attr(mf,"terms")
  gc()  

  ## summarize the *raw* input variables
  ## note can't use get_all_vars here -- buggy with matrices
  vars <- all.vars(gp$fake.formula[-2]) ## drop response here
  inp <- parse(text = paste("list(", paste(vars, collapse = ","),")"))

  ## allow a bit of extra flexibility in what `data' is allowed to be (as model.frame actually does)
  if (!is.list(data)&&!is.data.frame(data)) data <- as.data.frame(data) 

  dl <- eval(inp, data, parent.frame())
  if (!control$keepData) { rm(data);gc()} ## save space
  names(dl) <- vars ## list of all variables needed
  var.summary <- mgcv:::variable.summary(gp$pf,dl,nrow(mf)) ## summarize the input data
  rm(dl);gc() ## save space    

  ## need mini.mf for basis setup, then accumulate full X, y, w and offset
  mf0 <- mini.mf(mf,chunk.size)
    
  G<-mgcv:::gam.setup(gp,pterms=pterms,data=mf0,knots=knots,sp=sp,min.sp=min.sp,
                 H=NULL,absorb.cons=TRUE,sparse.cons=0,select=FALSE,
                 idLinksBases=TRUE,scale.penalty=control$scalePenalty,
                 paraPen=paraPen)

  G$var.summary <- var.summary
  G$family <- family
  G$terms<-terms;G$pterms<-pterms
  
  n <- nrow(mf)
  
  if (is.null(mf$"(weights)")) G$w<-rep(1,n)
  else G$w<-mf$"(weights)"    
  
  G$offset <- model.offset(mf)  
  if (is.null(G$offset)) G$offset <- rep(0,n)

  if (ncol(G$X)>nrow(mf)+nrow(G$C)) stop("Model has more coefficients than data")
 
  G$cl<-cl;
  G$am <- am
     
  G$min.edf<-G$nsdf-dim(G$C)[1]
  if (G$m) for (i in 1:G$m) G$min.edf<-G$min.edf+G$smooth[[i]]$null.space.dim

  G$formula<-formula
  environment(G$formula)<-environment(formula)
  
  G$conv.tol<-control$mgcv.tol      # tolerence for mgcv
  G$max.half<-control$mgcv.half     # max step halving in bfgs optimization


  ## now build up proper model matrix, and deal with y, w, and offset...

  if (control$trace) cat("Setup complete. Calling fit\n")
  
  colnamesX <- colnames(G$X)  

  if (am) {
    if (nrow(mf)>chunk.size) G$X <- matrix(0,0,ncol(G$X)); gc() 
    object <- bam.fit(G,mf,chunk.size,gp,scale,gamma,method,rho=rho)
  } else {
    G$X  <- matrix(0,0,ncol(G$X)); gc()
    if (rho!=0) warning("AR1 parameter rho unused with generalized model")
    object <- bgam.fit(G, mf, chunk.size, gp ,scale ,gamma,method=method,
                       control = control,...)
  }

  gc()

  if (control$trace) cat("Fit complete. Finishing gam object.\n")

  if (scale < 0) { object$scale.estimated <- TRUE;object$scale <- object$scale.est} else {
    object$scale.estimated <- FALSE; object$scale <- scale
  }

  object$assign <- G$assign # applies only to pterms  
  object$boundary <- FALSE  # always FALSE for this case
  object$call<-G$cl # needed for update() to work 
  object$cmX <- G$cmX ## column means of model matrix --- useful for CIs
 
  object$contrasts <- G$contrasts
  object$control <- control
  object$converged <- TRUE ## no iteration
  object$data <- NA ## not saving it in this case
  object$df.null <- nrow(mf)
  object$df.residual <- object$df.null - sum(object$edf) 
 
  object$family <- family
  object$formula<-G$formula 
 
  object$iter <- 1
  #object$linear.predictors <- NA
  if (method=="GCV.Cp") {
    if (scale<=0) object$method <- "GCV" else object$method <- "UBRE"
  } else {
    object$method <- method
  }
  object$min.edf<-G$min.edf
  object$model <- mf;rm(mf);gc()
  object$na.action <- attr(object$model,"na.action") # how to deal with NA's
  object$nsdf <- G$nsdf
  names(object$coefficients)[1:G$nsdf] <- colnamesX[1:G$nsdf]
  object$offset <- G$offset
  object$prior.weights <- G$w
  object$pterms <- G$pterms
 
  object$smooth <- G$smooth

  object$terms <- G$terms
  object$var.summary <- G$var.summary 
 
  object$weights <- object$prior.weights
  object$xlevels <- G$xlevels
  object$y <- object$model[[gp$response]]
  object$NA.action <- na.action ## version to use in bam.update

  rm(G);gc()

  ## note that predict.gam assumes that it must be ok not to split the 
  ## model frame, if no new data supplied, so need to supply explicitly
  object$linear.predictors <- as.numeric(predict.gam(object,newdata=object$model,block.size=chunk.size))
  object$fitted.values <- family$linkinv(object$linear.predictors)
  
  object$residuals <- sqrt(family$dev.resids(object$y,object$fitted.values,object$weights)) * 
                      sign(object$y-object$fitted.values)
  object$deviance <- sum(object$residuals^2)
  object$aic <- family$aic(object$y,1,object$fitted.values,object$weights,object$deviance) +
                2*sum(object$edf)
  object$null.deviance <- sum(family$dev.resids(object$y,mean(object$y),object$weights))
  class(object) <- c("gam","glm","lm")
  object
}


bam.update <- function(b,data,chunk.size=10000) {
## update the strictly additive model `b' in the light of new data in `data'
## Need to update modelframe (b$model) 
  if (is.null(b$qrx)) { 
    stop("Model can not be updated")
  }
  gp<-interpret.gam(b$formula) # interpret the formula 
  
  X <- predict(b,newdata=data,type="lpmatrix",na.action=b$NA.action) ## extra part of model matrix
  
  cnames <- names(b$coefficients)

  ## now get the new data in model frame form...

  if ("(weights)"%in%names(b$model)) { 
    mf <- model.frame(gp$fake.formula,data,weights=weights,xlev=b$xlev,na.action=b$NA.action)
    w <- mf[["(weights)"]]
  } else {
    mf <- model.frame(gp$fake.formula,data,xlev=b$xlev,na.action=b$NA.action)
    w <- rep(1,nrow(mf))
  }


  b$model <- rbind(b$model,mf) ## complete model frame --- old + new

  ## get response and offset...

  off.col <- attr(attr(b$model,"terms"),"offset")
  if (is.null(off.col)) offset <- rep(0,nrow(mf)) else offset <-  mf[,off.col]
  y <-  mf[,attr(attr(b$model,"terms"),"response")] - offset
  

  ## update G
  b$G$y <- c(b$G$y,y)
  b$G$offset <- c(b$G$offset,offset)
  b$G$w <- c(b$G$w,w)
  b$G$n <- nrow(b$model)
  n <- b$G$n;
  ## update the qr decomposition...

  w <- sqrt(w)

  if (b$AR1.rho!=0) { ## original model had AR1 error structure...
    rho <- b$AR1.rho
    ld <- 1/sqrt(1-rho^2) ## leading diagonal of root inverse correlation
    sd <- -rho*ld         ## sub diagonal
    ## append the final row of weighted X and y from original fit, first
    wy <- c(b$yX.last[1],w*y)
    wX <- rbind(b$yX.last[-1],w*X)
    m <- nrow(wX)
    b$yX.last <- c(wy[m],wX[m,])

    row <- c(1,rep(1:m,rep(2,m))[-c(1,2*m)])
    weight <- c(1,rep(c(sd,ld),m-1))
    stop <- c(1,1:(m-1)*2+1)
   
    ## re-weight to independence....
    wX <- rwMatrix(stop,row,weight,wX)[-1,]
    wy <- rwMatrix(stop,row,weight,wy)[-1]    

    ## update
    b$qrx <- mgcv:::qr.update(wX,wy,b$qrx$R,b$qrx$f,b$qrx$y.norm2)
  } else {
    b$qrx <- mgcv:::qr.update(w*X,w*y,b$qrx$R,b$qrx$f,b$qrx$y.norm2)
  }

  ## now do the refit...
  rss.extra <- b$qrx$y.norm2 - sum(b$qrx$f^2)

  if (b$method=="GCV"||b$method=="UBRE") method <- "GCV.Cp" else method <- b$method

 
  if (method=="GCV.Cp") {
    if (b$method=="GCV") scale <- -1 else scale = b$sig2
   
    fit <- magic(b$qrx$f,b$qrx$R,b$sp,b$G$S,b$G$off,L=b$G$L,lsp0=b$G$lsp0,rank=b$G$rank,
               H=b$G$H,C=b$G$C,gamma=b$gamma,scale=scale,gcv=(scale<=0),
               extra.rss=rss.extra,n.score=n)
 
    post <- magic.post.proc(b$qrx$R,fit,b$qrx$f*0+1) 
    b$y <- b$G$y;b$offset <- b$G$offset; b$G$w -> b$weights -> b$prior.weights;
    
  } else { ## method is "REML" or "ML"
    y <- b$G$y; w <- b$G$w;offset <- b$G$offset
    b$G$y <- b$qrx$f
    b$G$w <- b$G$y*0+1
    b$G$X <- b$qrx$R
    b$G$n <- length(b$G$y)
    b$G$offset <- b$G$y*0
    b$G$dev.extra <- rss.extra
    b$G$pearson.extra <- rss.extra
    b$G$n.true <- n
    if (b$scale.estimated) scale <- -1 else scale = b$sig2
    in.out <- list(sp=b$sp,scale=b$reml.scale)
    object <- gam(G=b$G,method=method,gamma=b$gamma,scale=scale,in.out=in.out)
    offset -> b$G$offset -> b$offset
    w -> b$G$w -> b$weights -> b$prior.weights; n -> b$G$n
    y -> b$G$y -> b$y;
  }
 
  if (method=="GCV.Cp") { 

    b$coefficients <- fit$b
    b$edf <- post$edf
    b$full.sp <- fit$sp.full
    b$gcv.ubre <- fit$score
    b$hat <- post$hat
    b$mgcv.conv <- fit$gcv.info 
    b$optimizer="magic"
    b$rank <- fit$gcv.info$rank
    b$Ve <- post$Ve
    b$Vp <- post$Vb
    b$sig2 <- b$scale <- fit$scale
    b$sp <- fit$sp

  } else { ## REML or ML
    b$coefficients <- object$coefficients
    b$edf <- object$edf
    b$full.sp <- object$sp.full
    b$gcv.ubre <- object$gcv.ubre
    b$hat <- object$hat
    b$outer.info <- object$outer.info 
    b$rank <- object$rank
    b$Ve <- object$Ve
    b$Vp <- object$Vp
    b$sig2 <- b$scale <- object$sig2
    b$sp <- object$sp
    if (b$AR1.rho!=0) { ## correct RE/ML score for AR1 transform
      b$gcv.ubre <- b$gcv.ubre - (n-1)*log(ld)
    }
  }
  b$G$X <- NULL
  b$linear.predictors <- as.numeric(predict.gam(b,newdata=b$model,block.size=chunk.size))
  b$fitted.values <- b$linear.predictor ## strictly additive only!
  
  b$residuals <- sqrt(b$family$dev.resids(b$y,b$fitted.values,b$weights)) * 
                      sign(b$y-b$fitted.values)
  b$deviance <- sum(b$residuals^2)
  b$aic <- b$family$aic(b$y,1,b$fitted.values,b$weights,b$deviance) + 2 * sum(b$edf)
  b$null.deviance <- sum(b$family$dev.resids(b$y,mean(b$y),b$weights))
  names(b$coefficients) <- names(b$edf) <- cnames
  b
} ## end of bam.update


#### ISSUES:   
## ? negative binomial support --- docs say it's there...
## need to add `bam' examples to the test suite, and possibly update the test suite
## to work through failures, logging them. 