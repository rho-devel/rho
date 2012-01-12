
### the following two functions are for use in place of log and exp
### in positivity ensuring re-parameterization.... they have `better' 
### over/underflow characteristics, but are still continuous to second
### derivative. 

notExp <- function(x)
# overflow avoiding C2 function for ensuring positivity
{ f <- x
  ind <- x > 1
  f[ind] <- exp(1)*(x[ind]^2+1)/2
  ind <- (x <= 1)&(x > -1)
  f[ind] <- exp(x[ind])
  ind <- (x <= -1)
  x[ind] <- -x[ind] ;f[ind] <-  exp(1)*(x[ind]^2+1)/2; f[ind]<-1/f[ind]
  f
}


notLog <- function(x)
# inverse function of notExp
{ f <- x
  ind <- x> exp(1)
  f[ind] <- sqrt(2*x[ind]/exp(1)-1)
  ind <- !ind & x > exp(-1)
  f[ind] <- log(x[ind])
  ind <- x <= exp(-1)
  x[ind]<- 1/x[ind]; f[ind] <- sqrt(2*x[ind]/exp(1)-1);f[ind] <- -f[ind]
 f
}

## notLog/notExp replacements. 
## around 27/7/05 nlme was modified to use a new optimizer, which fails with 
## indefinite Hessians. This is a problem if smoothing parameters are zero 
## or infinite. The following attempts to make the notLog parameterization 
## non-monotonic, to artificially reduce the likelihood at very large and very
## small parameter values.

## note gamm, pdTens, pdIdnot, notExp and notExp2 .Rd files all modified by
## this change.


notExp2 <- function (x,d=.Options$mgcv.vc.logrange,b=1/d)
## to avoid needing to modify solve.pdIdnot, this transformation must
## maintain the property that 1/notExp2(x) = notExp2(-x)
{ exp(d*sin(x*b))
}

notLog2 <- function(x,d=.Options$mgcv.vc.logrange,b=1/d)
{ x <- log(x)/d
  x <- pmin(1,x)
  x <- pmax(-1,x)
  asin(x)/b
}


#### pdMat class definitions, to enable tensor product smooths to be employed with gamm()
#### Based on various Pinheiro and Bates pdMat classes.

pdTens <- function(value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()))
## Constructor for the pdTens pdMat class: 
# the inverse of the scaled random effects covariance matrix for this class
# is given by a weighted sum of the matrices in the list that is the "S" attribute of 
# a pdTens formula. The weights are the exponentials of the class parameters.
# i.e. the inverse of the r.e. covariance matrix is 
#   \sum_i \exp(\theta_i) S_i / \sigma^2 
# The class name relates to the fact that these objects are used with tensor product smooths.  
{
  object <- numeric(0)
  class(object) <- c("pdTens", "pdMat")
  nlme::pdConstruct(object, value, form, nam, data)
}

## Methods for local generics


pdConstruct.pdTens <-
  function(object, value = numeric(0), form = formula(object),
	   nam = nlme::Names(object), data = sys.frame(sys.parent()), ...)
## used to initialize pdTens objects. Note that the initialization matrices supplied
## are (factors of) trial random effects covariance matrices or their inverses.
## Which one is being passed seems to have to be derived from looking at its
##  structure.
## Class tested rather thoroughly with nlme 3.1-52 on R 2.0.0
{
  val <- NextMethod()
  if (length(val) == 0) {               # uninitiliazed object
    class(val) <- c("pdTens","pdMat")
    return(val)
  }
  if (is.matrix(val)) {			# initialize from a positive definite
    S <- attr(form,"S")
    m <- length(S)
    ## codetools gets it wrong about `y'
    y <- as.numeric((crossprod(val)))   # it's a factor that gets returned in val
    lform <- "y ~ as.numeric(S[[1]])"
    if (m>1) for (i in 2:m) lform <- paste(lform," + as.numeric(S[[",i,"]])",sep="")
    lform <- formula(paste(lform,"-1"))
    mod1<-lm(lform)
    y <- as.numeric(solve(crossprod(val))) ## ignore codetools complaint about this
    mod2<-lm(lform)
    ## `value' and `val' can relate to the cov matrix or its inverse:
    ## the following seems to be only way to tell which.
    if (summary(mod2)$r.sq>summary(mod1)$r.sq) mod1<-mod2
    value <- coef(mod1)  
    value[value <=0] <- .Machine$double.eps * mean(as.numeric(lapply(S,function(x) max(abs(x)))))
    value <- notLog2(value)
    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
    class(value) <- c("pdTens", "pdMat")
    return(value)
  }
  m <- length(attr(form,"S"))
  if ((aux <- length(val)) > 0) {
    if (aux && (aux != m)) {
      stop(paste("An object of length", aux,
		 "does not match the required parameter size"))
    }
  }
  class(val) <- c("pdTens","pdMat")
  val
}


pdFactor.pdTens <- function(object)
## The factor of the inverse of the scaled r.e. covariance matrix is returned here
## it should be returned as a vector. 
{ sp <- as.vector(object)
  m <- length(sp)
  S <- attr(formula(object),"S")
  value <- S[[1]]*notExp2(sp[1])
  if (m>1) for (i in 2:m) value <- value + notExp2(sp[i])*S[[i]] 
  if (sum(is.na(value))>0) warning("NA's in pdTens factor")
  value <- (value+t(value))/2
  c(t(mroot(value,rank=nrow(value))))
}


pdMatrix.pdTens <-
  function(object, factor = FALSE) 
# the inverse of the scaled random effect covariance matrix is returned here, or
# its factor if factor==TRUE. If A is the matrix being factored and B its
# factor, it is required that A=B'B (not the mroot() default!)

{
  if (!nlme::isInitialized(object)) {
    stop("Cannot extract the matrix from an uninitialized object")
  }
  sp <- as.vector(object)
  m <- length(sp)
  S <- attr(formula(object),"S")
  value <- S[[1]]*notExp2(sp[1])   
  if (m>1) for (i in 2:m) value <- value + notExp2(sp[i])*S[[i]]  
 
  value <- (value + t(value))/2 # ensure symmetry
  if (sum(is.na(value))>0) warning("NA's in pdTens matrix")
  if (factor) {
    value <- t(mroot(value,rank=nrow(value)))
  } 
  dimnames(value) <- attr(object, "Dimnames")
  value
}

#### Methods for standard generics

coef.pdTens <-
  function(object, unconstrained = TRUE, ...)
{
  if (unconstrained) NextMethod()
  else {
    val <- notExp2(as.vector(object))
    names(val) <- paste("sp.",1:length(val), sep ="")
    val
  }
}

summary.pdTens <-
  function(object, structName = "Tensor product smooth term", ...)
{
  NextMethod(object, structName, noCorrelation=TRUE)
}


# .... end of pdMat definitions for tensor product smooths


### pdIdnot: multiple of the identity matrix - the parameter is
### the notLog2 of the multiple. This is directly modified form 
### Pinheiro and Bates pdIdent class.

####* Constructor

pdIdnot <-
  ## Constructor for the pdIdnot class
  function(value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()))
{ #cat(" pdIdnot  ")
  object <- numeric(0)
  class(object) <- c("pdIdnot", "pdMat")
  nlme::pdConstruct(object, value, form, nam, data)
}

####* Methods for local generics

corMatrix.pdIdnot <-
  function(object, ...)
{
  if (!nlme::isInitialized(object)) {
    stop("Cannot extract the matrix from an uninitialized pdMat object")
  }
  if (is.null(Ncol <- attr(object, "ncol"))) {
    stop(paste("Cannot extract the matrix with uninitialized dimensions"))
  }
  val <- diag(Ncol)
## REMOVE sqrt() to revert ...
  attr(val, "stdDev") <- rep(sqrt(notExp2(as.vector(object))), Ncol)
  if (length(nm <- nlme::Names(object)) == 0) {
    len <- length(as.vector(object)) 
    nm <- paste("V", 1:len, sep = "")
    dimnames(val) <- list(nm, nm)
  }
  names(attr(val, "stdDev")) <- nm
  val
}

pdConstruct.pdIdnot <-
  function(object, value = numeric(0), form = formula(object),
	   nam = nlme::Names(object), data = sys.frame(sys.parent()), ...)
{ #cat(" pdConstruct.pdIdnot  ")
  val <- NextMethod()
  if (length(val) == 0) {			# uninitialized object
    if ((ncol <- length(nlme::Names(val))) > 0) {
      attr(val, "ncol") <- ncol
    }
    return(val)
  }
  if (is.matrix(val)) {
#    value <- notLog2(sqrt(mean(diag(crossprod(val)))))
    value <- notLog2(mean(diag(crossprod(val)))) ## REPLACE by above to revert
    attributes(value) <- attributes(val)[names(attributes(val)) != "dim"]
    attr(value, "ncol") <- dim(val)[2]
    class(value) <- c("pdIdnot", "pdMat")
    return(value)
  }
  if (length(val) > 1) {
    stop(paste("An object of length", length(val),
	       "does not match the required parameter size"))
  }
  if (((aux <- length(nlme::Names(val))) == 0) && is.null(formula(val))) {
    stop(paste("Must give names when initializing pdIdnot from parameter.",
	       "without a formula"))
  } else {
    attr(val, "ncol") <- aux
  }
  val
}

pdFactor.pdIdnot <-
  function(object)
{ ## UNCOMMENT first line, comment 2nd to revert
  # notExp2(as.vector(object)) * diag(attr(object, "ncol"))
  #cat(" pdFactor.pdIdnot  ")
  sqrt(notExp2(as.vector(object))) * diag(attr(object, "ncol"))
}

pdMatrix.pdIdnot <-
  function(object, factor = FALSE)
{ #cat("  pdMatrix.pdIdnot  ")
  if (!nlme::isInitialized(object)) {
    stop("Cannot extract the matrix from an uninitialized pdMat object")
  }
  if (is.null(Ncol <- attr(object, "ncol"))) {
    stop(paste("Cannot extract the matrix with uninitialized dimensions"))
  }
  value <- diag(Ncol)
    
  ## REPLACE by #1,#2,#3 to revert
  if (factor) {
   #1  value <- notExp2(as.vector(object)) * value
   #2  attr(value, "logDet") <- Ncol * log(notExp2(as.vector(object)))
   value <- sqrt(notExp2(as.vector(object))) * value
   attr(value, "logDet") <- Ncol * log(notExp2(as.vector(object)))/2
  } else {
   #3 value <- notExp2(as.vector(object))^2 * value
    value <- notExp2(as.vector(object)) * value
  }
  dimnames(value) <- attr(object, "Dimnames")
  value
}

####* Methods for standard generics

coef.pdIdnot <-
  function(object, unconstrained = TRUE, ...)
{ #cat(" coef.pdIdnot    ")
  if (unconstrained) NextMethod()
  else structure(notExp2(as.vector(object)),
           names = c(paste("sd(", deparse(formula(object)[[2]],backtick=TRUE),")",sep = "")))
}

Dim.pdIdnot <-
  function(object, ...)
{
  if (!is.null(val <- attr(object, "ncol"))) {
    c(val, val)
  } else {
    stop("Cannot extract the dimensions")
  }
}

logDet.pdIdnot <-
  function(object, ...)
{ ## REMOVE /2 to revert ....
  attr(object, "ncol") * log(notExp2(as.vector(object)))/2
  
}

solve.pdIdnot <-
  function(a, b, ...)
{
  if (!nlme::isInitialized(a)) {
    stop("Cannot extract the inverse from an uninitialized object")
  }
  atr <- attributes(a)
  a <- -coef(a, TRUE)
  attributes(a) <- atr
  a
}

summary.pdIdnot <-
  function(object, structName = "Multiple of an Identity", ...)
{ #cat("  summary.pdIdnot  ")
  # summary.pdMat(object, structName, noCorrelation = TRUE)

  ## ... summary.pdMat is not exported in the nlme NAMESPACE file, so....

  NextMethod(object, structName, noCorrelation=TRUE)
}

### end of pdIdnot class


gamm.setup<-function(formula,pterms,data=stop("No data supplied to gamm.setup"),knots=NULL,
                     parametric.only=FALSE,absorb.cons=FALSE)
# set up the model matrix, penalty matrices and auxilliary information about the smoothing bases
# needed for a gamm fit.
# NOTE: lme can't deal with offset terms.
# There is an implicit assumption that any rank deficient penalty does not penalize 
# the constant term in a basis. 
{ 
  ## first simply call `gam.setup'....

  G <- gam.setup(formula,pterms,data=data,knots=knots,sp=NULL,
                    min.sp=NULL,H=NULL,absorb.cons=TRUE,sparse.cons=0)
 
  if (!is.null(G$L)) stop("gamm can not handle linked smoothing parameters (probably from use of `id' or adaptive smooths)")
  # now perform re-parameterization...

  first.f.para<-G$nsdf+1
  first.r.para<-1
 
  G$Xf <- G$X # full GAM model matrix, treating smooths as fixed effects
  random<-list()
  random.i<-0
  
  if (G$nsdf>0) ind <- 1:G$nsdf else ind <- rep(0,0)  

  X <- G$X[,ind,drop=FALSE] # accumulate fixed effects into here

  xlab <- rep("",0)
  
  ## code to deal with t2 smooths, by splitting up into single terms
  if (G$m) {
    sme <- expand.t2.smooths(G$smooth)
    if (is.null(sme)) G$original.smooth <- NULL else {
      G$original.smooth <- G$smooth
      G$smooth <- sme ## G's smooth list is replaced by expanded version, until some time after model fitting is complete
      rm(sme)
    }
    ## G$m is always the length of G$smooth here...
    G$m <- length(G$smooth)
  }


  if (G$m)
  for (i in 1:G$m) 
  { sm <- G$smooth[[i]]
    sm$X <- G$X[,sm$first.para:sm$last.para,drop=FALSE]
    if (inherits(sm,"tensor.smooth"))
    { if (sum(sm$fx)==length(sm$fx)) sm$fixed <- TRUE
      else 
      { sm$fixed <- FALSE
        if (sum(sm$fx)!=0) warning("gamm can not fix only some margins of tensor product.")
      }
    } 
    if (!sm$fixed) random.i <- random.i+1
   
    ZSZ <- list()
    if (!sm$fixed) 
    for (l in 1:length(sm$S)) ZSZ[[l]]<-sm$S[[l]]
    XZ<-sm$X
    k <- ncol(sm$X);j<-0
   
    if (!sm$fixed) 
    { sm$ZSZ <- ZSZ            # store these too - for construction of Vp matrix
      if (inherits(sm,"tensor.smooth")) { 
        # tensor product term - need to find null space from sum of penalties
        sum.ZSZ <- ZSZ[[1]]/mean(abs(ZSZ[[1]]))
        ## null.rank <- sm$margin[[1]]$bs.dim-sm$margin[[1]]$rank
        null.rank <- ncol(sm$margin[[1]]$X)-sm$margin[[1]]$rank
        bs.dim <- sm$margin[[1]]$bs.dim
        if (length(ZSZ)>1) for (l in 2:length(ZSZ)) 
        { sum.ZSZ <- sum.ZSZ + ZSZ[[l]]/mean(abs(ZSZ[[l]]))
          dfl <- ncol(sm$margin[[l]]$X) ## actual df of term (`df' may not be set by constructor)
          null.rank <- ## the rank of the null space of the penalty 
                       ## null.rank * (sm$margin[[l]]$bs.dim-sm$margin[[l]]$rank)
                       null.rank * (dfl-sm$margin[[l]]$rank) 
          bs.dim <- bs.dim*dfl
        }
        null.rank <- null.rank - bs.dim + sm$df
        sum.ZSZ <- (sum.ZSZ+t(sum.ZSZ))/2 # ensure symmetry
        ev <- eigen(sum.ZSZ,symmetric=TRUE)
        mult.pen <- TRUE
      } else            # regular s() term
      { ZSZ[[1]] <- (ZSZ[[1]]+t(ZSZ[[1]]))/2
        ev<-eigen(ZSZ[[1]],symmetric=TRUE)
        null.rank <- sm$df - sm$rank
        mult.pen <- FALSE
      }
      p.rank <- ncol(XZ) - null.rank
      if (p.rank>ncol(XZ)) p.rank <- ncol(XZ)
      U<-ev$vectors
      D<-ev$values[1:p.rank]
      if (sum(D<=0)) stop(
      "Tensor product penalty rank appears to be too low: please email Simon.Wood@R-project.org with details.")
      D<-1/sqrt(D)
      XZU<-XZ%*%U
      if (p.rank<k-j) Xf<-XZU[,(p.rank+1):(k-j),drop=FALSE]
      else Xf<-matrix(0,nrow(sm$X),0) # no fixed terms left
      if (mult.pen) 
      { Xr <- XZU[,1:p.rank] # tensor product case
        for (l in 1:length(ZSZ))   # transform penalty explicitly
        { ZSZ[[l]] <- (t(U)%*%ZSZ[[l]]%*%U)[1:p.rank,1:p.rank]
          ZSZ[[l]] <- (ZSZ[[l]]+t(ZSZ[[l]]))/2
        }
      }
      else Xr<-t(t(XZU[,1:p.rank])*D)
      n.para<-k-j-p.rank # indices for fixed parameters
      sm$first.f.para<-first.f.para
      first.f.para<-first.f.para+n.para
      sm$last.f.para<-first.f.para-1
      n.para<-ncol(Xr) # indices for random parameters
      sm$first.r.para<-first.r.para
      first.r.para<-first.r.para+n.para
      sm$last.r.para<-first.r.para-1
    
      sm$D<-D;sm$U<-U # information (with qrc) for backtransforming to original space 

      term.name <- paste("Xr.",random.i,sep="")
      term.name <- new.name(term.name,names(data))
      form <- as.formula(paste("~",term.name,"-1",sep=""))
      if (mult.pen)  # tensor product case
      { attr(form,"S") <- ZSZ
        random[[random.i]] <- pdTens(form)
      } else  # single penalty smooth
      random[[random.i]] <- pdIdnot(form)
      names(random)[random.i] <- term.name
      eval(parse(text=paste("G$",term.name,"<-Xr",sep="")))
    } else # term is fixed, so model matrix appended to fixed matrix
    { Xf <- XZ # whole term goes to fixed 
      n.para <- ncol(Xf)       # now define where the parameters of this term live 
      sm$first.f.para <- first.f.para
      first.f.para <- first.f.para+n.para
      sm$last.f.para <- first.f.para-1
    }
    ## now add appropriate column names to Xf.
    ## without these, summary.lme will fail
    
    if (ncol(Xf)) {
      Xfnames<-rep("",ncol(Xf)) 
      k<-length(xlab)+1
      for (j in 1:ncol(Xf)) {
        xlab[k] <- Xfnames[j] <-
        new.name(paste(sm$label,"Fx",j,sep=""),xlab)
        k <- k + 1
      } 
      colnames(Xf) <- Xfnames
    }

    X<-cbind(X,Xf) # add fixed model matrix to overall X
  
    sm$X <- NULL
  
    G$smooth[[i]] <- sm  ## replace smooth object with transformed version 
  }
 
  G$random<-random
  G$X<-X  ## fixed effects model matrix
 
  G
}



varWeights.dfo <- function(b,data)
## get reciprocal *standard deviations* implied by the estimated variance
## structure of an lme object, b, in *original data frame order*.
{  w <- nlme::varWeights(b$modelStruct$varStruct) 
   # w is not in data.frame order - it's in inner grouping level order
   group.name <- names(b$groups) # b$groups[[i]] doesn't always retain factor ordering
   ind <- NULL
   order.txt <- paste("ind<-order(data[[\"",group.name[1],"\"]]",sep="")
   if (length(b$groups)>1) for (i in 2:length(b$groups)) 
   order.txt <- paste(order.txt,",data[[\"",group.name[i],"\"]]",sep="")
   order.txt <- paste(order.txt,")")
   eval(parse(text=order.txt))
   w[ind] <- w # into data frame order
   w
}

extract.lme.cov2<-function(b,data,start.level=1)
# function to extract the response data covariance matrix from an lme fitted
# model object b, fitted to the data in data. "inner" == "finest" grouping 
# start.level is the r.e. grouping level at which to start the construction, 
# levels outer to this will not be included in the calculation - this is useful
# for gamm calculations
# 
# This version aims to be efficient, by not forming the complete matrix if it
# is diagonal or block diagonal. To this end the matrix is returned in a form
# that relates to the data re-ordered according to the coarsest applicable 
# grouping factor. ind[i] gives the row in the original data frame
# corresponding to the ith row/column of V. 
# V is either returned as an array, if it's diagonal, a matrix if it is
# a full matrix or a list of matrices if it is block diagonal.
{ if (!inherits(b,"lme")) stop("object does not appear to be of class lme")
  grps<-nlme::getGroups(b) # labels of the innermost groupings - in data frame order
  n<-length(grps)    # number of data
  n.levels <- length(b$groups) # number of levels of grouping
  if (n.levels<start.level) ## then examine correlation groups
  { if (is.null(b$modelStruct$corStruct)) n.corlevels <- 0 else
    n.corlevels <-
    length(all.vars(nlme::getGroupsFormula(b$modelStruct$corStruct)))
  } else n.corlevels <- 0 ## used only to signal irrelevance
  ## so at this stage n.corlevels > 0 iff it determines the coarsest grouping
  ## level if > start.level. 
  if (n.levels<n.corlevels) ## then cor groups are finest
  grps <- nlme::getGroups(b$modelStruct$corStruct) # replace grps (but not n.levels)

  if (n.levels >= start.level||n.corlevels >= start.level)
  { if (n.levels >= start.level)
    Cgrps <- nlme::getGroups(b,level=start.level) # outer grouping labels (dforder) 
    else Cgrps <- nlme::getGroups(b$modelStruct$corStruct) # ditto
    Cind <- sort(as.numeric(Cgrps),index.return=TRUE)$ix
    # Cind[i] is where row i of sorted Cgrps is in original data frame order 
    rCind <- 1:n; rCind[Cind] <- 1:n
    # rCind[i] is location of ith original datum in the coarse ordering
    ## CFgrps <- grps[Cind] # fine group levels in coarse group order (unused!!)
    Clevel <- levels(Cgrps) # levels of coarse grouping factor
    n.cg <- length(Clevel)  # number of outer groups
    size.cg <- array(0,n.cg)  
    for (i in 1:n.cg) size.cg[i] <- sum(Cgrps==Clevel[i]) # size of each coarse group
    ## Cgrps[Cind] is sorted by coarsest grouping factor level
    ## so e.g. y[Cind] would be data in c.g.f. order
  } else {n.cg <- 1;Cind<-1:n}
  if (is.null(b$modelStruct$varStruct)) w<-rep(b$sigma,n) ### 
  else 
  { w<-1/nlme::varWeights(b$modelStruct$varStruct) 
    # w is not in data.frame order - it's in inner grouping level order
    group.name<-names(b$groups) # b$groups[[i]] doesn't always retain factor ordering
    order.txt <- paste("ind<-order(data[[\"",group.name[1],"\"]]",sep="")
    if (length(b$groups)>1) for (i in 2:length(b$groups)) 
    order.txt <- paste(order.txt,",data[[\"",group.name[i],"\"]]",sep="")
    order.txt <- paste(order.txt,")")
    eval(parse(text=order.txt))
    w[ind] <- w # into data frame order
    w<-w*b$sigma
  }
  w <- w[Cind] # re-order in coarse group order
  if (is.null(b$modelStruct$corStruct)) V<-array(1,n) 
  else
  { c.m<-nlme::corMatrix(b$modelStruct$corStruct) # correlation matrices for each innermost group
    if (!is.list(c.m)) { # copy and re-order into coarse group order
      V <- c.m;V[Cind,] -> V;V[,Cind] -> V 
    } else { 
      V<-list()   # V[[i]] is cor matrix for ith coarse group
      ind <- list() # ind[[i]] is order index for V[[i]] 
      for (i in 1:n.cg) { 
        V[[i]] <- matrix(0,size.cg[i],size.cg[i]) 
        ind[[i]] <- 1:size.cg[i]
      }
      # Voff[i] is where, in coarse order data, first element of V[[i]]
      # relates to ... 
      Voff <- cumsum(c(1,size.cg)) 
      gr.name <- names(c.m) # the names of the innermost groups
      n.g<-length(c.m)   # number of innermost groups
      j0<-rep(1,n.cg) # place holders in V[[i]]'s
      ii <- 1:n
      for (i in 1:n.g) # work through innermost groups
      { # first identify coarse grouping
        Clev <- unique(Cgrps[grps==gr.name[i]])  # level for coarse grouping factor
        if (length(Clev)>1) stop("inner groupings not nested in outer!!")
        k <- (1:n.cg)[Clevel==Clev] # index of coarse group - i.e. update V[[k]] 
        # now need to get c.m into right place within V[[k]]
        j1<-j0[k]+nrow(c.m[[i]])-1
        V[[k]][j0[k]:j1,j0[k]:j1]<-c.m[[i]]
        ind1 <- ii[grps==gr.name[i]] 
        # ind1 is the rows of original data.frame to which c.m[[i]] applies 
        # assuming that data frame order is preserved at the inner grouping
        ind2 <- rCind[ind1] 
        # ind2 contains the rows of the coarse ordering to which c.m[[i]] applies
        ind[[k]][j0[k]:j1] <- ind2 - Voff[k] + 1
        # ind[k] accumulates rows within coarse group k to which V[[k]] applies
        j0[k]<-j1+1  
      }
      for (k in 1:n.cg) { # pasting correlations into right place in each matrix
        V[[k]][ind[[k]],]<-V[[k]];V[[k]][,ind[[k]]]<-V[[k]] 
      }
    }
  } 
  # now form diag(w)%*%V%*%diag(w), depending on class of V
  if (is.list(V)) # it's  a block diagonal structure
  { for (i in 1:n.cg)
    { wi <- w[Voff[i]:(Voff[i]+size.cg[i]-1)] 
      V[[i]] <- as.vector(wi)*t(as.vector(wi)*V[[i]])
    }
  } else
  if (is.matrix(V))
  { V <- as.vector(w)*t(as.vector(w)*V)
  } else # it's a diagonal matrix
  { V <- w^2*V
  }
  # ... covariance matrix according to fitted correlation structure in coarse
  # group order
  
  ## Now work on the random effects ..... 
  X<-list()
  grp.dims<-b$dims$ncol # number of Zt columns for each grouping level (inner levels first)
  # inner levels are first in Zt
  Zt<-model.matrix(b$modelStruct$reStruct,data)  # a sort of proto - Z matrix
  # b$groups and cov (defined below have the inner levels last)
  cov<-as.matrix(b$modelStruct$reStruct) # list of estimated covariance matrices (inner level last)
  i.col<-1
  Z<-matrix(0,n,0) # Z matrix
  if (start.level<=n.levels)
  { for (i in 1:(n.levels-start.level+1)) # work through the r.e. groupings inner to outer
    { # get matrix with columns that are indicator variables for ith set of groups...
      # groups has outer levels first 
      if(length(levels(b$groups[[n.levels-i+1]]))==1) { ## model.matrix needs >1 level 
        X[[1]] <- matrix(rep(1,nrow(b$groups))) } else { 
        X[[1]] <- model.matrix(~b$groups[[n.levels-i+1]]-1,
                  contrasts.arg=c("contr.treatment","contr.treatment")) }
      # Get `model matrix' columns relevant to current grouping level...
      X[[2]] <- Zt[,i.col:(i.col+grp.dims[i]-1),drop=FALSE]
      i.col <- i.col+grp.dims[i]
      # tensor product the X[[1]] and X[[2]] rows...
      Z <- cbind(Z,tensor.prod.model.matrix(X))
    } # so Z assembled from inner to outer levels
    # Now construct overall ranef covariance matrix
    Vr <- matrix(0,ncol(Z),ncol(Z))
    start <- 1
    for (i in 1:(n.levels-start.level+1))
    { k <- n.levels-i+1
      for (j in 1:b$dims$ngrps[i]) 
      { stop <- start+ncol(cov[[k]])-1
        Vr[start:stop,start:stop]<-cov[[k]]
        start <- stop+1
      }
    }
    Vr <- Vr*b$sigma^2
    ## Now re-order Z into coarse group order
    Z <- Z[Cind,]
    ## Now Z %*% Vr %*% t(Z) is block diagonal: if Z' = [Z1':Z2':Z3': ... ]
    ## where Zi contains th rows of Z for the ith level of the coarsest
    ## grouping factor, then the ith block of (Z Vr Z') is (Zi Vr Zi')
    if (n.cg == 1) { 
      if (is.matrix(V)) { 
        V <- V+Z%*%Vr%*%t(Z)
      } else V <- diag(V) + Z%*%Vr%*%t(Z) 
    } else { # V has a block - diagonal structure
      j0<-1
      Vz <- list()
      for (i in 1:n.cg) {
        j1 <- size.cg[i] + j0 -1
        Zi <- Z[j0:j1,,drop=FALSE]
        Vz[[i]] <- Zi %*% Vr %*% t(Zi) 
        j0 <- j1+1
      }
      if (is.list(V)) {
        for (i in 1:n.cg) V[[i]] <- V[[i]]+Vz[[i]] 
      } else { 
        j0 <-1
        for (i in 1:n.cg) {
          kk <- size.cg[i]
          j1 <- kk + j0 -1
          Vz[[i]] <- Vz[[i]] + diag(x=V[j0:j1],nrow=kk,ncol=kk)
          j0 <- j1+1
        }
        V <- Vz
      }
    }
  }
  list(V=V,ind=Cind)
}

extract.lme.cov<-function(b,data,start.level=1)
# function to extract the response data covariance matrix from an lme fitted
# model object b, fitted to the data in data. "inner" == "finest" grouping 
# start.level is the r.e. grouping level at which to start the construction, 
# levels outer to this will not be included in the calculation - this is useful
# for gamm calculations
{ if (!inherits(b,"lme")) stop("object does not appear to be of class lme")
  grps<-nlme::getGroups(b) # labels of the innermost groupings - in data frame order
  n<-length(grps)    # number of data
  if (is.null(b$modelStruct$varStruct)) w<-rep(b$sigma,n) ### 
  else 
  { w<-1/nlme::varWeights(b$modelStruct$varStruct) 
    # w is not in data.frame order - it's in inner grouping level order
    group.name<-names(b$groups) # b$groups[[i]] doesn't always retain factor ordering
    order.txt <- paste("ind<-order(data[[\"",group.name[1],"\"]]",sep="")
    if (length(b$groups)>1) for (i in 2:length(b$groups)) 
    order.txt <- paste(order.txt,",data[[\"",group.name[i],"\"]]",sep="")
    order.txt <- paste(order.txt,")")
    eval(parse(text=order.txt))
    w[ind] <- w # into data frame order
    w<-w*b$sigma
  }
  if (is.null(b$modelStruct$corStruct)) V<-diag(n) #*b$sigma^2
  else
  { c.m<-nlme::corMatrix(b$modelStruct$corStruct) # correlation matrices for each group
    if (!is.list(c.m)) V<-c.m
    else
    { V<-matrix(0,n,n)   # data cor matrix
      gr.name <- names(c.m) # the names of the groups
      n.g<-length(c.m)   # number of innermost groups
      j0<-1
      ind<-ii<-1:n
      for (i in 1:n.g) 
      { j1<-j0+nrow(c.m[[i]])-1
        V[j0:j1,j0:j1]<-c.m[[i]]
        ind[j0:j1]<-ii[grps==gr.name[i]]
        j0<-j1+1  
      }
      V[ind,]<-V;V[,ind]<-V # pasting correlations into right place in overall matrix
      # V<-V*b$sigma^2
    }
  }  
  V <- as.vector(w)*t(as.vector(w)*V) # diag(w)%*%V%*%diag(w)
  # ... covariance matrix according to fitted correlation structure
  X<-list()
  grp.dims<-b$dims$ncol # number of Zt columns for each grouping level (inner levels first)
  # inner levels are first in Zt
  Zt<-model.matrix(b$modelStruct$reStruct,data)  # a sort of proto - Z matrix
  # b$groups and cov (defined below have the inner levels last)
  cov<-as.matrix(b$modelStruct$reStruct) # list of estimated covariance matrices (inner level last)
  i.col<-1
  n.levels<-length(b$groups)
  Z<-matrix(0,n,0) # Z matrix
  if (start.level<=n.levels)
  { for (i in 1:(n.levels-start.level+1)) # work through the r.e. groupings inner to outer
    { # get matrix with columns that are indicator variables for ith set of groups...
      # groups has outer levels first 
      if(length(levels(b$groups[[n.levels-i+1]]))==1) { ## model.matrix needs >1 level 
        X[[1]] <- matrix(rep(1,nrow(b$groups))) } else { 
        X[[1]] <- model.matrix(~b$groups[[n.levels-i+1]]-1,
                  contrasts.arg=c("contr.treatment","contr.treatment")) }
      # Get `model matrix' columns relevant to current grouping level...
      X[[2]] <- Zt[,i.col:(i.col+grp.dims[i]-1),drop=FALSE]
      i.col <- i.col+grp.dims[i]
      # tensor product the X[[1]] and X[[2]] rows...
      Z <- cbind(Z,tensor.prod.model.matrix(X))
    } # so Z assembled from inner to outer levels
    # Now construct overall ranef covariance matrix
    Vr <- matrix(0,ncol(Z),ncol(Z))
    start <- 1
    for (i in 1:(n.levels-start.level+1))
    { k <- n.levels-i+1
      for (j in 1:b$dims$ngrps[i]) 
      { stop <- start+ncol(cov[[k]])-1
        Vr[start:stop,start:stop]<-cov[[k]]
        start <- stop+1
      }
    }
    Vr <- Vr*b$sigma^2
    V <- V+Z%*%Vr%*%t(Z)
  }
  V
}

formXtViX <- function(V,X)
## forms X'V^{-1}X as efficiently as possible given the structure of
## V (diagonal, block-diagonal, full)
{ X <- X[V$ind,,drop=FALSE] # have to re-order X according to V ordering
  if (is.list(V$V)) {     ### block diagonal case
    Z <- X
    j0<-1
    for (i in 1:length(V$V))
    { Cv <- chol(V$V[[i]])
      j1 <- j0+nrow(V$V[[i]])-1
      Z[j0:j1,]<-backsolve(Cv,X[j0:j1,,drop=FALSE],transpose=TRUE)
      j0 <- j1 + 1
    }
    res <- t(Z)%*%Z
  } else if (is.matrix(V$V)) { ### full matrix case
    Cv<-chol(V$V)
    Z<-backsolve(Cv,X,transpose=TRUE)
    res <- t(Z)%*%Z
  } else {                ### diagonal matrix case
    res <- t(X)%*%(X/as.numeric(V$V))
  }
  res
}


new.name <- function(proposed,old.names)
# finds a name based on proposed, that is not in old.names
# if the proposed name is in old.names then ".xx" is added to it 
# where xx is a number chosen to ensure the a unique name
{ prop <- proposed
  k <- 0
  while (sum(old.names==prop))
  { prop<-paste(proposed,".",k,sep="")
    k <- k + 1
  }
  prop
}

gammPQL <- function (fixed, random, family, data, correlation, weights,
    control, niter = 30, verbose = TRUE, ...)

## service routine for `gamm' to do PQL fitting. Based on glmmPQL
## from the MASS library (Venables & Ripley). In particular, for back 
## compatibility the numerical results should be identical with gamm 
## fits by glmmPQL calls. Because `gamm' already does some of the 
## preliminary stuff that glmmPQL does, gammPQL can be simpler. It also 
## deals with the possibility of the original data frame containing 
## variables called `zz' `wts' or `invwt'

{ off <- model.offset(data)
  if (is.null(off)) off <- 0

  wts <- weights
  if (is.null(wts)) wts <- rep(1, nrow(data))
  wts.name <- new.name("wts",names(data)) ## avoid overwriting what's already in `data'
  data[[wts.name]] <- wts 

  fit0 <- NULL ## keep checking tools happy 
  ## initial fit (might be better replaced with `gam' call)
  eval(parse(text=paste("fit0 <- glm(formula = fixed, family = family, data = data,",
                        "weights =",wts.name,",...)")))
  w <- fit0$prior.weights
  eta <- fit0$linear.predictors
    
  zz <- eta + fit0$residuals - off
  wz <- fit0$weights
  fam <- family

  ## find non clashing name for pseudodata and insert in formula
  zz.name <- new.name("zz",names(data))
  eval(parse(text=paste("fixed[[2]] <- quote(",zz.name,")")))
 
  data[[zz.name]] <- zz ## pseudodata to `data' 
  
  ## find non-clashing name fro inverse weights, and make 
  ## varFixed formula using it...
  
  invwt.name <- new.name("invwt",names(data))
  data[[invwt.name]] <- 1/wz
  w.formula <- as.formula(paste("~",invwt.name,sep=""))

  for (i in 1:niter) {
    if (verbose) message("iteration ", i)
    fit<-lme(fixed=fixed,random=random,data=data,correlation=correlation,
             control=control,weights=varFixed(w.formula),method="ML",...)
    etaold <- eta
    eta <- fitted(fit) + off
    if (sum((eta - etaold)^2) < 1e-06 * sum(eta^2)) break
    mu <- fam$linkinv(eta)
    mu.eta.val <- fam$mu.eta(eta)
    ## get pseudodata and insert in `data' 
    data[[zz.name]] <- eta + (fit0$y - mu)/mu.eta.val - off
    wz <- w * mu.eta.val^2/fam$variance(mu)
    data[[invwt.name]] <- 1/wz
  } ## end i in 1:niter
  fit
}





gamm <- function(formula,random=NULL,correlation=NULL,family=gaussian(),data=list(),weights=NULL,
      subset=NULL,na.action,knots=NULL,control=list(niterEM=0,optimMethod="L-BFGS-B"),   ## nlme::lmeControl(niterEM=0,optimMethod="L-BFGS-B"),
      niterPQL=20,verbosePQL=TRUE,method="ML",...)
## NOTE: niterEM modified after changed notLog parameterization - old version
##       needed niterEM=3. 10/8/05.
# Routine to fit a GAMM to some data. Fixed and smooth terms are defined in the formula, but the wiggly 
# parts of the smooth terms are treated as random effects. The onesided formula random defines additional 
# random terms. correlation describes the correlation structure. This routine is basically an interface
# between the bases constructors provided in mgcv and the glmmPQL routine used to estimate the model.
# NOTE: need to fill out the gam object properly
{
  if (!require("nlme")) stop("gamm() requires package nlme to be installed")
  control <- do.call("lmeControl",control) 
  #  if (!require("MASS")) stop("gamm() requires package MASS to be installed")
    # check that random is a named list
    if (!is.null(random))
    { if (is.list(random)) 
      { r.names<-names(random)
        if (is.null(r.names)) stop("random argument must be a *named* list.")
        else if (sum(r.names=="")) stop("all elements of random list must be named")
      }
      else stop("gamm() can only handle random effects defined as named lists")
      random.vars<-c(unlist(lapply(random, function(x) all.vars(formula(x)))),r.names)
    } else random.vars<-NULL
    if (!is.null(correlation))
    { cor.for<-attr(correlation,"formula")
      if (!is.null(cor.for))
      cor.vars<-all.vars(cor.for)
    } else cor.vars<-NULL
    # create model frame.....
    gp<-interpret.gam(formula) # interpret the formula 
    ##cl<-match.call() # call needed in gamm object for update to work
    mf<-match.call(expand.dots=FALSE)
    mf$formula<-gp$fake.formula
    mf$correlation<-mf$random<-mf$family<-mf$control<-mf$scale<-mf$knots<-mf$sp<-mf$weights<-
    mf$min.sp<-mf$H<-mf$gamma<-mf$fit<-mf$niterPQL<-mf$verbosePQL<-mf$G<-mf$method<-mf$...<-NULL
    mf$drop.unused.levels<-TRUE
    mf[[1]]<-as.name("model.frame")
    pmf <- mf
    gmf <- eval(mf, parent.frame()) # the model frame now contains all the data, for the gam part only 
    gam.terms <- attr(gmf,"terms") # terms object for `gam' part of fit -- need this for prediction to work properly

    allvars <- c(cor.vars,random.vars)
    if (length(allvars)) {
      mf$formula<-as.formula(paste(paste(deparse(gp$fake.formula,backtick=TRUE),collapse=""),
                           "+",paste(allvars,collapse="+")))
      mf <- eval(mf, parent.frame()) # the model frame now contains *all* the data
    }
    else mf <- gmf
    rm(gmf)
    if (nrow(mf)<2) stop("Not enough (non-NA) data to do anything meaningful")
    Terms <- attr(mf,"terms")    
  
    ## summarize the *raw* input variables
    ## note can't use get_all_vars here -- buggy with matrices
    vars <- all.vars(gp$fake.formula[-2]) ## drop response here
    inp <- parse(text = paste("list(", paste(vars, collapse = ","),")"))
    dl <- eval(inp, data, parent.frame())
    names(dl) <- vars ## list of all variables needed
    var.summary <- variable.summary(gp$pf,dl,nrow(mf)) ## summarize the input data
    rm(dl) ## save space 

    pmf$formula <- gp$pf
    pmf <- eval(pmf, parent.frame()) # pmf contains all data for parametric part 

    pTerms <- attr(pmf,"terms")

    if (is.character(family)) family<-eval(parse(text=family))
    if (is.function(family)) family <- family()
    if (is.null(family$family)) stop("family not recognized")
  
    # now call gamm.setup 

    G<-gamm.setup(gp,pterms=pTerms,data=mf,knots=knots,parametric.only=FALSE,absorb.cons=TRUE)
    G$var.summary <- var.summary    

    n.sr <- length(G$random) # number of random smooths (i.e. s(...,fx=FALSE,...) terms)

    if (is.null(random)&&n.sr==0) 
    stop("gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect")

    g <- as.factor(rep(1,G$n)) ##as.factor(G$y*0+1) ## needed, whatever codetools says

    offset.name <- attr(mf,"names")[attr(attr(mf,"terms"),"offset")]

    yname <- new.name("y",names(mf))
    eval(parse(text=paste("mf$",yname,"<-G$y",sep="")))
    Xname <- new.name("X",names(mf))
    eval(parse(text=paste("mf$",Xname,"<-G$X",sep="")))
    
    fixed.formula <- paste(yname,"~",Xname,"-1")
    if (length(offset.name)) 
    { fixed.formula <- paste(fixed.formula,"+",offset.name) 
    }
    fixed.formula <- as.formula(fixed.formula)
    
    group.name<-rep("",n.sr)
    r.name <- names(G$random) 
    if (n.sr) for (i in 1:n.sr) # adding the constructed variables to the model frame avoiding name duplication
    { mf[[r.name[i]]] <- G[[r.name[i]]]
      group.name[i] <- new.name(paste("g.",i,sep=""),names(mf))
      eval(parse(text=paste("mf$",group.name[i]," <- g",sep="")))
    }
    ret<-list()
    rand<-G$random;names(rand)<-group.name  
    if (!is.null(random)) # add explicit random effects
    { r.m<-length(random)
      r.names<-c(names(rand),names(random))
      for (i in 1:r.m) rand[[n.sr+i]]<-random[[i]] # this line had G$m not n.sr <1.3-12    
      names(rand)<-r.names
    }
    ## need to modify the correlation structure formula, in order that any
    ## grouping factors for correlation get nested within at least the 
    ## constructed dummy grouping factors.
    if (length(formula(correlation))) # then modify the correlation formula
    { # first get the existing grouping structure ....
      corGroup <- paste(names(rand),collapse="/")
      groupForm<-nlme::getGroupsFormula(correlation)
      if (!is.null(groupForm)) {
        groupFormNames <- all.vars(groupForm)
        exind <- groupFormNames %in% names(rand)
        groupFormNames <- groupFormNames[!exind] ## dumping duplicates 
        if (length(groupFormNames)) corGroup <- 
             paste(corGroup,paste(groupFormNames,collapse="/"),sep="/")
      }
      # now make a new formula for the correlation structure including these groups
      corForm <- as.formula(paste(deparse(nlme::getCovariateFormula(correlation)),"|",corGroup))
      attr(correlation,"formula") <- corForm
    }

    ### Actually do fitting ....
    if (family$family=="gaussian"&&family$link=="identity"&&
    length(offset.name)==0) lme.used <- TRUE else lme.used <- FALSE
    if (lme.used&&!is.null(weights)&&!inherits(weights,"varFunc")) lme.used <- FALSE   

    if (lme.used)
    { ## following construction is a work-around for problem in nlme 3-1.52 
      eval(parse(text=paste("ret$lme<-lme(",deparse(fixed.formula),
          ",random=rand,data=strip.offset(mf),correlation=correlation,",
          "control=control,weights=weights,method=method)"
            ,sep=""    ))) 
      ##ret$lme<-lme(fixed.formula,random=rand,data=mf,correlation=correlation,control=control)
    } else
    { ## Again, construction is a work around for nlme 3-1.52
      if (inherits(weights,"varFunc")) 
      stop("weights must be like glm weights for generalized case")
      if (verbosePQL) cat("\n Maximum number of PQL iterations: ",niterPQL,"\n")
      eval(parse(text=paste("ret$lme<-gammPQL(",deparse(fixed.formula),
          ",random=rand,data=strip.offset(mf),family=family,",
          "correlation=correlation,control=control,",
            "weights=weights,niter=niterPQL,verbose=verbosePQL)",sep=""))) 
     
      ##ret$lme<-glmmPQL(fixed.formula,random=rand,data=mf,family=family,correlation=correlation,
      ##                 control=control,niter=niterPQL,verbose=verbosePQL)
    }

    ### .... fitting finished

    # now fake a gam object 
    
    object<-list(model=mf,formula=formula,smooth=G$smooth,nsdf=G$nsdf,family=family,
                 df.null=nrow(G$X),y=G$y,terms=gam.terms,pterms=pTerms,xlevels=G$xlevels,
                 contrasts=G$contrasts,assign=G$assign,na.action=attr(mf,"na.action"),
                 cmX=G$cmX,var.summary=G$var.summary,scale.estimated=TRUE)

    #######################################################
    ## Transform  parameters back to the original space....
    #######################################################

    bf<-as.numeric(ret$lme$coefficients$fixed)
    br<-as.numeric(unlist(ret$lme$coefficients$random))
    if (G$nsdf) p<-bf[1:G$nsdf] else p<-array(0,0)
    n.pen <- 0 # count up the penalties
    if (G$m>0) for (i in 1:G$m)
    { fx <- G$smooth[[i]]$fixed 
      first<-G$smooth[[i]]$first.f.para;last<-G$smooth[[i]]$last.f.para
      if (first <=last) beta<-bf[first:last] else beta<-array(0,0)
      if (fx) b <- beta 
      else # not fixed so need to undo transform of random effects etc. 
      { dum <- length(G$smooth[[i]]$S) # number of penalties for this term
        n.pen <- n.pen + dum
        if (inherits(G$smooth[[i]],"tensor.smooth")) mult.pen <- TRUE else mult.pen <- FALSE
        b<-br[G$smooth[[i]]$first.r.para:G$smooth[[i]]$last.r.para]     
        if (mult.pen) b <- c(b,beta) # tensor product penalties not reduced to identity
        else b <- c(G$smooth[[i]]$D*b,beta) # single penalty case
        b<-G$smooth[[i]]$U%*%b 
      }
      ## if (is.null(G$smooth[[i]]$C)) nc <- 0 else nc <- nrow(G$smooth[[i]]$C) 
      ## if (nc) b <- qr.qy(G$smooth[[i]]$qrc,c(rep(0,nc),b))
      object$smooth[[i]]$first.para<-length(p)+1
      p<-c(p,b)
      object$smooth[[i]]$last.para<-length(p)
    }
 
   ## cov<-as.matrix(ret$lme$modelStruct$reStruct)
    var.param <- coef(ret$lme$modelStruct$reStruct)
    n.v <- length(var.param) 
    k <- 1
    if (G$m>0) for (i in 1:G$m) # var.param in reverse term order, but forward order within terms!!
    { n.sp <- length(object$smooth[[i]]$S) # number of s.p.s for this term 
      if (n.sp>0) {
        if (inherits(object$smooth[[i]],"tensor.smooth"))
        object$sp[k:(k+n.sp-1)] <- notExp2(var.param[(n.v-n.sp+1):n.v])
        else object$sp[k:(k+n.sp-1)] <- 1/notExp2(var.param[(n.v-n.sp+1):n.v])   ## ^2 <- reinstate to revert
      }
      k <- k + n.sp
      n.v <- n.v - n.sp
    }
   
    object$coefficients<-p
    
    V<-extract.lme.cov2(ret$lme,mf,n.sr+1) # the data covariance matrix, excluding smooths
    XVX <- formXtViX(V,G$Xf)
    S<-matrix(0,ncol(G$Xf),ncol(G$Xf)) # penalty matrix
    first <- G$nsdf+1
    k <- 1
    if (G$m>0) for (i in 1:G$m) # Accumulate the total penalty matrix
    { ## if (is.null(G$smooth[[i]]$C)) nc <- 0 else nc <- nrow(G$smooth[[i]]$C) 
      n.para <- object$smooth[[i]]$last.para - object$smooth[[i]]$first.para + 1 ##  - nc
      last <- first + n.para - 1 
      if (!object$smooth[[i]]$fixed)
      { for (l in 1:length(object$smooth[[i]]$ZSZ))
         { S[first:last,first:last] <- S[first:last,first:last] + 
                  object$smooth[[i]]$ZSZ[[l]]*object$sp[k]
          k <- k+1
        }
      }
      first <- last + 1 
    }
    S<-S/ret$lme$sigma^2 # X'V^{-1}X divided by \sigma^2, so should S be
    
    ## Vb <- chol2inv(chol(XVX+S)) # covariance matrix - in constraint space
    ## replacement, more stable version...
    ev <- eigen(XVX+S,symmetric=TRUE)
    ind <- ev$values != 0
    iv <- ev$values;iv[ind] <- 1/ev$values[ind]
    Vb <- ev$vectors%*%(iv*t(ev$vectors))    

    object$edf<-rowSums(Vb*t(XVX))   
    
    object$sig2 <- ret$lme$sigma^2
    if (lme.used) { object$method <- paste("lme.",method,sep="")} 
    else { object$method <- "PQL"}

    if (!lme.used||method=="ML") Vb<-Vb*length(G$y)/(length(G$y)-G$nsdf)
    object$Vp <- Vb
    ## object$Ve <- Vb%*%Z%*%Vb
    object$Ve <- Vb%*%XVX%*%Vb
    
    object$prior.weights <- weights
    class(object)<-"gam"
    ##object$full.formula <- G$full.formula

    ## Restore original smooth list, if it was split to deal with t2 terms...
    if (!is.null(G$original.smooth)) {
      object$smooth <- G$smooth <- G$original.smooth
    }

    ## If prediction parameterization differs from fit parameterization, transform now...
    ## (important for t2 smooths, where fit constraint is not good for component wise 
    ##  prediction s.e.s)

    if (!is.null(G$P)) {
      object$coefficients <- G$P %*% object$coefficients
      object$Vp <- G$P %*% object$Vp %*% t(G$P) 
      object$Ve <- G$P %*% object$Ve %*% t(G$P) 
    }


#    object$fitted.values <- predict.gam(object,type="response")
    object$linear.predictors <- predict.gam(object,type="link")
    object$fitted.values <- object$family$linkinv(object$linear.predictors)  
 
    object$residuals <- residuals(ret$lme) #as.numeric(G$y) - object$fitted.values

    if (G$nsdf>0) term.names<-colnames(G$X)[1:G$nsdf] else term.names<-array("",0)
    n.smooth<-length(G$smooth) 
    if (n.smooth) {
      for (i in 1:n.smooth)
      { k<-1
        for (j in object$smooth[[i]]$first.para:object$smooth[[i]]$last.para)
        { term.names[j]<-paste(object$smooth[[i]]$label,".",as.character(k),sep="")
          k<-k+1
        }
      }
      names(object$sp) <- names(G$sp)
    }

    names(object$coefficients) <- term.names  # note - won't work on matrices!!
    names(object$edf) <- term.names
    if (is.null(weights))
    object$prior.weights <- object$y*0+1
    else if (inherits(weights,"varFunc")) 
    object$prior.weights <- varWeights.dfo(ret$lme,mf)^2
    else object$prior.weights <- weights 
    
    object$weights<-object$prior.weights   

    if (!is.null(G$Xcentre)) object$Xcentre <- G$Xcentre ## column centering values

    ret$gam<-object
    ret

}


test.gamm <- function(control=nlme::lmeControl(niterEM=3,tolerance=1e-11,msTol=1e-11))
## this function is a response to repeated problems with nlme/R updates breaking
## the pdTens class. It tests fo obvious breakages!
{ test1<-function(x,z,sx=0.3,sz=0.4)
  { x<-x*20
    (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
    0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
  }
  compare <- function(b,b1,edf.tol=.001) 
  { edf.diff <- abs(sum(b$edf)-sum(b1$edf))
    fit.cor <- cor(fitted(b),fitted(b1))
    if (fit.cor<.999) { cat("FAILED: fit.cor = ",fit.cor,"\n");return()}
    if (edf.diff>edf.tol) { cat("FAILED: edf.diff = ",edf.diff,"\n");return()}
    cat("PASSED \n")
  }
  n<-500
  x<-runif(n)/20;z<-runif(n);
  f <- test1(x,z)
  y <- f + rnorm(n)*0.2
  cat("testing covariate scale invariance ... ")  
  b <- gamm(y~te(x,z), control=control )
  x1 <- x*100 
  b1 <- gamm(y~te(x1,z),control=control)
  res <- compare(b$gam,b1$gam)
 
  cat("testing invariance w.r.t. response ... ")
  y1 <- y*100 
  b1 <- gamm(y1~te(x,z),control=control)
  res <- compare(b$gam,b1$gam)
  
  cat("testing equivalence of te(x) and s(x) ... ")
  b2 <- gamm(y~te(x,k=10,bs="cr"),control=control)
  b1 <- gamm(y~s(x,bs="cr",k=10),control=control)
  res <- compare(b2$gam,b1$gam,edf.tol=.1)

  cat("testing equivalence of gam and gamm with same sp ... ")
  b1 <- gam(y~te(x,z),sp=b$gam$sp)
  res <- compare(b$gam,b1)  
  
  if (FALSE) cat(res,x1,y1) ## fool codetools
}


