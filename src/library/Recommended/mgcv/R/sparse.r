## (c) Simon N. Wood 2011
## functions for sparse smoothing.


tri2nei <- function(T) {
## Each row of matrix T gives the indices of the points making up
## one triangle in d dimensions. T has d+1 columns. This routine 
## finds the neighbours of each point in that triangulation.
## indices start at 1.  
  n <- max(T)
  oo <- .C(C_tri2nei,T=as.integer(cbind(T-1,T*0)),as.integer(nrow(T)),
           as.integer(n),as.integer(ncol(T)-1),off=as.integer(rep(0,n)));
  ## ni[1:off[1]] gives neighbours of point 1. 
  ## ni[(off[i-1]+1):off[i]] give neighbours of point i>1
  return(list(ni = oo$T[1:oo$off[n]]+1,off=oo$off))
}

tri.pen <- function(X,T) {
## finds a sparse approximate TPS penalty, based on the points in X, 
## with triangulation T. Rows of X are points. Rows of T index vertices 
## of triangles in X.
  require(Matrix)
  nn <- tri2nei(T) ## get neighbour list from T
  ## now obtain generalized FD penalty...
  n <- nrow(X);d <- ncol(X);
  D <- rep(0,3*(nn$off[n]+n)) ## storage for 
  oo <- .C(C_nei_penalty,as.double(X),as.integer(n),as.integer(d),D=as.double(D),
           ni=as.integer(nn$ni-1),ii=as.integer(nn$ni*0),off=as.integer(nn$off),
           as.integer(2),as.integer(0),kappa=as.double(rep(0,n)));
  
  ## unpack into sparse matrices...
  ni <- oo$off[n]
  ii <- c(1:n,oo$ii[1:ni]+1) ## row index
  jj <- c(1:n,oo$ni[1:ni]+1) ## col index
  
  ni <- length(ii)
  Kx <- sparseMatrix(i=ii,j=jj,x=oo$D[1:ni],dims=c(n,n))
  Kz <- sparseMatrix(i=ii,j=jj,x=oo$D[1:ni+ni],dims=c(n,n))
  Kxz <- sparseMatrix(i=ii,j=jj,x=oo$D[1:ni+2*ni],dims=c(n,n))
  list(Kx=Kx,Kz=Kz,Kxz=Kxz)
}


## Efficient stable full rank cubic spline routines, based on    
## deHoog and Hutchinson, 1987 and Hutchinson and deHoog,
## 1985....

setup.spline <- function(x,w=rep(1,length(x)),lambda=1,tol=1e-9) { 
## setup a cubic smoothing spline given data locations in x. 
## ties will be treated by removing duplicate x's, and averaging corresponding
## y's. Averaging is \sum_i y_i w_i^2 / \sum_i w_i^2, and the weight
## assigned to this average is then w_a^2 = \sum_i w_i^2... 
## spline object has to record duplication information, as well as 
## rotations defining spline.  
   n <- length(x)
   ind <- order(x)
   x <- x[ind] ## sort x
   w <- w[ind]
   U <- V <- rep(0,4*n)
   diagA <- rep(0,n)
   lb <- rep(0,2*n)
   oo <- .C(C_sspl_construct,as.double(lambda),x=as.double(x),w=as.double(w),U=as.double(U),V=as.double(V),
                          diagA=as.double(diagA),lb=as.double(lb),n=as.integer(n),tol=as.double(tol))
   list(trA=sum(oo$diagA), ## trace of influence matrix
        U=oo$U,V=oo$V,     ## spline defining Givens rotations
        lb=oo$lb,          ## final lower band
        x=x,               ## original x sequence, ordered
        ind=ind,           ## x0 <- x; x0[ind] <- x, puts original ordering in x0 
        w=w,               ## original weights
        ns=oo$n,           ## number of unique x values (maximum spline rank)
        tol=tol)           ## tolerance used to judge tied x values
}

apply.spline <- function(spl,y) {
## Use cubic spline object spl, from setup.spline, to smooth data in y.
  if (is.matrix(y)) { 
    m <- ncol(y) 
    y <- y[spl$ind,] ## order as x
  } else {
    m <- 1
    y <- y[spl$ind]
  }
  n <- length(spl$x)
  oo <- .C(C_sspl_mapply,f = as.double(y),x=as.double(spl$x),as.double(spl$w),
            U=as.double(spl$U),as.double(spl$V),n=as.integer(spl$ns),
            nf=as.integer(n),tol=as.double(spl$tol),m=as.integer(m))
  if (is.matrix(y)) {
    y <- matrix(oo$f,n,m)
    y[spl$ind,] <- y ## original order
  } else {
    y[spl$ind] <- oo$f
  }
  y
}



## Routines for sparse thin plate splines...

kd.vis <- function(X,cex=.5) {
## code obtains and visualizes a kd tree for points in rows of X
  if (ncol(X)!=2) stop("only deals with 2D case")
  n <- nrow(X)
  d <- ncol(X)
  ind <- rind <- rep(0,n)
  m <- 2;
  while (m<n) m <- m*2
  nb <- min(m-1,2*n-m/2-1)
  lo <- hi <- rep(0,nb*d)
  oo <- .C(C_Rkdtree,as.double(X),as.integer(n),as.integer(d),lo = as.double(lo),hi =  as.double(hi),
           ind = as.integer(ind), rind = as.integer(rind));
  lo <- matrix(oo$lo,nb,d)
  hi <- matrix(oo$hi,nb,d)
  plot(X[,1],X[,2],pch=19,cex=cex,col=2)
  for (i in 1:nb) {
    rect(lo[i,1],lo[i,2],hi[i,1],hi[i,2])
  }
  #points(X[,1],X[,2],pch=19,cex=cex,col=2)

}

nearest <- function(k,X,gt.zero = FALSE,get.a=FALSE,balanced=FALSE,cut.off=5) {
## The rows of X contain coordinates of points.
## For each point, this routine finds its k nearest 
## neighbours, returning a list of 2, n by k matrices:
## ni - ith row indexes the rows of X containing 
##      the k nearest neighbours of X[i,]
## dist - ith row is the distances to the k nearest 
##        neighbours.
## a - area associated with each point, if get.a is TRUE 
## ties are broken arbitrarily.
## if balanced = TRUE, then some nearest neighbours are sacrificed 
## for neighbours chosen to be on either side of the box in each 
## direction in this case k>2*ncol(X). These neighbours are only used
## if closer than cut.off*max(k nearest distances).
## gt.zero indicates that neighbours must have distances greater
## than zero...
  require(mgcv)
  if (balanced) gt.zero <- TRUE
  if (gt.zero) {
    Xu <- uniquecombs(X);ind <- attr(Xu,"index") ## Xu[ind,] == X
  } else { Xu <- X; ind <- 1:nrow(X)}
  if (k>nrow(Xu)) stop("not enough unique values to find k nearest")
  nobs <- length(ind)
  n <- nrow(Xu)
  d <- ncol(Xu)
  dist <- matrix(0,n,k)
  if (get.a) a <- 1:n else a=1
  if (balanced) {
    kk <- k - 2*ncol(X)
    if (kk<0) stop("k too small for balanced neighbours")
    oo <- .C(C_kba_nn,Xu=as.double(Xu),dist=as.double(dist),a=as.double(a),ni=as.integer(dist),
                    n=as.integer(n),d=as.integer(d),k=as.integer(kk),get.a=as.integer(get.a),
                    as.double(cut.off))
  } else {
    oo <- .C(C_k_nn,Xu=as.double(Xu),dist=as.double(dist),a=as.double(a),ni=as.integer(dist),
                    n=as.integer(n),d=as.integer(d),k=as.integer(k),get.a=as.integer(get.a))
  }
  dist <- matrix(oo$dist,n,k)[ind,]
  rind <- 1:nobs
  rind[ind] <- 1:nobs
  ni <- matrix(rind[oo$ni+1],n,k)[ind,]
  if (get.a) a=oo$a[ind] else a <- NULL
  list(ni=ni,dist=dist,a=a)
}


sparse.pen <- function(X,area.weight=TRUE) {
## Function to generate a sparse approximate TPS penalty matrix
## NOTE: duplicates not yet handled!!
  require(Matrix)
  n <- nrow(X)
  d <- ncol(X)
  if (d!=2) stop("only 2D case available so far")
  m <- 3
  D <- matrix(0,n,6*m)
  k <- 5
  ni <- matrix(0,n,k)
  kappa <- rep(0,n) ## condition numbers
  ## Get the sqrt penalty matrix entries...

  oo <- .C(C_sparse_penalty,as.double(X),as.integer(n),as.integer(d),D=as.double(D),
           ni=as.integer(ni),as.integer(k),as.integer(m),as.integer(area.weight),kappa=
           as.double(kappa))

  ## Now make put the entries into sparse matrices...

  ## First sort out indexing...
  ##ni <- cbind(1:n,matrix(oo$ni,n,k)) ## neighbour indices, including self
  ii <- rep(1:n,k+1) ## row index
  jj <- c(1:n,oo$ni+1) ## col index
  
  ni <- length(ii)
  Kx <- sparseMatrix(i=ii,j=jj,x=oo$D[1:ni],dims=c(n,n))
  Kz <- sparseMatrix(i=ii,j=jj,x=oo$D[1:ni+ni],dims=c(n,n))
  Kxz <- sparseMatrix(i=ii,j=jj,x=oo$D[1:ni+2*ni],dims=c(n,n))
  list(Kx=Kx,Kz=Kz,Kxz=Kxz)
}

tieMatrix <- function(x) {
## takes matrix x, and produces sparse matrix P that maps list of unique 
## rows to full set. Matrix of unique rows is returned in xu.
## If a smoothing penalty matrix, S, is set up based on rows of xu,
## then P%*%solve(t(P)%*%P + S,t(P)) is hat matrix. 
  x <- as.matrix(x)
  n <- nrow(x)
  xu <- uniquecombs(x)
  if (nrow(xu)==nrow(x)) return(NULL)
  ind <- attr(xu,"index")
  x <- as.matrix(x)
  n <- nrow(x)
  P <- sparseMatrix(i=1:n,j=ind,x=rep(1,n),dims=c(n,nrow(xu)))
  return(list(P=P,xu=xu))
}


## sparse smooths must be initialized with...
## 1. a set of variable names, a blocking factor and a type.  



#########################################################
# routines for full rank cubic spline smoothers, based on
# deHoog and Hutchinson, 1987.
#########################################################


spasm.construct.cus <- function(object,data) {
## entry object inherits from "cus" & contains:
## * terms, the name of the argument of the smooth
## * block, the name of a blocking factor. Can be NULL.
## return object also has...
## * nobs - number of observations in total
## * nblock - number of blocks.
## * ind, list where ind[[i]] gives rows to which block i applies.
## * spl, and empty list which will contain intialised cubic 
##   spline smoothers for each block, once a smoothing parameter 
##   has been supplied...
  dat <- list()
  d <- length(object$terms)
  if (d != 1) stop("cubic spline only deals with 1D data") 
  object$x <- get.var(object$term[1],data)
  object$nobs <- length(object$x)
  ind <- list()
  n <- length(object$x)
  ## if there is a blocking factor then set up indexes 
  ## indicating which data go with which block...
  if (!is.null(object$block)) {
    block <- as.factor(get.var(object$block,data))
    nb <- length(levels(block))
    edf1 <- 0
    for (i in 1:nb) {
      ind[[i]] <- (1:n)[block==levels(block)[i]]
      edf1 <- edf1 + length(unique(object$x[ind[[i]]])) ## max edf for this block 
    }
  } else { ## all one block
    nb <- 1
    ind[[1]] <- 1:n
    edf1 <- length(unique(object$x))
  }
  object$nblock <- nb
  object$ind <- ind
 
  ## so ind[[i]] indexes the elements operated on by the ith smoother.
  object$spl <- list()
  object$edf0 <- 2*nb;object$edf1 <- edf1
  class(object) <- "cus"
  object
}

spasm.sp.cus <- function(object,sp,w=rep(1,object$nobs),get.trH=FALSE,block=0,centre=FALSE) { 
## Set up full cubic spline smooth, given new smoothing parameter and weights.
## In particular, construct the Givens rotations defining the 
## smooth and compute the trace of the influence matrix. 
## If block is non-zero, then it specifies which block to set up, otherwise
## all are set up. In either case w is assumed to be for the whole smoother,
## although only the values for the specified block(s) are used.
## If centre == TRUE then the spline is set up for centred smoothing, i.e.
## the results sum to zero. 
## Note: w propto 1/std.dev(response)
  if (is.null(object$spl)) stop("object not fully initialized")
  trH <-  0
  if (block==0) block <- 1:object$nblock
  for (i in block) {
    n <- length(object$ind[[i]])
    object$spl[[i]] <- setup.spline(object$x[object$ind[[i]]],w=w[object$ind[[i]]],lambda=sp)
    trH <- trH + object$spl[[i]]$trA
  }
  object$sp=sp
  if (get.trH) { 
    if (centre) { ## require correction for DoF lost by centring...
       for (i in block) {
         one <- rep(1,length(object$ind[[i]]))
         object$centre <- FALSE
         trH <- trH - mean(spasm.smooth(object,one,block=i))
       }
    }
    object$trH <- trH
  }
  object$centre <- centre
  object
}

spasm.smooth.cus <- function(object,X,residual=FALSE,block=0) {
## apply smooth, or its residual operation to X.
## if block == 0 then apply whole thing, otherwise X must have the correct 
## number of rows for the smooth block.
  if (block>0) { 
    ## n <- length(object$ind[[block]])
    if (object$centre) {
      X0 <- apply.spline(object$spl[[block]],X)
      if (is.matrix(X0)) {
        x0 <- colMeans(X0)
        X0 <- sweep(X0,2,x0)
      } else X0 <- X0 - mean(X0)
      if (residual) X <- X - X0 else X <- X0
    } else {
      if (residual) X <- X - apply.spline(object$spl[[block]],X)
      else X <-  apply.spline(object$spl[[block]],X)
    }
  } else for (i in 1:object$nblock) { ## work through all blocks 
   ind <- object$ind[[i]]
   if (is.matrix(X)) {
      X0 <- apply.spline(object$spl[[i]],X[ind,])
      if (object$centre) X0 <- sweep(X0,2,colMeans(X0))
      if (residual) X[ind,] <- X[ind,] - X0
      else X[ind,] <-  X0
    } else {
      X0 <- apply.spline(object$spl[[i]],X[ind])
      if (object$centre) X0 <- X0 - mean(X0)
      if (residual) X[ind] <- X[ind] - X0
      else X[ind] <- X0 
    }    
  }
  X
} 

#########################################################
## The default sparse smooth class, which does nothing...
#########################################################

spasm.construct.default <- function(object,data) {
## This smooth simply returns 0, under all circumstances.
## object might contain....
## * block, the name of a blocking factor. Can be NULL.
## return object also has...
## * nblock - number of blocks.
## * ind, list where ind[[i]] gives rows to which block i applies.
  n <- nrow(data)
  if (!is.null(object$block)) {
    block <- as.factor(get.var(object$block,data))
    nb <- length(levels(block))
    for (i in 1:nb) { 
      ind[[i]] <- (1:n)[block==levels(block)[i]]
    }
  } else { ## all one block
    nb <- 1
    ind[[1]] <- 1:n
  }
  object$nblock <- nb
  object$ind <- ind
 
  ## so ind[[i]] indexes the elements operated on by the ith smoother.
  class(object) <- "default"
  object
}

spasm.sp.default <- function(object,sp,get.trH=FALSE) { 
## Set up default null smoother. i.e. set trH=0,
  trH <-  0
  if (get.trH) object$trH <- trH
  object$ldetH <- NA
  object
}

spasm.smooth.default <- function(object,X,residual=FALSE,block=0) {
## apply smooth, or its residual operation to X.
## if block == 0 then apply whole thing, otherwise X must have the correct 
## number of rows for the smooth block.
  if (residual) return(X) else return(X*0)
  X
} 





## generics for sparse smooth classes...

spasm.construct <- function(object,data) UseMethod("spasm.construct")
spasm.sp <- function(object,sp,w=rep(1,object$nobs),get.trH=TRUE,block=0,centre=FALSE) UseMethod("spasm.sp")
## Note that w is assumed proportional to 1/std.dev(response) 

spasm.smooth <- function(object,X,residual=FALSE,block=0) UseMethod("spasm.smooth")

spasm.range <- function(object,upper.prop=.5,centre=TRUE) {
## get reasonable smoothing parameter range for sparse smooth in object
  sp <- 1
  edf <- spasm.sp(object,sp,get.trH=TRUE,centre=centre)$trH
  while (edf < object$edf0*1.01) { 
    sp <- sp /100
    edf <- spasm.sp(object,sp,get.trH=TRUE,centre=centre)$trH
  }
  sp1 <- sp ## store smallest known good
  while (edf > object$edf0*1.01) { 
    sp <- sp * 100
    edf <- spasm.sp(object,sp,get.trH=TRUE,centre=centre)$trH
  }
  sp0 <- sp
  while (edf < object$edf1*upper.prop) { 
    sp1 <- sp1 / 100
    edf <- spasm.sp(object,sp1,get.trH=TRUE,centre=centre)$trH
  }

  while (edf > object$edf1*upper.prop) { 
    sp1 <- sp1 * 4
    edf <- spasm.sp(object,sp1,get.trH=TRUE,centre=centre)$trH
  }
  c(sp1,sp0) ## small, large
}


